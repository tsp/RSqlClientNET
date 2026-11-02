# SQL Server DBI read/write benchmark
# Separate R script; does not modify RSqlClientNET or other drivers.
# Dependencies: DBI; optional bit64 and blob for the corresponding cases.

.bench_types <- c(
  "integer", "numeric", "logical", "character", "date", "datetime",
  "mixed", "bigint", "binary", "binary_unique", "decimal", "time", "guid", "unicode",
  "text_long", "datetimeoffset", "real", "smallint", "tinyint",
  "money", "varchar", "xml", "wide_mixed"
)

.bench_scalar_int <- function(x, label, minimum = 1) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < minimum || x > .Machine$integer.max)
    stop(label, " must be a whole number >= ", minimum, call. = FALSE)
  as.integer(x)
}

.bench_text <- function(n, length_chars, unicode = FALSE) {
  # Repeated pool of distinct values reduces pre-test generation time/memory.
  pool_n <- min(n, 1024L)
  unit <- if (unicode) "æøå漢字🙂AZ09" else "abcdefXYZ09"
  ids <- sprintf("%06d", seq_len(pool_n))
  strings <- vapply(ids, function(id) {
    base <- paste0(id, "_", paste(rep(unit, ceiling(length_chars /
                                               nchar(unit, type = "chars")) + 2L),
                                    collapse = ""))
    substr(base, 1L, length_chars)
  }, character(1L), USE.NAMES = FALSE)
  strings[((seq_len(n) - 1L) %% pool_n) + 1L]
}

.bench_make_data <- function(sc, seed = 17L) {
  n <- sc$n_rows
  i <- seq_len(n)
  day <- as.Date("2024-01-01") + (i %% 365L)
  moment <- as.POSIXct("2024-01-01 00:00:00", tz = "UTC") +
    (i %% 86400L) + 0.123
  typ <- sc$type
  dat <- switch(typ,
    integer = data.frame(value = as.integer(i)),
    numeric = data.frame(value = as.numeric(i) / 10),
    logical = data.frame(value = (i %% 2L == 0L)),
    character = data.frame(value = .bench_text(n, sc$text_length)),
    date = data.frame(value = day),
    datetime = data.frame(value = moment),
    bigint = {
      if (!requireNamespace("bit64", quietly = TRUE))
        stop("Install package 'bit64' to benchmark BIGINT")
      data.frame(value = bit64::as.integer64("10000000000") +
                   bit64::as.integer64(i))
    },
    binary = {
      if (!requireNamespace("blob", quietly = TRUE))
        stop("Install package 'blob' to benchmark binary data")
      b <- as.raw((seq_len(sc$binary_bytes) - 1L) %% 256L)
      data.frame(value = blob::new_blob(rep(list(b), n)))
    },
    binary_unique = {
      # A second VARBINARY test varies the bytes on every row to avoid
      # measuring only the repeated-blob fast path. Generate outside timing.
      if (!requireNamespace("blob", quietly = TRUE))
        stop("Install package 'blob' to benchmark binary data")
      width <- sc$binary_bytes
      base <- as.raw((seq_len(width) - 1L) %% 256L)
      values <- lapply(seq_len(n), function(id) {
        out <- base
        x <- as.double(id) - 1
        for (j in seq_len(min(width, 8L))) {
          out[j] <- as.raw(as.integer(x %% 256))
          x <- floor(x / 256)
        }
        out
      })
      data.frame(value = blob::new_blob(values))
    },
    decimal = data.frame(value = as.numeric(i) / 10000 + 0.125),
    time = data.frame(value = as.difftime((i %% 86400L) + 0.125,
                                         units = "secs")),
    guid = data.frame(value = sprintf(
      "00000000-0000-4000-8000-%012x", i)),
    unicode = data.frame(value = .bench_text(n, sc$text_length, unicode = TRUE)),
    text_long = data.frame(value = .bench_text(n, sc$text_length, unicode = TRUE)),
    # POSIXct represents instants consistently across DBI drivers. SQL
    # DATETIMEOFFSET is populated with the UTC offset (not an arbitrary
    # +02:00 character input, which ODBC drivers may reject when binding).
    datetimeoffset = data.frame(value = moment),
    real = data.frame(value = as.numeric(i) / 10),
    smallint = data.frame(value = as.integer(i %% 32768L) - 16384L),
    tinyint = data.frame(value = as.integer(i %% 256L)),
    money = data.frame(value = as.numeric(i) / 100 + 0.125),
    varchar = data.frame(value = .bench_text(n, sc$text_length)),
    xml = data.frame(value = sprintf(
      '<root id="%d"><item>tekst_%d</item></root>', i %% 10000L, i %% 10000L)),
    mixed = data.frame(
      id = as.integer(i), number = as.numeric(i) / 10,
      flag = (i %% 2L == 0L), text = .bench_text(n, 32L),
      day = day, timestamp = moment
    ),
    wide_mixed = {
      v <- list(
        int = as.integer(i),
        float = as.numeric(i) / 10,
        bit = (i %% 2L == 0L),
        text = .bench_text(n, 32L),
        date = day,
        datetime = moment,
        decimal = as.numeric(i) / 10000 + 0.125,
        guid = sprintf("00000000-0000-4000-8000-%012x", i)
      )
      cols <- rep(v, length.out = sc$wide_columns)
      names(cols) <- sprintf("c%03d_%s", seq_along(cols),
                              rep(names(v), length.out = length(cols)))
      as.data.frame(cols, check.names = FALSE, stringsAsFactors = FALSE)
    },
    stop("Unknown type: ", typ)
  )

  # The same deterministic row positions are NULL across all columns.
  # NULL is inserted OUTSIDE the timed sections.
  frac <- sc$null_fraction
  if (frac > 0) {
    positions <- which(((as.double(i) * 4799 +
                         as.double(seed) * 101) %% 10000) <
                         round(frac * 10000))
    if (length(positions)) {
      for (j in seq_along(dat)) {
        if (inherits(dat[[j]], "blob")) {
          vals <- unclass(dat[[j]])
          vals[positions] <- rep(list(NULL), length(positions))
          dat[[j]] <- blob::new_blob(vals)
        } else {
          dat[[j]][positions] <- NA
        }
      }
    }
  }
  dat
}

.bench_schema <- function(sc) {
  text_sql <- function(length_chars, unicode = TRUE, force_max = FALSE) {
    prefix <- if (unicode) "NVARCHAR" else "VARCHAR"
    limit <- if (unicode) 4000L else 8000L
    if (force_max || length_chars > limit) paste0(prefix, "(MAX)")
    else sprintf("%s(%d)", prefix, max(1L, length_chars))
  }
  switch(sc$type,
    integer = c(value = "INT"), numeric = c(value = "FLOAT"),
    logical = c(value = "BIT"),
    character = c(value = text_sql(sc$text_length)),
    date = c(value = "DATE"), datetime = c(value = "DATETIME2(3)"),
    bigint = c(value = "BIGINT"),
    binary = c(value = if (sc$binary_bytes <= 8000L)
      sprintf("VARBINARY(%d)", sc$binary_bytes) else "VARBINARY(MAX)"),
    binary_unique = c(value = if (sc$binary_bytes <= 8000L)
      sprintf("VARBINARY(%d)", sc$binary_bytes) else "VARBINARY(MAX)"),
    decimal = c(value = "DECIMAL(18,4)"),
    time = c(value = "TIME(7)"),
    guid = c(value = "UNIQUEIDENTIFIER"),
    # NVARCHAR(n) counts UTF-16 code units, NOT Unicode code points:
    # the emoji in the test alphabet occupies two UTF-16 code units.
    # Reserve 2*n code units so legitimate test strings never truncate.
    unicode = c(value = text_sql(2L * sc$text_length)),
    text_long = c(value = text_sql(2L * sc$text_length, force_max = TRUE)),
    datetimeoffset = c(value = "DATETIMEOFFSET(7)"),
    real = c(value = "REAL"), smallint = c(value = "SMALLINT"),
    tinyint = c(value = "TINYINT"), money = c(value = "MONEY"),
    varchar = c(value = text_sql(sc$text_length, unicode = FALSE)),
    xml = c(value = "XML"),
    mixed = c(
      id = "INT", number = "FLOAT", flag = "BIT",
      text = "NVARCHAR(32)", day = "DATE", timestamp = "DATETIME2(3)"
    ),
    wide_mixed = {
      template <- c("INT", "FLOAT", "BIT", "NVARCHAR(32)",
                    "DATE", "DATETIME2(3)", "DECIMAL(18,4)",
                    "UNIQUEIDENTIFIER")
      keys <- c("int", "float", "bit", "text", "date", "datetime",
                "decimal", "guid")
      values <- rep(template, length.out = sc$wide_columns)
      names(values) <- sprintf("c%03d_%s", seq_along(values),
                               rep(keys, length.out = length(values)))
      values
    },
    stop("Unknown type: ", sc$type)
  )
}

.bench_row_bytes <- function(type, text_length = NA_integer_,
                             binary_bytes = NA_integer_, wide_columns = NA_integer_) {
  switch(type,
    integer = 4, numeric = 8, logical = 1, character = 2 * text_length,
    date = 3, datetime = 7, mixed = 4 + 8 + 1 + 64 + 3 + 7,
    bigint = 8, binary = binary_bytes, binary_unique = binary_bytes, decimal = 9, time = 5,
    guid = 16, unicode = 4 * text_length,
    text_long = 4 * text_length, datetimeoffset = 10,
    real = 4, smallint = 2, tinyint = 1, money = 8,
    varchar = text_length, xml = 60,
    wide_mixed = sum(rep(c(4, 8, 1, 64, 3, 7, 9, 16),
                          length.out = wide_columns))
  )
}

# Build a plan without allocating any test payloads. n_write and n_read are
# independent; n remains a backward-compatible fallback for both.
benchmark_plan <- function(
    n = 1000000L,
    types = c("integer", "numeric", "logical", "character",
              "date", "datetime", "mixed"),
    text_lengths = 32L,
    null_fractions = 0.01,
    binary_sizes = 16L,
    wide_columns = 25L,
    max_case_payload_mb = 512,
    max_wide_rows = 100000L,
    n_write = NULL,
    n_read = NULL) {

  check_sizes <- function(x, label) {
    if (!length(x) || !is.numeric(x) || anyNA(x) ||
        any(!is.finite(x) | x < 1 | x != floor(x) |
            x > .Machine$integer.max))
      stop(label, " must contain positive integer row counts", call. = FALSE)
    as.integer(x)
  }
  nw <- check_sizes(if (is.null(n_write)) n else n_write, "n_write")
  nr <- check_sizes(if (is.null(n_read)) n else n_read, "n_read")
  count <- max(length(nw), length(nr))
  if (!(length(nw) %in% c(1L, count) && length(nr) %in% c(1L, count)))
    stop("n_write and n_read must have matching lengths or length one", call. = FALSE)
  nw <- rep_len(nw, count)
  nr <- rep_len(nr, count)

  if (!length(types) || anyNA(types) || any(!types %in% .bench_types))
    stop("Unknown or empty types; available: ", paste(.bench_types, collapse = ", "))
  stopifnot(is.numeric(null_fractions), length(null_fractions) > 0L,
            all(is.finite(null_fractions) & null_fractions >= 0 & null_fractions < 1))
  stopifnot(is.numeric(text_lengths), length(text_lengths) > 0L,
            all(is.finite(text_lengths) & text_lengths >= 1 &
                  text_lengths == floor(text_lengths)))
  stopifnot(is.numeric(binary_sizes), length(binary_sizes) > 0L,
            all(is.finite(binary_sizes) & binary_sizes >= 1 &
                  binary_sizes == floor(binary_sizes)))
  stopifnot(is.numeric(wide_columns), length(wide_columns) > 0L,
            all(is.finite(wide_columns) & wide_columns >= 2 &
                  wide_columns == floor(wide_columns)))
  stopifnot(length(max_case_payload_mb) == 1L,
            is.finite(max_case_payload_mb), max_case_payload_mb > 0)
  max_wide_rows <- .bench_scalar_int(max_wide_rows, "max_wide_rows")

  rows <- list()
  k <- 0L
  for (size_idx in seq_len(count)) {
    for (type in unique(types)) {
      variants <- if (type %in% c("character", "unicode", "text_long", "varchar")) {
        lapply(unique(as.integer(text_lengths)), function(v)
          list(t = v, b = NA_integer_, w = NA_integer_))
      } else if (type %in% c("binary", "binary_unique")) {
        lapply(unique(as.integer(binary_sizes)), function(v)
          list(t = NA_integer_, b = v, w = NA_integer_))
      } else if (type == "wide_mixed") {
        lapply(unique(as.integer(wide_columns)), function(v)
          list(t = NA_integer_, b = NA_integer_, w = v))
      } else {
        list(list(t = NA_integer_, b = NA_integer_, w = NA_integer_))
      }
      for (v in variants) {
        bytes <- .bench_row_bytes(type, v$t, v$b, v$w)
        cap <- max(1L, floor(max_case_payload_mb * 1024^2 / bytes))
        if (type == "wide_mixed") cap <- min(cap, max_wide_rows)
        actual_write <- as.integer(min(nw[size_idx], cap))
        actual_read <- as.integer(min(nr[size_idx], cap))
        for (frac in unique(null_fractions)) {
          k <- k + 1L
          rows[[k]] <- data.frame(
            scenario = sprintf("s%04d_%s", k, type),
            type = type,
            n_requested = max(nw[size_idx], nr[size_idx]),
            n_rows = max(actual_write, actual_read),
            n_write_requested = nw[size_idx], n_write_rows = actual_write,
            n_read_requested = nr[size_idx], n_read_rows = actual_read,
            null_fraction = frac,
            text_length = v$t, binary_bytes = v$b, wide_columns = v$w,
            row_bytes_est = bytes,
            sql_payload_est_mb = max(actual_write, actual_read) * bytes *
              (1 - frac) / 1024^2,
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  do.call(rbind, rows)
}

.bench_median <- function(x) {
  x <- x[is.finite(x)]
  if (length(x)) stats::median(x) else NA_real_
}

# One row per scenario/driver/table_type in the summary; write/read results are separate
# so read-only and write-only runs work, and failed writes cannot masquerade
# as successful reads.
benchmark_summary <- function(details) {
  if (is.null(details) || !nrow(details)) return(data.frame())
  spl <- split(seq_len(nrow(details)),
               paste(details$scenario, details$driver, details$table_type, sep = "|"))
  out <- lapply(spl, function(ix) {
    d <- details[ix, , drop = FALSE]
    w <- d[d$operation == "write", , drop = FALSE]
    r <- d[d$operation == "read", , drop = FALSE]
    a <- d[d$operation == "append", , drop = FALSE]
    t <- d[d$operation == "append_table_lock", , drop = FALSE]
    ag <- a[a$status == "ok" & is.finite(a$write_s) & a$write_s > 0, , drop = FALSE]
    tg <- t[t$status == "ok" & is.finite(t$write_s) & t$write_s > 0, , drop = FALSE]
    wg <- w[w$status == "ok" & is.finite(w$write_s) & w$write_s > 0, , drop = FALSE]
    rg <- r[r$status == "ok" & is.finite(r$read_s) & r$read_s > 0, , drop = FALSE]
    data.frame(
      scenario = d$scenario[1L], driver = d$driver[1L],
      table_type = d$table_type[1L], type = d$type[1L],
      n_write_requested = d$n_write_requested[1L],
      n_write_rows = d$n_write_rows[1L],
      n_read_requested = d$n_read_requested[1L],
      n_read_rows = d$n_read_rows[1L],
      null_fraction = d$null_fraction[1L],
      text_length = d$text_length[1L], binary_bytes = d$binary_bytes[1L],
      wide_columns = d$wide_columns[1L],
      # A successful operation may time as 0 on coarse Windows clocks.
      # Count success by status, but exclude 0 timings from speed summaries.
      write_runs = nrow(w), write_ok = sum(w$status == "ok"),
      read_runs = nrow(r), read_ok = sum(r$status == "ok"),
      failed = sum(d$status != "ok"),
      append_runs = nrow(a), append_ok = sum(a$status == "ok"),
      append_table_lock_runs = nrow(t), append_table_lock_ok = sum(t$status == "ok"),
      median_append_s = .bench_median(ag$write_s),
      median_append_table_lock_s = .bench_median(tg$write_s),
      speedup_table_lock_vs_append = if (length(ag$write_s) && length(tg$write_s) &&
                                    is.finite(.bench_median(tg$write_s)) &&
                                    .bench_median(tg$write_s) > 0)
                                    .bench_median(ag$write_s) / .bench_median(tg$write_s)
                                    else NA_real_,
      median_write_s = .bench_median(wg$write_s),
      median_read_s = .bench_median(rg$read_s),
      median_write_rows_s = .bench_median(wg$write_rows_s),
      median_read_rows_s = .bench_median(rg$read_rows_s),
      median_write_mb_s_est = .bench_median(wg$write_mb_s_est),
      median_read_mb_s_est = .bench_median(rg$read_mb_s_est),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out)
  rownames(out) <- NULL
  out[order(out$scenario, out$table_type, out$driver), , drop = FALSE]
}

.bench_append_csv <- function(row, file) {
  exists <- file.exists(file)
  utils::write.table(row, file = file, sep = ",", row.names = FALSE,
                     col.names = !exists, append = exists, quote = TRUE,
                     na = "", fileEncoding = "UTF-8")
}

benchmark_dbi <- function(
    con,
    repetitions = 5L,
    n = 1000000L,
    types = c("integer", "numeric", "logical", "character",
              "date", "datetime", "mixed"),
    text_lengths = 32L,
    null_fractions = 0.01,
    binary_sizes = 16L,
    wide_columns = 25L,
    max_case_payload_mb = 512,
    max_wide_rows = 100000L,
    run_dir = NULL,
    resume = FALSE,
    max_hours = Inf,
    fail_fast = FALSE,
    verbose = TRUE,
    seed = 17L,
    repetitions_write = NULL,
    repetitions_read = NULL,
    n_write = NULL,
    n_read = NULL,
    reconnect = NULL,
    append_modes = character(),
    table_types = "temporary") {

  if (!requireNamespace("DBI", quietly = TRUE)) stop("Install package 'DBI'")
  if (!is.list(con) || !length(con))
    stop("con must be a nonempty named list of DBI connections")
  drivers <- names(con)
  if (is.null(drivers) || anyNA(drivers) || any(!nzchar(drivers)) ||
      anyDuplicated(drivers))
    stop("con must have unique, nonempty names")
  if (!all(vapply(con, function(cx) methods::is(cx, "DBIConnection"), logical(1L))))
    stop("Each con element must be a DBI connection")
  # Target types are independent runs of identical, deterministically generated
  # data. 'permanent' creates normal tables in the connection's default schema
  # (without #) and drops them after measuring; 'temporary' uses local #tables.
  if (!is.character(table_types) || !length(table_types) ||
      anyNA(table_types) || anyDuplicated(table_types) ||
      any(!table_types %in% c("temporary", "permanent")))
    stop("table_types must be 'temporary', 'permanent', or both", call. = FALSE)
  # Extra writes use a pre-created empty table (excluded from timing). The
  # original create-and-write operation and the read path stay unchanged.
  if (!is.character(append_modes) || anyNA(append_modes) ||
      any(!append_modes %in% c("append", "append_table_lock")) ||
      anyDuplicated(append_modes))
    stop("append_modes must contain only 'append' and/or 'append_table_lock'", call. = FALSE)
  if ("append_table_lock" %in% append_modes && !"RSqlClientNET" %in% drivers)
    stop("append_table_lock requires a connection named RSqlClientNET", call. = FALSE)
  if (!is.null(reconnect) && (!is.list(reconnect) ||
      any(!vapply(reconnect, is.function, logical(1L))) ||
      any(!names(reconnect) %in% drivers)))
    stop("reconnect must be a named list of driver-specific connection functions")

  repetitions <- .bench_scalar_int(repetitions, "repetitions", minimum = 0)
  repetitions_write <- .bench_scalar_int(
    if (is.null(repetitions_write)) repetitions else repetitions_write,
    "repetitions_write", minimum = 0)
  repetitions_read <- .bench_scalar_int(
    if (is.null(repetitions_read)) repetitions else repetitions_read,
    "repetitions_read", minimum = 0)
  if (!repetitions_write && !repetitions_read)
    stop("Both repetitions_write and repetitions_read are zero")
  seed <- .bench_scalar_int(seed, "seed", minimum = 0)
  if (length(max_hours) != 1L || is.na(max_hours) || max_hours <= 0)
    stop("max_hours must be positive or Inf")

  plan <- benchmark_plan(n, types, text_lengths, null_fractions,
                         binary_sizes, wide_columns,
                         max_case_payload_mb, max_wide_rows,
                         n_write = n_write, n_read = n_read)
  manifest <- list(plan = plan, drivers = drivers,
                   repetitions_write = repetitions_write,
                   repetitions_read = repetitions_read, seed = seed,
                   table_types = table_types)
  # Preserve manifest identity for existing runs when extra modes are off.
  if (length(append_modes)) manifest$append_modes <- append_modes
  csv <- NULL
  if (!is.null(run_dir)) {
    if (length(run_dir) != 1L || !nzchar(run_dir)) stop("Invalid run_dir")
    dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)
    cfg_file <- file.path(run_dir, "plan.rds")
    csv <- file.path(run_dir, "details.csv")
    if (isTRUE(resume)) {
      if (!file.exists(cfg_file)) stop("Cannot resume: plan.rds is missing")
      if (!identical(readRDS(cfg_file), manifest))
        stop("Existing run_dir has different benchmark settings")
      if (file.exists(csv)) {
        h <- names(utils::read.csv(csv, nrows = 0L))
        if (!all(c("operation", "repetition", "status") %in% h))
          stop("Cannot resume an old-format checkpoint; choose a new run_dir")
      }
    } else {
      if (file.exists(cfg_file) || file.exists(csv))
        stop("Output directory contains a benchmark; use resume=TRUE or a new run_dir")
      saveRDS(manifest, cfg_file)
    }
  } else if (isTRUE(resume)) stop("resume=TRUE requires run_dir")

  details <- if (!is.null(csv) && file.exists(csv))
    utils::read.csv(csv, na.strings = c("", "NA"),
                    stringsAsFactors = FALSE, check.names = FALSE) else data.frame()
  completed <- if (nrow(details))
    paste(details$scenario, details$driver, details$table_type,
          details$operation, details$repetition, sep = "|") else character()
  start_time <- proc.time()[["elapsed"]]
  over_time <- function() (proc.time()[["elapsed"]] - start_time) / 3600 >= max_hours
  run_tag <- paste0(Sys.getpid(), "_", gsub("[^0-9]", "",
                  format(Sys.time(), "%Y%m%d%H%M%OS3")))
  # Track live targets for cleanup on error/fail_fast (including on
  # connections supplied by callers, which remain open after benchmarking).
  outstanding <- new.env(parent = emptyenv())
  register_table <- function(driver, name) {
    assign(paste(driver, name, sep = "|"), list(driver = driver, name = name),
           envir = outstanding)
  }
  forget_table <- function(driver, name) {
    key <- paste(driver, name, sep = "|")
    if (exists(key, envir = outstanding, inherits = FALSE))
      rm(list = key, envir = outstanding)
  }
  drop_table <- function(cx, driver, name) {
    if (!valid(cx)) return(invisible(FALSE))
    ok <- tryCatch({
      DBI::dbExecute(cx, paste0("DROP TABLE IF EXISTS ",
        as.character(DBI::dbQuoteIdentifier(cx, name))))
      TRUE
    }, error = function(e) FALSE)
    if (ok) forget_table(driver, name)
    invisible(ok)
  }
  on.exit({
    for (key in ls(outstanding, all.names = TRUE)) {
      target <- get(key, envir = outstanding, inherits = FALSE)
      cx <- con[[target$driver]]
      drop_table(cx, target$driver, target$name)
    }
  }, add = TRUE)
  append_jobs <- ("append" %in% append_modes) * length(drivers) +
    ("append_table_lock" %in% append_modes) * 1L
  total_jobs <- nrow(plan) * length(table_types) *
    (length(con) * (repetitions_write + repetitions_read) +
                               append_jobs * repetitions_write)
  if (isTRUE(verbose)) {
    cat(sprintf("Benchmark: %d scenarios, %d drivers, tables=%s, write=%d, read=%d, append=%s; %d timed runs\n",
                nrow(plan), length(con), paste(table_types, collapse = ","),
                repetitions_write, repetitions_read,
                if (length(append_modes)) paste(append_modes, collapse = ",") else "none",
                total_jobs))
    cat(sprintf("Rows requested: write=%s; read=%s. Payload cap: %.0f MB per case\n",
                paste(unique(plan$n_write_requested), collapse = ","),
                paste(unique(plan$n_read_requested), collapse = ","), max_case_payload_mb))
  }

  valid <- function(cx) isTRUE(tryCatch(DBI::dbIsValid(cx), error = function(e) FALSE))
  ensure_connection <- function(driver) {
    cx <- con[[driver]]
    if (valid(cx)) return(cx)
    callback <- if (is.null(reconnect)) NULL else reconnect[[driver]]
    if (is.null(callback)) stop(
      "Connection invalid after an earlier failure; provide reconnect[['",
      driver, "']] or create a new connection", call. = FALSE)
    replacement <- callback()
    if (!methods::is(replacement, "DBIConnection") || !valid(replacement))
      stop("Reconnect callback did not return a valid DBI connection: ", driver)
    con[[driver]] <<- replacement
    if (isTRUE(verbose)) message("Reconnected ", driver, " after an earlier failure")
    replacement
  }
  append_result <- function(row) {
    if (!is.null(csv)) .bench_append_csv(row, csv)
    details <<- if (nrow(details)) rbind(details, row) else row
    completed <<- c(completed, paste(row$scenario, row$driver, row$table_type,
                                     row$operation, row$repetition, sep = "|"))
    if (isTRUE(verbose)) {
      secs <- if (row$operation == "read") row$read_s else row$write_s
      cat(sprintf("[%d/%d] %-12s %-16s %-9s %-18s rep %d: %7.3fs %s%s\n",
                  length(completed), total_jobs, row$driver, row$type,
                  row$table_type, row$operation, row$repetition, secs, row$status,
                  if (!is.na(row$error)) paste0(" | ", substr(row$error, 1L, 175L)) else ""))
      flush.console()
    }
    if (isTRUE(fail_fast) && row$status != "ok") {
      if (!is.null(run_dir)) utils::write.csv(benchmark_summary(details),
        file.path(run_dir, "summary.csv"), row.names = FALSE, na = "")
      stop("Benchmark failed: ", row$error, call. = FALSE)
    }
  }
  for (s in seq_len(nrow(plan))) {
    if (over_time()) break
    sc <- as.list(plan[s, , drop = FALSE])
    schema <- .bench_schema(sc)
    # Alternate which table type runs first across scenarios to reduce
    # systematic warm-cache advantage for either target type.
    target_order <- if (s %% 2L == 0L) rev(table_types) else table_types
    for (table_type in target_order) {
      table_prefix <- if (table_type == "temporary") "#br_" else "br_"
      # Session-local SQL tables can survive from a successful timed write
      # into the read phase. Keep the most recent successful one when row counts match;
      # for read-only, different-size, resumed or failed writes, stage the read
      # table outside the read stopwatch as before.
      reuse_write <- repetitions_write > 0L && repetitions_read > 0L &&
        sc$n_write_rows == sc$n_read_rows
      read_tabs <- stats::setNames(lapply(seq_along(drivers), function(j)
        sprintf("%s%s_r_%04d_%02d", table_prefix, run_tag, s, j)), drivers)
      prepared <- stats::setNames(rep(FALSE, length(drivers)), drivers)
      for (operation in c("write", append_modes, "read")) {
        reps <- if (operation == "read") repetitions_read else repetitions_write
        if (!reps || over_time()) next
        # The TableLock variant is intentionally available only on our driver;
        # ODBC gets its ordinary append; RSqlClientNET baseline explicitly uses table_lock=FALSE.
        operation_drivers <- if (operation == "append_table_lock") "RSqlClientNET" else drivers
        n_active <- if (operation == "read") sc$n_read_rows else sc$n_write_rows
        operation_keys <- unlist(lapply(operation_drivers, function(d)
          paste(sc$scenario, d, table_type, operation, seq_len(reps), sep = "|")))
        if (all(operation_keys %in% completed)) next
        sub_sc <- sc
        sub_sc$n_rows <- n_active
        generation_error <- NA_character_
        dat <- tryCatch(.bench_make_data(sub_sc, seed = seed), error = function(e) {
          generation_error <<- conditionMessage(e)
          NULL
        })
        object_mb <- if (is.null(dat)) NA_real_ else
          as.numeric(utils::object.size(dat)) / 1024^2
        payload_mb <- n_active * sc$row_bytes_est * (1 - sc$null_fraction) / 1024^2
        # The read phase creates an untimed fixture only if it cannot reuse
        # the last successful write from this scenario and this connection.
        for (rep in seq_len(reps)) {
          if (over_time()) break
          driver_order <- if (rep %% 2L == 0L) rev(operation_drivers) else operation_drivers
          for (driver in driver_order) {
            if (over_time()) break
            key <- paste(sc$scenario, driver, table_type, operation, rep, sep = "|")
            if (key %in% completed) next
            elapsed_s <- NA_real_
            rows_read <- columns_read <- NA_integer_
            status <- if (is.null(dat)) "generation_error" else "ok"
            error <- generation_error
            cx <- NULL
            keep_write <- FALSE
            # Keep the baseline write table intact for the untimed read reuse.
            suffix <- switch(operation, append = "_ap", append_table_lock = "_tl", "")
            write_tab <- sprintf("%s%s_w_%04d_%02d_%02d%s", table_prefix,
                                 run_tag, s, rep, match(driver, drivers), suffix)
            if (!is.null(dat)) {
              tryCatch({
                # A reconnected SQL Server session has lost its #temp tables,
                # regardless of whether reconnection occurs during write or read.
                if (!valid(con[[driver]]))
                  prepared[[driver]] <- FALSE
                cx <- ensure_connection(driver)
                if (operation == "read" && !prepared[[driver]]) {
                  # A reconnected session loses #temp tables but NOT permanent
                  # tables. Clear a previously retained permanent table before
                  # staging a new untimed read fixture after reconnect.
                  if (table_type == "permanent")
                    drop_table(cx, driver, read_tabs[[driver]])
                  # Setup is deliberately outside the read stopwatch.
                  register_table(driver, read_tabs[[driver]])
                  DBI::dbWriteTable(cx, read_tabs[[driver]], dat,
                                    row.names = FALSE, field.types = schema)
                  prepared[[driver]] <- TRUE
                }
                # Append modes use an already-created empty table, outside the
                # stopwatch. This isolates bulk insertion from CREATE TABLE and
                # matches the conditions in the successful RC34a TableLock test.
                if (operation %in% c("append", "append_table_lock")) {
                  register_table(driver, write_tab)
                  DBI::dbCreateTable(cx, write_tab, schema)
                }
                gc(verbose = FALSE)
                t0 <- proc.time()[["elapsed"]]
                if (operation == "write") {
                  register_table(driver, write_tab)
                  DBI::dbWriteTable(cx, write_tab, dat,
                                    row.names = FALSE, field.types = schema)
                } else if (operation == "append_table_lock") {
                  DBI::dbAppendTable(cx, write_tab, dat, table_lock = TRUE)
                } else if (operation == "append") {
                  if (identical(driver, "RSqlClientNET"))
                    DBI::dbAppendTable(cx, write_tab, dat, table_lock = FALSE)
                  else DBI::dbAppendTable(cx, write_tab, dat)
                } else {
                  out <- DBI::dbReadTable(cx, read_tabs[[driver]])
                }
                elapsed_s <- proc.time()[["elapsed"]] - t0
                # Validation runs outside the timed section for append variants.
                if (operation %in% c("append", "append_table_lock")) {
                  qt <- as.character(DBI::dbQuoteIdentifier(cx, write_tab))
                  actual <- as.numeric(DBI::dbGetQuery(cx,
                    paste0("SELECT COUNT_BIG(*) AS n FROM ", qt))$n[[1L]])
                  if (!identical(actual, as.numeric(n_active)))
                    stop("Append count mismatch: got ", actual, ", expected ", n_active)
                }
                if (operation == "write" && reuse_write) {
                  # Keep the latest *successful* measured upload. An earlier
                  # fixture is dropped outside timing only after its replacement
                  # succeeded; a failed later write can still fall back to it.
                  if (prepared[[driver]])
                    drop_table(cx, driver, read_tabs[[driver]])
                  read_tabs[[driver]] <- write_tab
                  prepared[[driver]] <- TRUE
                  keep_write <- TRUE
                }
                if (operation == "read") {
                  rows_read <- nrow(out)
                  columns_read <- ncol(out)
                  rm(out)
                  if (rows_read != n_active || columns_read != ncol(dat))
                    stop(sprintf("Read shape mismatch: got %d rows/%d columns; expected %d/%d",
                                 rows_read, columns_read, n_active, ncol(dat)))
                }
              }, error = function(e) {
                if (operation != "read") status <<- "write_error"
                else if (!prepared[[driver]]) status <<- "read_setup_error"
                else status <<- "read_error"
                error <<- conditionMessage(e)
              }, finally = {
                if (operation != "read" && !keep_write &&
                    !is.null(cx) && valid(cx))
                  drop_table(cx, driver, write_tab)
              })
            }
            # If the table setup failed or the connection died, do not assume
            # that subsequent reads can reuse this session's #temp table.
            if (operation == "read" && !is.null(cx) && !valid(cx))
              prepared[[driver]] <- FALSE
            write_s <- if (operation != "read") elapsed_s else NA_real_
            read_s <- if (operation == "read") elapsed_s else NA_real_
            row <- data.frame(
              timestamp_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
              scenario = sc$scenario, driver = driver, table_type = table_type,
              operation = operation, repetition = rep, type = sc$type,
              n_requested = if (operation == "read") sc$n_read_requested else sc$n_write_requested,
              n_rows = n_active,
              n_write_requested = sc$n_write_requested, n_write_rows = sc$n_write_rows,
              n_read_requested = sc$n_read_requested, n_read_rows = sc$n_read_rows,
              null_fraction = sc$null_fraction, text_length = sc$text_length,
              binary_bytes = sc$binary_bytes, wide_columns = sc$wide_columns,
              columns = length(schema), r_object_mb = object_mb,
              sql_payload_est_mb = payload_mb,
              elapsed_s = elapsed_s, write_s = write_s, read_s = read_s,
              rows_read = rows_read, columns_read = columns_read,
              write_rows_s = if (is.finite(write_s) && write_s > 0 && status == "ok") n_active / write_s else NA_real_,
              read_rows_s = if (is.finite(read_s) && read_s > 0 && status == "ok") n_active / read_s else NA_real_,
              write_mb_s_est = if (is.finite(write_s) && write_s > 0 && status == "ok") payload_mb / write_s else NA_real_,
              read_mb_s_est = if (is.finite(read_s) && read_s > 0 && status == "ok") payload_mb / read_s else NA_real_,
              status = status, error = error, stringsAsFactors = FALSE
            )
            append_result(row)
          }
        }
        rm(dat)
        gc(verbose = FALSE)
      }
      # Clean up both retained write tables and standalone read fixtures,
      # including when reads were skipped due to resume or max_hours.
      for (driver in drivers) {
        cx <- con[[driver]]
        drop_table(cx, driver, read_tabs[[driver]])
      }
      if (!is.null(run_dir)) utils::write.csv(
        benchmark_summary(details), file.path(run_dir, "summary.csv"),
        row.names = FALSE, na = "")
    } # table_type
  }
  summary <- benchmark_summary(details)
  if (!is.null(run_dir)) {
    utils::write.csv(summary, file.path(run_dir, "summary.csv"),
                     row.names = FALSE, na = "")
    if (nrow(details)) utils::write.csv(
      details[details$status != "ok", , drop = FALSE],
      file.path(run_dir, "failures.csv"), row.names = FALSE, na = "")
  }
  if (isTRUE(verbose)) cat(sprintf(
    "Finished/paused: %d of %d operations; results in %s\n",
    length(completed), total_jobs,
    if (is.null(run_dir)) "return value" else
      normalizePath(run_dir, winslash = "/", mustWork = TRUE)))
  invisible(list(raw = details, details = details, summary = summary,
                 plan = plan, completed = length(completed), total = total_jobs,
                 connections = con, run_dir = run_dir))
}
