# Optional, manually run SQL Server comparison for vignettes/benchmarks.Rmd.
# NOT executed by R CMD check or while building the vignette.
#
# Run at the package root, using a disposable test database with CREATE/DROP
# rights. Do NOT run against production data. Never commit credentials.
# Required environment variables:
#   RSQLCLIENTNET_TEST_CONNECTION_STRING (or RSQLCLIENTNET_CONNECTION_STRING)
#   ODBC_CONNECTION_STRING (or RSQLCLIENTNET_BENCH_ODBC_CONNECTION_STRING),
#     alternatively RSQLCLIENTNET_BENCH_DSN
# Optional variables:
#   RSQLCLIENTNET_VIGNETTE_REPEATS (default 3; integer 1..30)
#   RSQLCLIENTNET_VIGNETTE_ROWS (default full nycflights13::flights)
#   RSQLCLIENTNET_BENCH_EXTRA_DRIVERS="RODBCDBI,RODBC" (requires a DSN)
#   RSQLCLIENTNET_BENCH_UID, RSQLCLIENTNET_BENCH_PWD (for DSN connections)
#
# Output: vignettes/benchmark-results.csv (medians for publication), and
#         tools/benchmarks/benchmark_results/flights-vignette-attempts.csv
#         (ignored by Git). Times exclude setup, verification, and cleanup.

run_flights_vignette_benchmark <- function() {
  for (p in c("DBI", "RSqlClientNET", "odbc", "nycflights13")) {
    if (!requireNamespace(p, quietly = TRUE))
      stop("Package required for this benchmark: ", p, call. = FALSE)
  }
  stopifnot(file.exists("DESCRIPTION"), dir.exists("vignettes"))
  cfg <- function(key, fallback = "") {
    x <- Sys.getenv(key, unset = "")
    if (nzchar(x)) x else fallback
  }
  int_env <- function(key, default, maximum) {
    s <- cfg(key, as.character(default))
    v <- suppressWarnings(as.double(s))
    if (length(v) != 1L || !is.finite(v) || v < 1 || v > maximum || v != trunc(v))
      stop(key, " must be a whole number from 1 to ", maximum, call. = FALSE)
    as.integer(v)
  }
  repetitions <- int_env("RSQLCLIENTNET_VIGNETTE_REPEATS", 3L, 30L)
  n_requested <- int_env("RSQLCLIENTNET_VIGNETTE_ROWS", 336776L, 336776L)
  net_string <- cfg("RSQLCLIENTNET_TEST_CONNECTION_STRING",
                    cfg("RSQLCLIENTNET_CONNECTION_STRING"))
  odbc_string <- cfg("RSQLCLIENTNET_BENCH_ODBC_CONNECTION_STRING",
                     cfg("ODBC_CONNECTION_STRING"))
  dsn <- cfg("RSQLCLIENTNET_BENCH_DSN")
  uid <- cfg("RSQLCLIENTNET_BENCH_UID")
  pwd <- cfg("RSQLCLIENTNET_BENCH_PWD")
  if (!nzchar(net_string))
    stop("Set RSQLCLIENTNET_TEST_CONNECTION_STRING (or RSQLCLIENTNET_CONNECTION_STRING)",
         call. = FALSE)
  if (!nzchar(odbc_string) && !nzchar(dsn))
    stop("Set ODBC_CONNECTION_STRING or RSQLCLIENTNET_BENCH_DSN", call. = FALSE)

  extra <- trimws(strsplit(cfg("RSQLCLIENTNET_BENCH_EXTRA_DRIVERS"), ",",
                            fixed = TRUE)[[1L]])
  extra <- setdiff(extra[nzchar(extra)], "")
  if (length(setdiff(extra, c("RODBC", "RODBCDBI"))))
    stop("Extra drivers must be RODBC and/or RODBCDBI", call. = FALSE)
  if (length(extra) && !nzchar(dsn))
    stop("RSQLCLIENTNET_BENCH_DSN is required for RODBC and RODBCDBI", call. = FALSE)
  for (p in extra) if (!requireNamespace(p, quietly = TRUE))
    stop("Optional comparison package is not installed: ", p, call. = FALSE)

  flights <- as.data.frame(nycflights13::flights)
  flights$time_hour <- NULL  # Same data shape as odbc benchmark vignette
  stopifnot(nrow(flights) == 336776L, ncol(flights) == 18L)
  flights <- flights[seq_len(n_requested), , drop = FALSE]
  message("Comparing drivers using ", nrow(flights), " rows and ", ncol(flights), " columns")

  # Open all connections before timing; they all must address the same DB.
  con <- list()
  made <- character()
  # A single exit handler is deliberate: drop ordinary SQL tables WHILE the
  # RSqlClientNET connection is still open, THEN disconnect all drivers.
  # Separate on.exit handlers previously ran in the wrong order on failure.
  on.exit({
    for (name in rev(unique(made))) {
      tryCatch(remove_table(name), error = function(e) {
        warning("Could not remove benchmark table ", name, ": ",
                conditionMessage(e), call. = FALSE)
      })
    }
    for (name in names(con)) {
      if (identical(name, "RODBC")) {
        try(RODBC::odbcClose(con[[name]]), silent = TRUE)
      } else {
        try(DBI::dbDisconnect(con[[name]]), silent = TRUE)
      }
    }
  }, add = TRUE)
  con$RSqlClientNET <- DBI::dbConnect(
    RSqlClientNET::SqlClientNET(), connection_string = net_string,
    data_table = FALSE)
  if (nzchar(odbc_string)) {
    con$odbc <- DBI::dbConnect(odbc::odbc(), .connection_string = odbc_string)
  } else {
    args <- list(drv = odbc::odbc(), dsn = dsn)
    if (nzchar(uid)) args$uid <- uid
    if (nzchar(pwd)) args$pwd <- pwd
    con$odbc <- do.call(DBI::dbConnect, args)
  }
  if ("RODBCDBI" %in% extra) {
    args <- list(drv = RODBCDBI::ODBC(), dsn = dsn)
    if (nzchar(uid)) args$user <- uid
    if (nzchar(pwd)) args$password <- pwd
    con$RODBCDBI <- do.call(DBI::dbConnect, args)
  }
  if ("RODBC" %in% extra) {
    args <- list(dsn = dsn)
    if (nzchar(uid)) args$uid <- uid
    if (nzchar(pwd)) args$pwd <- pwd
    con$RODBC <- do.call(RODBC::odbcConnect, args)
    if (is.numeric(con$RODBC) && con$RODBC < 0L)
      stop("RODBC::odbcConnect failed", call. = FALSE)
  }

  server <- function(dbcon) {
    DBI::dbGetQuery(dbcon, "SELECT DB_NAME() AS database_name, @@SERVERNAME AS server_name")
  }
  primary <- server(con$RSqlClientNET)
  peer <- server(con$odbc)
  if (!identical(as.character(primary$database_name), as.character(peer$database_name)) ||
      !identical(as.character(primary$server_name), as.character(peer$server_name)))
    stop("RSqlClientNET and odbc point to different SQL Server databases/servers", call. = FALSE)
  if ("RODBCDBI" %in% extra) {
    p <- server(con$RODBCDBI)
    if (!identical(as.character(p$database_name), as.character(primary$database_name)) ||
        !identical(as.character(p$server_name), as.character(primary$server_name)))
      stop("RODBCDBI points to a different database/server", call. = FALSE)
  }
  if ("RODBC" %in% extra) {
    p <- RODBC::sqlQuery(con$RODBC,
      "SELECT DB_NAME() AS database_name, @@SERVERNAME AS server_name")
    if (!is.data.frame(p) ||
        !identical(as.character(p$database_name), as.character(primary$database_name)) ||
        !identical(as.character(p$server_name), as.character(primary$server_name)))
      stop("RODBC points to a different database/server", call. = FALSE)
  }

  # tempfile() provides a new suffix even for repeated calls in the same R
  # process and avoids clashing with fixtures left by earlier interrupted runs.
  # Never delete a pre-existing fixture: it might belong to another benchmark.
  for (attempt in seq_len(20L)) {
    stem <- gsub("[^A-Za-z0-9_]", "",
                 basename(tempfile(pattern = "rscn_vignette_")))
    fixture <- paste0(stem, "_read")
    if (!DBI::dbExistsTable(con$RSqlClientNET, fixture)) break
    if (attempt == 20L)
      stop("Cannot allocate a unique benchmark fixture name", call. = FALSE)
  }
  # DROP via primary DBI connection because these are ordinary SQL tables
  # visible to all four connections, not session-local #temp tables.
  remove_table <- function(name) {
    if (DBI::dbExistsTable(con$RSqlClientNET, name))
      DBI::dbRemoveTable(con$RSqlClientNET, name)
  }
  # Register BEFORE creation so a partially failed upload is also cleaned up.
  made <- c(made, fixture)
  DBI::dbWriteTable(con$RSqlClientNET, fixture, flights, row.names = FALSE)

  count_rows <- function(name) {
    sql <- paste0("SELECT COUNT_BIG(*) AS n FROM ",
      as.character(DBI::dbQuoteIdentifier(con$RSqlClientNET, name)))
    as.numeric(DBI::dbGetQuery(con$RSqlClientNET, sql)$n[[1L]])
  }
  if (count_rows(fixture) != nrow(flights)) stop("Invalid read fixture")

  attempts <- list()
  k <- 0L
  for (operation in c("write", "read")) {
    for (rep in seq_len(repetitions)) {
      for (driver in sample(names(con))) {
        cx <- con[[driver]]
        table <- if (operation == "write") {
          paste0(stem, "_", match(driver, names(con)), "_", rep)
        } else fixture
        if (operation == "write" && DBI::dbExistsTable(con$RSqlClientNET, table))
          stop("Unexpected existing table: ", table, call. = FALSE)
        if (operation == "write") made <- c(made, table)
        gc(verbose = FALSE)
        t0 <- proc.time()[["elapsed"]]
        if (operation == "write") {
          if (driver == "RODBC") {
            status <- RODBC::sqlSave(cx, flights, tablename = table,
                                      append = FALSE, rownames = FALSE, safer = FALSE)
            if (is.character(status) && length(status))
              stop("RODBC sqlSave reported: ", paste(status, collapse = "; "))
          } else if (driver == "RODBCDBI") {
            # RODBCDBI forwards ... to RODBC::sqlSave(), whose argument is
            # `rownames` (no dot), unlike the standardized DBI `row.names`.
            # Pass FALSE explicitly: sqlSave otherwise adds a 19th column.
            DBI::dbWriteTable(cx, table, flights, rownames = FALSE)
          } else {
            DBI::dbWriteTable(cx, table, flights, row.names = FALSE)
          }
        } else {
          out <- if (driver == "RODBC") RODBC::sqlFetch(cx, table)
                 else DBI::dbReadTable(cx, table)
        }
        elapsed <- proc.time()[["elapsed"]] - t0
        if (operation == "write") {
          if (count_rows(table) != nrow(flights))
            stop("Write row-count mismatch: ", driver, call. = FALSE)
          remove_table(table)
        } else {
          if (!is.data.frame(out) || nrow(out) != nrow(flights) ||
              ncol(out) != ncol(flights))
            stop("Read shape mismatch: ", driver, call. = FALSE)
          rm(out)
        }
        k <- k + 1L
        attempts[[k]] <- data.frame(
          driver = driver, operation = operation, repetition = rep,
          seconds = elapsed, n_rows = nrow(flights),
          stringsAsFactors = FALSE)
        message(sprintf("%s  %s  rep %d/%d: %.3f s",
                        driver, operation, rep, repetitions, elapsed))
      }
    }
  }
  raw <- do.call(rbind, attempts)
  med <- aggregate(seconds ~ driver + operation + n_rows,
                   data = raw, FUN = stats::median)
  med$repetitions <- repetitions
  # Fill skipped optional drivers with blank rows; no fabricated timings.
  skeleton <- expand.grid(driver = c("RSqlClientNET", "odbc", "RODBCDBI", "RODBC"),
                          operation = c("write", "read"), stringsAsFactors = FALSE)
  med <- merge(skeleton, med, by = c("driver", "operation"), all.x = TRUE, sort = FALSE)
  med <- med[order(match(med$operation, c("write", "read")),
                   match(med$driver, c("RSqlClientNET", "odbc", "RODBCDBI", "RODBC"))), ]
  med <- med[, c("driver", "operation", "seconds", "n_rows", "repetitions")]
  rownames(med) <- NULL
  dir.create("tools/benchmarks/benchmark_results", recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(raw,
    "tools/benchmarks/benchmark_results/flights-vignette-attempts.csv",
    row.names = FALSE, na = "")
  utils::write.csv(med, "vignettes/benchmark-results.csv", row.names = FALSE, na = "")
  print(med, row.names = FALSE)
  message("Wrote vignettes/benchmark-results.csv. Re-knit vignettes/benchmarks.Rmd.")
  invisible(list(summary = med, attempts = raw))
}

if (sys.nframe() == 0L) run_flights_vignette_benchmark()
