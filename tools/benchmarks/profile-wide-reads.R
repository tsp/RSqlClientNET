# RSqlClientNET wide-table read profiling — run from package project root.
# Uses Windows Authentication via existing con_str or env var, and ODBC DSN.
# Creates and removes four uniquely named permanent benchmark tables.
# Never modifies an existing SQL table. Requires CREATE/DROP rights.
# Measures the same SQL Server table from both drivers.

run_wide_read_profile <- function(
    n = 100000L, columns = c(25L, 100L),
    null_fractions = c(0, 0.5), repetitions = 3L,
    dsn = "SqlClientRTest", output = NULL,
    batch_rows = 32768L) {
  stopifnot(is.numeric(n), length(n) == 1L, n >= 1L, n <= 1000000L,
            is.numeric(repetitions), length(repetitions) == 1L,
            repetitions >= 1L, repetitions <= 20L)
  stopifnot(length(batch_rows) == 1L, is.numeric(batch_rows),
            !is.na(batch_rows), batch_rows == floor(batch_rows),
            batch_rows >= 1L, batch_rows <= 32768L)
  if (!file.exists("tools/benchmarks/benchmark_dbi.R"))
    stop("Run this script from the RSqlClientNET project root")
  if (!requireNamespace("RSqlClientNET", quietly = TRUE) ||
      !requireNamespace("odbc", quietly = TRUE) ||
      !requireNamespace("DBI", quietly = TRUE))
    stop("Install RSqlClientNET, DBI and odbc")

  benchmark_env <- new.env(parent = globalenv())
  sys.source("tools/benchmarks/benchmark_dbi.R", envir = benchmark_env)
  cs <- if (exists("con_str", envir = .GlobalEnv, inherits = FALSE)) {
    get("con_str", envir = .GlobalEnv, inherits = FALSE)
  } else Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  if (!is.character(cs) || length(cs) != 1L || !nzchar(cs))
    stop("Set con_str or RSQLCLIENTNET_TEST_CONNECTION_STRING")

  # No SQL time-zone information is stored in DATETIME2. The benchmark's
  # artificial DATETIME2 values originate from UTC, so both resolutions are
  # measured to isolate the cost of local-time conversion. Do NOT equate their
  # timestamp semantics for arbitrary user databases.
  connections <- list(
    RSqlClientNET_local = DBI::dbConnect(RSqlClientNET::SqlClientNET(),
                       connection_string = cs, tz = "", data_table = FALSE,
                       batch_rows = as.integer(batch_rows)),
    RSqlClientNET_UTC = NULL,
    RSqlClientNET_dt = NULL,
    odbc = NULL
  )
  tables <- character()
  old_profile <- getOption("RSqlClientNET.dbi_read_profile", FALSE)
  on.exit({
    options(RSqlClientNET.dbi_read_profile = old_profile)
    for (tab in rev(tables)) {
      try(DBI::dbRemoveTable(connections$RSqlClientNET_local, tab), silent = TRUE)
    }
    for (cx in connections) {
      if (!is.null(cx)) try(DBI::dbDisconnect(cx), silent = TRUE)
    }
  }, add = TRUE)
  connections$RSqlClientNET_UTC <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
                               connection_string = cs, tz = "UTC", data_table = FALSE,
                              batch_rows = as.integer(batch_rows))
  connections$RSqlClientNET_dt <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
                              connection_string = cs, tz = "", data_table = TRUE,
                              batch_rows = as.integer(batch_rows))
  connections$odbc <- DBI::dbConnect(odbc::odbc(), dsn = dsn)

  # Check identical target database/server without exposing credentials.
  query <- "SELECT DB_NAME() AS db, CAST(SERVERPROPERTY('ServerName') AS nvarchar(128)) AS server"
  endpoints <- lapply(connections, DBI::dbGetQuery, statement = query)
  signatures <- vapply(endpoints, function(x) paste(x$db[1], x$server[1]), character(1))
  if (length(unique(signatures)) != 1L)
    stop("Connections do not target the same database and server: ",
         paste(names(signatures), signatures, collapse = "; "))

  plan <- benchmark_env$benchmark_plan(
    n = n, types = "wide_mixed", wide_columns = columns,
    null_fractions = null_fractions, max_wide_rows = n,
    max_case_payload_mb = 4096)
  measurements <- list()
  idx <- 0L
  run_tag <- sprintf("rscn_wp_%d_%s", Sys.getpid(), format(Sys.time(), "%H%M%S"))

  for (k in seq_len(nrow(plan))) {
    sc <- as.list(plan[k, , drop = FALSE]); sc$n_rows <- sc$n_read_rows
    tab <- sprintf("%s_%03d", run_tag, k)
    cat(sprintf("\n=== %d rows, %d columns, %.0f%% NULL ===\n",
                sc$n_rows, sc$wide_columns, 100 * sc$null_fraction))
    dat <- benchmark_env$.bench_make_data(sc, seed = 17L)
    types <- benchmark_env$.bench_schema(sc)
    DBI::dbWriteTable(connections$RSqlClientNET_local, tab, dat,
                      field.types = types, row.names = FALSE)
    tables <- c(tables, tab)
    rm(dat); gc(verbose = FALSE)

    # One profiled query per case: inspect breakdown without timing this run.
    options(RSqlClientNET.dbi_read_profile = TRUE)
    cat("\nDetailed timing: RSqlClientNET (system local time zone):\n")
    invisible(DBI::dbReadTable(connections$RSqlClientNET_local, tab))
    options(RSqlClientNET.dbi_read_profile = FALSE)

    for (r in seq_len(repetitions)) {
      order <- if (r %% 2L) names(connections) else rev(names(connections))
      for (name in order) {
        gc(verbose = FALSE)
        t0 <- proc.time()[["elapsed"]]
        result <- DBI::dbReadTable(connections[[name]], tab)
        elapsed <- proc.time()[["elapsed"]] - t0
        if (nrow(result) != sc$n_rows || ncol(result) != sc$wide_columns)
          stop("Unexpected result dimensions for ", name)
        idx <- idx + 1L
        measurements[[idx]] <- data.frame(
          width = sc$wide_columns, null_fraction = sc$null_fraction,
          n_rows = sc$n_rows, driver = name, repetition = r,
          elapsed_seconds = elapsed)
        cat(sprintf("%s rep %d: %.3f seconds\n", name, r, elapsed))
        rm(result)
      }
    }
  }
  out <- do.call(rbind, measurements)
  aggregate(elapsed_seconds ~ width + null_fraction + driver,
            out, median) |>
    print(row.names = FALSE)
  if (!is.null(output)) utils::write.csv(out, output, row.names = FALSE)
  invisible(out)
}

# Explicit call; source() alone does not create tables.
# out <- run_wide_read_profile(repetitions = 3L)
