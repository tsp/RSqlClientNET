# Quick DBI/SCB2 SQL Server READ benchmark (not run by R CMD check).
# Usage, from the project root, after installing RSqlClientNET:
#   Rscript tools/benchmarks/read-batch-size-benchmark.R
# Requires RSQLCLIENTNET_TEST_CONNECTION_STRING for a disposable database.
# Optional: RSQLCLIENTNET_BENCH_ROWS=200000 (max 10000000)
#           RSQLCLIENTNET_BENCH_REPEATS=3
#           RSQLCLIENTNET_BENCH_TZ=Europe/Copenhagen
# The script creates only a session-local #temp table. Neither fixture
# creation nor startup is timed. All configurations read the same rows.

suppressPackageStartupMessages({
  library(DBI)
  library(RSqlClientNET)
})

positive_integer <- function(key, default, maximum) {
  raw <- Sys.getenv(key, unset = as.character(default))
  number <- suppressWarnings(as.numeric(raw))
  if (length(number) != 1L || !is.finite(number) ||
      number < 1 || number > maximum || number != floor(number))
    stop(key, " must be an integer from 1 to ", maximum, call. = FALSE)
  as.integer(number)
}

n <- positive_integer("RSQLCLIENTNET_BENCH_ROWS", 100000L, 10000000L)
repetitions <- positive_integer("RSQLCLIENTNET_BENCH_REPEATS", 2L, 20L)
zone <- Sys.getenv("RSQLCLIENTNET_BENCH_TZ", unset = "")
cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", unset = "")
if (!nzchar(cs))
  stop("Set RSQLCLIENTNET_TEST_CONNECTION_STRING for a disposable test SQL Server database.",
       call. = FALSE)

# No user tables, ALTER permissions, or external dependencies are needed.
# One to seven ten-digit Cartesian products yield up to 10,000,000 rows
# without relying on SQL Server catalog sizes or GENERATE_SERIES() compat level.
temp <- "#RSqlClientNET_read_batch_bench"
con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
                      connection_string = cs, tz = zone,
                      precision = "native", data_table = FALSE)
on.exit_cleanup <- function() {
  if (DBI::dbIsValid(con)) {
    try(DBI::dbExecute(con, paste0("DROP TABLE IF EXISTS ", temp)), silent = TRUE)
    try(DBI::dbDisconnect(con), silent = TRUE)
  }
}

run_benchmark <- function() {
  on.exit(on.exit_cleanup(), add = TRUE)
  digit <- "(VALUES (0),(1),(2),(3),(4),(5),(6),(7),(8),(9))"
  # Every VALUES derived table needs an explicit column name (SQL Server 8155).
  # Only construct as many ten-digit tables as needed for the target row count.
  # 10,000,000 rows require seven such tables (10^7 combinations).
  digits_needed <- nchar(as.character(n - 1L))
  cross <- if (digits_needed > 1L) {
    paste(sprintf("CROSS JOIN %s AS d%d(digit)", digit,
                  seq.int(2L, digits_needed)), collapse = "\n")
  } else ""
  setup <- sprintf(paste(
    "WITH numbers AS (",
    "  SELECT TOP (%d) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS rn",
    "  FROM %s AS d1(digit) %s",
    ")",
    "SELECT CAST(rn AS INT) AS id,",
    "       CAST(rn * 0.125 AS FLOAT) AS amount,",
    "       CAST(DATEADD(SECOND, CAST(rn %% 86400 AS INT),",
    "         CAST('2026-01-15T12:00:00' AS DATETIME2(3))) AS DATETIME2(3)) AS stamp,",
    "       CAST(REPLICATE(N'abc', 12) AS NVARCHAR(36)) AS label",
    "INTO %s FROM numbers", sep = "\n"),
    n, digit, cross, temp)
  message("Preparing ", n, " fixed test rows outside timing...")
  DBI::dbExecute(con, setup)
  actual <- as.numeric(DBI::dbGetQuery(con, paste0(
    "SELECT COUNT_BIG(*) AS n FROM ", temp))$n[[1L]])
  if (actual != n) stop("Fixture row count mismatch: ", actual, " vs ", n)

  sql <- paste("SELECT id, amount, stamp, label FROM", temp)
  # One short untimed read warms the helper, SQL plan cache and R decoder.
  invisible(DBI::dbGetQuery(con, paste0(sql, " WHERE id <= 1000")))
  sizes <- c(8192L, 16384L, 32768L)
  set.seed(20261025L)
  schedule <- unlist(replicate(repetitions, sample(sizes),
                               simplify = FALSE), use.names = FALSE)
  expected_sum <- n * (n + 1) / 2
  rows <- vector("list", length(schedule))
  for (i in seq_along(schedule)) {
    bs <- schedule[[i]]
    gc(verbose = FALSE)
    elapsed <- unname(system.time({
      value <- DBI::dbGetQuery(con, sql, batch_rows = bs)
    })[["elapsed"]])
    if (nrow(value) != n || sum(value$id) != expected_sum ||
        !inherits(value$stamp, "POSIXct"))
      stop("Read validation failed with batch_rows=", bs)
    rm(value)
    rows[[i]] <- data.frame(batch_rows = bs, seconds = elapsed,
                            rows_per_second = if (elapsed > 0) n / elapsed else NA_real_)
    message(sprintf("%d/%d  batch=%5d  %.3f s", i, length(schedule), bs, elapsed))
  }
  individual <- do.call(rbind, rows)
  summary <- do.call(rbind, lapply(split(individual, individual$batch_rows), function(d)
    data.frame(batch_rows = d$batch_rows[1L],
               median_seconds = median(d$seconds),
               min_seconds = min(d$seconds),
               max_seconds = max(d$seconds),
               median_rows_per_second = median(d$rows_per_second, na.rm = TRUE))))
  summary <- summary[order(summary$batch_rows), , drop = FALSE]
  rownames(summary) <- NULL
  cat("\nDBI native read / SCB2 benchmark (", n, " rows, ", repetitions,
      " repetitions, tz=", if(nzchar(zone)) zone else "system default", ")\n", sep = "")
  print(summary, row.names = FALSE, digits = 4)
  invisible(list(summary = summary, individual = individual))
}

result <- run_benchmark()
