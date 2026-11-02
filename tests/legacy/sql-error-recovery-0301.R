# Run on SQL Server Express or another TEST database.
# Requires only session-scoped #temp tables; no permanent objects modified.
# Sys.setenv(RSQLCLIENTNET_TEST_CONNECTION_STRING = con_str)
run_sql_error_recovery_0301 <- function() {
  library(RSqlClientNET)
  library(DBI)
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  if (!nzchar(cs)) stop("Set RSQLCLIENTNET_TEST_CONNECTION_STRING first.")
  expect_recoverable <- function(expr) {
    e <- tryCatch({ force(expr); NULL }, error=identity)
    if (!inherits(e, "sqlclient_recoverable_sql_error") || !isTRUE(e$recoverable)) {
      stop("Expected a recoverable SQL error; got: ",
           if (is.null(e)) "no error" else paste(class(e), collapse="/"),
           if (inherits(e,"error")) paste0(": ", conditionMessage(e)) else "")
    }
    stopifnot(is.numeric(e$sql_number), e$sql_number > 0)
    invisible(e)
  }

  message("sql-0301: DBI syntax and name-resolution errors preserve #temp")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs,
                        tz="UTC", precision="native")
  on.exit(try(DBI::dbDisconnect(con), silent=TRUE), add=TRUE)
  DBI::dbExecute(con, "CREATE TABLE #sql0301 (id INT NOT NULL)")
  DBI::dbExecute(con, "INSERT INTO #sql0301 VALUES (17)")
  expect_recoverable(DBI::dbGetQuery(con, "SELEC id FROM #sql0301"))
  stopifnot(DBI::dbIsValid(con),
            DBI::dbGetQuery(con, "SELECT id FROM #sql0301")$id[[1L]] == 17L)
  expect_recoverable(DBI::dbGetQuery(con, "SELECT unknown_column FROM #sql0301"))
  stopifnot(DBI::dbIsValid(con),
            DBI::dbExistsTable(con, "#sql0301"))
  expect_recoverable(DBI::dbExecute(con, "INSER INTO #sql0301 VALUES (19)"))
  stopifnot(DBI::dbIsValid(con),
            DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM #sql0301")$n[[1L]] == 1L)
  # A completed SQL exception must not leave a DBI cursor locked.
  res <- DBI::dbSendQuery(con, "SELECT id FROM #sql0301")
  stopifnot(DBI::dbFetch(res, n=1)$id[[1L]] == 17L)
  DBI::dbClearResult(res)
  stopifnot(DBI::dbIsValid(con))

  message("sql-0301: direct Rcpp columnar, CSV and callback queries")
  h <- con@handle
  expect_recoverable(RSqlClientNET::sql_query(h, "SELEC id FROM #sql0301",
                                           result_format="columnar", tz="UTC"))
  stopifnot(RSqlClientNET::sql_is_valid(h))
  expect_recoverable(RSqlClientNET::sql_query(h, "SELEC id FROM #sql0301",
                                           result_format="csv", tz="UTC"))
  stopifnot(RSqlClientNET::sql_is_valid(h))
  expect_recoverable(RSqlClientNET::sql_query_stream(h, "SELEC id FROM #sql0301",
                                                  callback=function(batch) NULL,
                                                  tz="UTC"))
  stopifnot(RSqlClientNET::sql_is_valid(h),
            RSqlClientNET::sql_query(h, "SELECT id FROM #sql0301",
                                  result_format="columnar", tz="UTC")$id[[1L]] == 17L)

  message("sql-0301: incomplete stream still invalidates")
  # Synthetic fault_test from existing regression suite remains destructive:
  probe <- RSqlClientNET:::sql_robustness_test("rcpp")
  stopifnot(isTRUE(probe$ok))

  message("RSqlClientNET 0.0.30.1: SQL error recovery tests passed")
  invisible(TRUE)
}
run_sql_error_recovery_0301()
