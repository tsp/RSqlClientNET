test_that("DBI supports RSqlClientNET operational extensions and callback streaming", {
  .rsqlclientnet_skip_no_sql()
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs,
                        precision="native", tz="UTC", batch_rows=8192L)
  on.exit(try(DBI::dbDisconnect(con), silent=TRUE), add=TRUE)
  expect_true(RSqlClientNET::sql_ping(con)$ok)
  expect_true(RSqlClientNET::sql_is_valid(con))
  expect_true(RSqlClientNET::sql_connection_status(con)$valid)
  expect_true(is.list(RSqlClientNET::sql_pipe_stats(con)))
  res <- DBI::dbSendQuery(con, "SELECT n FROM (VALUES (1),(2),(3)) t(n) ORDER BY n")
  expect_error(RSqlClientNET::sql_query_stream(con, "SELECT 1",
    callback=function(batch) NULL), "dbClearResult")
  DBI::dbClearResult(res)
  seen <- integer()
  summary <- RSqlClientNET::sql_query_stream(con,
    "SELECT n FROM (VALUES (1),(2),(3)) t(n) ORDER BY n",
    batch_rows=2L, callback=function(batch) seen <<- c(seen,batch$n))
  expect_identical(as.integer(seen), 1:3)
  expect_identical(as.numeric(summary$rows_received), 3)
  expect_identical(RSqlClientNET::sql_timings(summary)$precision, "native")
  expect_identical(RSqlClientNET::sql_timings(summary)$tz, "UTC")
  expect_equal(DBI::dbGetQuery(con, "SELECT 42 AS answer")$answer, 42)
})
