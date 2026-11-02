testthat::test_that("SQL Server: DBI defers @name binding with default immediate=NULL", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),connection_string=cs,tz="UTC")
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add=TRUE)

  query <- DBI::dbSendQuery(con,"SELECT CAST(@a AS int) AS result")
  expect_true(DBI::dbIsValid(query))
  expect_false(DBI::dbHasCompleted(query))
  expect_identical(DBI::dbGetRowCount(query), 0)
  expect_error(DBI::dbFetch(query), "not been bound")
  expect_identical(DBI::dbBind(query,list(a=12L)), query)
  expect_identical(DBI::dbFetch(query)$result, 12L)
  DBI::dbClearResult(query)

  DBI::dbExecute(con,"CREATE TABLE #defer03422 (id INT)")
  stmt <- DBI::dbSendStatement(con,"INSERT INTO #defer03422(id) VALUES (@a)")
  expect_false(DBI::dbHasCompleted(stmt))
  expect_identical(DBI::dbGetRowsAffected(stmt), NA_integer_)
  DBI::dbBind(stmt,list(a=42L))
  expect_identical(DBI::dbGetRowsAffected(stmt), 1L)
  DBI::dbClearResult(stmt)
  expect_identical(DBI::dbGetQuery(con,"SELECT id FROM #defer03422")$id, 42L)

  # Test the explicit direct path and avoid masking non-parameter SQL errors.
  expect_error(DBI::dbSendQuery(con,"SELECT @a AS a",immediate=TRUE), "137|declare")
  expect_error(DBI::dbSendQuery(con,"SELECT bad SQL syntax"), "syntax|SQL")
  expect_true(DBI::dbIsValid(con))
})
