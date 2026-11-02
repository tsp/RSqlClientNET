testthat::test_that("DBI blob quoting is valid T-SQL and matches bound values", {
  testthat::skip_if_not_installed("blob")
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs)
  res <- NULL
  # A DBI result must be cleared before closing its connection. Register a
  # single cleanup handler, so it also runs in the right order on test errors.
  on.exit({
    if (!is.null(res) && DBI::dbIsValid(res)) DBI::dbClearResult(res)
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  }, add=TRUE)
  b <- blob::blob(as.raw(1:10), raw(0), NULL)
  quote <- DBI::dbQuoteLiteral(con, b)
  testthat::expect_identical(as.character(quote),
    c("0x0102030405060708090A", "0x", "NULL"))
  q <- DBI::dbGetQuery(con, paste0(
    "SELECT CASE WHEN 0x0102030405060708090A = ", quote[[1L]],
    " AND DATALENGTH(", quote[[2L]], ") = 0 AND ",
    quote[[3L]], " IS NULL THEN 1 ELSE 0 END AS ok"))
  testthat::expect_identical(q$ok, 1L)
  res <- DBI::dbSendQuery(con,
    paste0("SELECT CASE WHEN CAST(@a AS VARBINARY(MAX)) = ", quote[[1L]],
           " AND DATALENGTH(CAST(@b AS VARBINARY(MAX))) = 0 ",
           "AND @c IS NULL THEN 1 ELSE 0 END AS ok"))
  DBI::dbBind(res, list(a=b[1L], b=b[2L], c=b[3L]))
  testthat::expect_identical(DBI::dbFetch(res)$ok, 1L)
  DBI::dbClearResult(res)
  res <- NULL
})
