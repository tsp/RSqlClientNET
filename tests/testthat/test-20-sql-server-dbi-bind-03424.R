testthat::test_that("SQL Server DBItest timestamps and binary values bind exactly", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs, tz="UTC")
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add=TRUE)
  # Quoted literal and bound parameter must both be valid datetime2, not
  # locale-/timezone-dependent strings (which fail with SQL 241).
  for (timestamp in list(as.POSIXct("2023-12-17 02:40:22",tz="UTC"),
                         as.POSIXlt(as.POSIXct("2023-12-17 02:40:49",tz="UTC")))) {
    quoted <- DBI::dbQuoteLiteral(con, timestamp)
    expect_match(as.character(quoted), "CAST\\('2023-12-17T02:40:")
    res <- DBI::dbSendQuery(con, paste0("SELECT CASE WHEN @a = ", quoted,
                  " THEN 1 ELSE 0 END AS ok"))
    DBI::dbBind(res, list(a=timestamp))
    expect_identical(DBI::dbFetch(res)$ok, 1L)
    DBI::dbClearResult(res)
  }
  expect_identical(as.character(DBI::dbQuoteLiteral(con,as.POSIXct(NA,tz="UTC"))), "NULL")
  raw_values <- list(a=list(as.raw(1:10)), b=list(raw(3)), c=list(NULL))
  res <- DBI::dbSendQuery(con,
    "SELECT CASE WHEN @a=0x0102030405060708090A AND @b=0x000000 AND @c IS NULL THEN 1 ELSE 0 END AS ok")
  DBI::dbBind(res,raw_values)
  expect_identical(DBI::dbFetch(res)$ok, 1L)
  DBI::dbClearResult(res)
})
