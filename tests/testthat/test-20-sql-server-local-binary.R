test_that("SQL Server SQL datetime2 uses binary wall clocks in IANA timezone", {
  .rsqlclientnet_skip_no_sql()
  conn <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    tz="Europe/Copenhagen", precision="native")
  on.exit(DBI::dbDisconnect(conn), add=TRUE)
  q <- paste0("SELECT CAST('2026-07-10T12:34:56.1234567' AS datetime2(7)) AS summer, ",
    "CAST('2026-01-10T12:34:56.1234567' AS datetime2(7)) AS winter")
  ans <- DBI::dbGetQuery(conn, q)
  expect_s3_class(ans$summer, "POSIXct")
  expect_identical(attr(ans$summer, "tzone"), "Europe/Copenhagen")
  expect_identical(format(ans$summer, "%Y-%m-%d %H:%M:%S"), "2026-07-10 12:34:56")
  expect_identical(format(ans$winter, "%Y-%m-%d %H:%M:%S"), "2026-01-10 12:34:56")
  expect_equal(as.numeric(ans$summer) - as.numeric(ans$winter),
    as.numeric(difftime(as.POSIXct("2026-07-10 10:34:56", tz="UTC"),
                         as.POSIXct("2026-01-10 11:34:56", tz="UTC"), units="secs")),
    tolerance=1e-5)
})
