testthat::test_that("SQL Server: DBItest cursor replacement and bigint modes", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),connection_string=cs,
                        precision="native",tz="UTC")
  on.exit(if (DBI::dbIsValid(con)) try(DBI::dbDisconnect(con),silent=TRUE),add=TRUE)
  first <- DBI::dbSendQuery(con,"SELECT CAST(1 AS INT) AS a")
  second <- NULL
  testthat::expect_warning(
    second <- DBI::dbSendQuery(con,"SELECT CAST(2 AS INT) AS b"),
    "Previous DBI result")
  testthat::expect_false(DBI::dbIsValid(first))
  testthat::expect_identical(DBI::dbFetch(second)$b,2L)
  DBI::dbClearResult(second)
  testthat::expect_warning(DBI::dbClearResult(first),"already")
  anon <- DBI::dbGetQuery(con,"SELECT 10000000000")
  testthat::expect_identical(ncol(anon),1L)
  for (mode in c("character","integer64","numeric","integer")) {
    c2 <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),connection_string=cs,
                         precision="native",bigint=mode,tz="UTC")
    value <- DBI::dbGetQuery(c2,
        "SELECT CAST(10000000000 AS BIGINT) AS large_value")$large_value
    if (mode == "character") testthat::expect_identical(value,"10000000000")
    if (mode == "numeric") testthat::expect_identical(value,1e10)
    if (mode == "integer") testthat::expect_true(is.na(value))
    if (mode == "integer64") testthat::expect_identical(as.character(value),"10000000000")
    DBI::dbDisconnect(c2)
  }
  DBI::dbDisconnect(con)
  testthat::expect_warning(DBI::dbDisconnect(con),"already")
})
