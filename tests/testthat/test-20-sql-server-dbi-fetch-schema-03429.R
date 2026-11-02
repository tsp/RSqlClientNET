testthat::test_that("SQL Server unnamed columns, fetch NA, and quoted local temporary table", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs)
  res <- NULL
  on.exit({
    if (!is.null(res) && DBI::dbIsValid(res)) DBI::dbClearResult(res)
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  }, add=TRUE)
  res <- DBI::dbSendQuery(con, "SELECT 1, 2 AS a, 3, 4")
  colinfo <- DBI::dbColumnInfo(res)
  data <- DBI::dbFetch(res, n=NA)
  testthat::expect_identical(colinfo$name, names(data))
  testthat::expect_identical(names(data), c("V1", "a", "V2", "V3"))
  testthat::expect_identical(nrow(data), 1L)
  DBI::dbClearResult(res)
  res <- NULL
  name <- "RSqlClientNET_temp03429"
  on.exit(try(DBI::dbRemoveTable(con, name, fail_if_missing=FALSE), silent=TRUE), add=TRUE, after=FALSE)
  DBI::dbCreateTable(con, DBI::SQL("[RSqlClientNET_temp03429]"),
                     data.frame(x=integer()), temporary=TRUE)
  testthat::expect_true(DBI::dbExistsTable(con, name))
  testthat::expect_true(DBI::dbExistsTable(con, "#RSqlClientNET_temp03429"))
  DBI::dbRemoveTable(con, name)
})
