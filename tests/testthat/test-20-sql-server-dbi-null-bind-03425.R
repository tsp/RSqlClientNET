testthat::test_that("SQL Server roundtrips raw and blob NULL vs empty binary values", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string = cs)
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

  check <- function(values) {
    res <- DBI::dbSendQuery(con,
      paste0("SELECT CASE WHEN ",
             "CAST(@a AS VARBINARY(MAX))=0x0102030405060708090A ",
             "AND CAST(@b AS VARBINARY(MAX))=0x000000 ",
             "AND CAST(@c AS VARBINARY(MAX)) IS NULL ",
             "AND CAST(@d AS VARBINARY(MAX))=0x ",
             "THEN 1 ELSE 0 END AS ok"))
    on.exit(if (DBI::dbIsValid(res)) DBI::dbClearResult(res), add=TRUE)
    DBI::dbBind(res, values)
    testthat::expect_identical(DBI::dbFetch(res)$ok, 1L)
  }

  check(list(a=list(as.raw(1:10)), b=list(raw(3)),
             c=list(NULL), d=list(raw(0))))
  testthat::skip_if_not_installed("blob")
  check(list(a=blob::blob(as.raw(1:10)), b=blob::blob(raw(3)),
             c=blob::blob(NULL), d=blob::blob(raw(0))))
})
