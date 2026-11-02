testthat::test_that("Arrow-style quoted names append into a session-local #table", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs)
  quoted <- DBI::dbQuoteIdentifier(con, "RSqlClientNET_arrow03430")
  # Cleanup the temporary table *before* closing the owning session.
  on.exit({
    if (DBI::dbIsValid(con)) {
      try(DBI::dbRemoveTable(con, quoted, fail_if_missing=FALSE),silent=TRUE)
      DBI::dbDisconnect(con)
    }
  },add=TRUE)
  DBI::dbCreateTable(con, quoted, data.frame(x=integer()),temporary=TRUE)
  expect_true(DBI::dbExistsTable(con,"#RSqlClientNET_arrow03430"))
  expect_true(DBI::dbExistsTable(con,quoted))
  expect_equal(as.numeric(DBI::dbAppendTable(con,quoted,data.frame(x=c(7L,9L)))),2)
  expect_identical(DBI::dbReadTable(con,quoted)$x,c(7L,9L))
  DBI::dbRemoveTable(con,quoted)
  expect_false(DBI::dbExistsTable(con,"#RSqlClientNET_arrow03430"))
})
