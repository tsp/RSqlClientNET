testthat::test_that("blob quotes use SQL Server 0x literals, not ANSI X literals", {
  testthat::skip_if_not_installed("blob")
  b <- blob::blob(as.raw(1:10), raw(0), NULL)
  names(b) <- c("a", "empty", "missing")
  fn <- getFromNamespace(".dbi_blob_literals", "RSqlClientNET")
  quoted <- fn(b)
  testthat::expect_s4_class(quoted, "SQL")
  # DBI::SQL() / as.character() can strip the names attribute depending on
  # the version of DBI; the SQL literals themselves are what matters.
  testthat::expect_identical(unname(as.character(quoted)),
    c("0x0102030405060708090A", "0x", "NULL"))
  testthat::expect_identical(as.character(fn(b[1L])), "0x0102030405060708090A")
  testthat::expect_length(fn(b[0L]), 0L)
  testthat::expect_error(fn(list(as.raw(1:2))), "blob vector")
  # Verify actual S4 generic dispatch, not the internal representation of
  # MethodDefinition; quoting does not require an open SQL connection.
  handle <- new.env(parent=emptyenv())
  class(handle) <- "sqlclient_connection"
  con <- methods::new("SqlClientNETConnection", handle=handle)
  testthat::expect_identical(unname(as.character(DBI::dbQuoteLiteral(con, b))),
    c("0x0102030405060708090A", "0x", "NULL"))
  testthat::expect_identical(as.character(DBI::dbQuoteLiteral(con, as.raw(1:2))),
    "0x0102")
  testthat::expect_identical(as.character(DBI::dbQuoteLiteral(con, list(raw(0), NULL))),
    c("0x", "NULL"))
})
