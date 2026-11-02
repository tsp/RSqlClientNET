testthat::test_that("opt-in managed allocations preserve LOB and XML roundtrips", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
      connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add=TRUE)
  old <- options(RSqlClientNET.bulk_profile=TRUE,
                 RSqlClientNET.bulk_alloc_profile=TRUE,
                 RSqlClientNET.dbi_alloc_profile=TRUE)
  on.exit(options(old), add=TRUE)
  x <- data.frame(id=1:4,
      value=c("<root>æøå 漢字</root>", NA_character_, "<root/>", "<x>4</x>"),
      stringsAsFactors=FALSE)
  x$b <- I(list(as.raw(0:255), NULL, raw(0), as.raw(128:255)))
  messages <- character()
  withCallingHandlers({
    testthat::expect_true(DBI::dbWriteTable(con, "#alloc27", x,
        field.types=c(value="XML",b="VARBINARY(MAX)"), batch_rows=2L))
    ans <- DBI::dbGetQuery(con, "SELECT [value] FROM #alloc27 ORDER BY [id]")
    testthat::expect_equal(nrow(ans), 4L)
    testthat::expect_true(grepl("漢字", ans$value[1L], fixed=TRUE))
    testthat::expect_true(is.na(ans$value[2L]))
  }, message=function(m) {
    messages <<- c(messages, conditionMessage(m))
    invokeRestart("muffleMessage")
  })
  testthat::expect_true(any(grepl("allocation bulk:.*GetValue=", messages)))
  testthat::expect_true(any(grepl("allocation read:.*GetValue=", messages)))
  DBI::dbRemoveTable(con, "#alloc27")
})
