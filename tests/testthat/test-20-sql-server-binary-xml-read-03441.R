testthat::test_that("VARBINARY and XML read through optimized SCB2 round-trip exactly", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
                        connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
                        tz="UTC")
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con),add=TRUE)
  query <- paste0(
    "SELECT CAST(0x00FF AS VARBINARY(2)) AS bytes, ",
    "CAST(N'<root>æøå &amp; 漢字</root>' AS XML) AS doc ",
    "UNION ALL SELECT CAST(0x00FF AS VARBINARY(2)), ",
    "CAST(N'<root>æøå &amp; 漢字</root>' AS XML) ",
    "UNION ALL SELECT CAST(NULL AS VARBINARY(2)), CAST(NULL AS XML) ",
    "UNION ALL SELECT CAST(0x AS VARBINARY(2)), CAST(N'<empty />' AS XML)")
  value <- DBI::dbGetQuery(con,query)
  testthat::expect_equal(nrow(value),4L)
  testthat::expect_identical(value$bytes[[1L]], as.raw(c(0L,255L)))
  testthat::expect_identical(value$bytes[[2L]], as.raw(c(0L,255L)))
  testthat::expect_null(value$bytes[[3L]])
  testthat::expect_identical(value$bytes[[4L]], raw(0))
  testthat::expect_identical(as.character(value$doc[1L]),as.character(value$doc[2L]))
  testthat::expect_match(as.character(value$doc[1L]),"æøå",fixed=TRUE)
  testthat::expect_match(as.character(value$doc[1L]),"漢字",fixed=TRUE)
  testthat::expect_true(is.na(value$doc[3L]))
  testthat::expect_true(DBI::dbIsValid(con))
})
