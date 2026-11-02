testthat::test_that("DBI single-column binary and XML fast path crosses batch boundaries", {
  .rsqlclientnet_skip_no_sql()
  cx <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
      connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"), tz = "UTC")
  on.exit(if (DBI::dbIsValid(cx)) DBI::dbDisconnect(cx), add=TRUE)

  # Cross several 8192-row SCB2 batches without generating R test data.
  cte <- paste0(
    "WITH numbers AS (SELECT TOP (20005) ",
    "ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS i ",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b) ")
  q_bin <- paste0(cte,
    "SELECT CASE WHEN i % 19 = 0 THEN CAST(NULL AS VARBINARY(16)) ",
    "WHEN i % 23 = 0 THEN CAST(0x AS VARBINARY(16)) ",
    "ELSE CONVERT(VARBINARY(16), CAST(i AS INT)) END AS value ",
    "FROM numbers ORDER BY i")
  b <- DBI::dbGetQuery(cx, q_bin)
  testthat::expect_equal(nrow(b), 20005L)
  testthat::expect_true(is.raw(b$value[[1L]]))
  testthat::expect_null(b$value[[19L]])
  testthat::expect_identical(b$value[[23L]], raw(0))
  testthat::expect_identical(b$value[[8192L]], as.raw(c(0L,0L,32L,0L)))
  testthat::expect_true(DBI::dbIsValid(cx))

  q_xml <- paste0(cte,
    "SELECT CASE WHEN i % 19 = 0 THEN CAST(NULL AS XML) ",
    "ELSE CAST(N'<tag>æøå 漢字</tag>' AS XML) END AS value ",
    "FROM numbers ORDER BY i")
  x <- DBI::dbGetQuery(cx, q_xml)
  testthat::expect_equal(nrow(x), 20005L)
  testthat::expect_true(grepl("漢字", x$value[[1L]], fixed=TRUE))
  testthat::expect_true(is.na(x$value[[19L]]))
  testthat::expect_identical(x$value[[8192L]], x$value[[1L]])

  q_both <- paste0(cte,
    "SELECT TOP (10) CONVERT(VARBINARY(16), CAST(i AS INT)) AS bytes, ",
    "CAST(N'<tag>ok</tag>' AS XML) AS doc FROM numbers ORDER BY i")
  both <- DBI::dbGetQuery(cx, q_both)
  testthat::expect_equal(nrow(both), 10L)
  testthat::expect_true(is.raw(both$bytes[[1L]]))
  testthat::expect_match(both$doc[[1L]], "ok")
  testthat::expect_true(DBI::dbIsValid(cx))
})
