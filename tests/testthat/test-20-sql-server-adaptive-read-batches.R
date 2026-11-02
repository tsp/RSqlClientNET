# Larger-than-128-MiB result-set regression. This allocates substantial SQL
# and R memory, so keep it opt-in for release/performance verification.
testthat::test_that("adaptive SCB2 frames handle >128 MiB SQL reads", {
  testthat::skip_if(
    Sys.getenv("RSQLCLIENTNET_RUN_LARGE_READ_TESTS", "0") != "1",
    "Enable with RSQLCLIENTNET_RUN_LARGE_READ_TESTS=1 (large memory test)")
  .rsqlclientnet_skip_no_sql()
  cx <- DBI::dbConnect(
    RSqlClientNET::SqlClientNET(),
    connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    batch_rows = 32768L, tz = "UTC")
  on.exit(if (DBI::dbIsValid(cx)) DBI::dbDisconnect(cx), add = TRUE)

  # 17,000 x 8,192 bytes > 128 MiB; exercise the DBI single-column fast path.
  numbers <- paste0(
    "WITH nums AS (SELECT TOP (17000) ",
    "ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS i ",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b) ")
  binary_sql <- paste0(numbers,
    "SELECT CASE WHEN i = 5000 THEN CAST(NULL AS VARBINARY(MAX)) ",
    "WHEN i = 10000 THEN CAST(0x AS VARBINARY(MAX)) ",
    "ELSE CONVERT(VARBINARY(MAX), REPLICATE(CONVERT(VARCHAR(MAX), 'z'), 8192)) ",
    "END AS payload FROM nums ORDER BY i")
  result <- DBI::dbGetQuery(cx, binary_sql)
  testthat::expect_identical(nrow(result), 17000L)
  testthat::expect_identical(length(result$payload[[1L]]), 8192L)
  testthat::expect_null(result$payload[[5000L]])
  testthat::expect_identical(result$payload[[10000L]], raw())
  testthat::expect_identical(length(result$payload[[17000L]]), 8192L)
  rm(result)
  gc(verbose = FALSE)

  # XML uses the same single-column path, but its byte size must be measured
  # *after* UTF-8 encoding, not in UTF-16 characters.
  xml_sql <- paste0(
    "WITH nums AS (SELECT TOP (14000) ",
    "ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS i ",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b) ",
    "SELECT CASE WHEN i = 5000 THEN CAST(NULL AS XML) ELSE ",
    "CAST(N'<x>' + REPLICATE(CONVERT(NVARCHAR(MAX), N'æ'), 5000) ",
    "+ N'</x>' AS XML) END AS doc FROM nums ORDER BY i")
  xml <- DBI::dbGetQuery(cx, xml_sql)
  testthat::expect_identical(nrow(xml), 14000L)
  testthat::expect_true(grepl("æ", xml$doc[[1L]], fixed = TRUE))
  testthat::expect_true(is.na(xml$doc[[5000L]]))
  testthat::expect_true(grepl("</x>", xml$doc[[14000L]], fixed = TRUE))
  rm(xml)
  gc(verbose = FALSE)

  # Multi-column query: 14,000 x 10,000 ASCII bytes > 128 MiB; ensure
  # the regular DBI cursor path and the direct sql_query() path both split.
  numbers <- paste0(
    "WITH nums AS (SELECT TOP (14000) ",
    "ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS i ",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b) ")
  text_sql <- paste0(numbers,
    "SELECT i AS id, CASE WHEN i = 111 THEN CAST(NULL AS VARCHAR(MAX)) ",
    "WHEN i = 222 THEN CAST('' AS VARCHAR(MAX)) ",
    "ELSE REPLICATE(CONVERT(VARCHAR(MAX), 'x'), 10000) ",
    "END AS value FROM nums ORDER BY i")
  result <- DBI::dbGetQuery(cx, text_sql)
  testthat::expect_identical(nrow(result), 14000L)
  testthat::expect_true(is.na(result$value[[111L]]))
  testthat::expect_identical(result$value[[222L]], "")
  testthat::expect_identical(nchar(result$value[[14000L]]), 10000L)
  rm(result)
  gc(verbose = FALSE)

  direct <- RSqlClientNET::sql_query(
    cx, text_sql, result_format = "columnar", batch_rows = 32768L,
    precision = "native")
  testthat::expect_identical(nrow(direct), 14000L)
  testthat::expect_identical(nchar(direct$value[[14000L]]), 10000L)
  testthat::expect_gt(RSqlClientNET::sql_timings(direct)$chunks_received, 1L)
  testthat::expect_true(DBI::dbIsValid(cx))
})
