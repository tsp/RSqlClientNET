testthat::test_that("direct SQL BINARY/XML read chunks preserve types and DBI behavior", {
  .rsqlclientnet_skip_no_sql()
  cx <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
      connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"), tz="UTC")
  on.exit(if (DBI::dbIsValid(cx)) DBI::dbDisconnect(cx), add=TRUE)

  nums <- paste0(
    "WITH nums AS (SELECT TOP (16525) ",
    "ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS i ",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b) ")
  q_binary <- paste0(nums,
    "SELECT CASE WHEN i % 31=0 THEN CAST(NULL AS VARBINARY(16)) ",
    "WHEN i % 37=0 THEN CAST(0x AS VARBINARY(16)) ",
    "ELSE CONVERT(VARBINARY(16), CAST(i AS INT)) END AS payload ",
    "FROM nums ORDER BY i")
  q_xml <- paste0(nums,
    "SELECT CASE WHEN i % 31=0 THEN CAST(NULL AS XML) ",
    "ELSE CAST(N'<x>æøå 漢字</x>' AS XML) END AS doc ",
    "FROM nums ORDER BY i")

  b <- DBI::dbGetQuery(cx, q_binary)
  testthat::expect_s3_class(b, "data.frame")
  testthat::expect_equal(nrow(b), 16525L)
  testthat::expect_identical(names(b), "payload")
  testthat::expect_true(is.raw(b$payload[[1L]]))
  testthat::expect_null(b$payload[[31L]])
  testthat::expect_identical(b$payload[[37L]], raw(0))
  testthat::expect_true(is.raw(b$payload[[8192L]]))

  x <- DBI::dbGetQuery(cx, q_xml)
  testthat::expect_equal(nrow(x), 16525L)
  testthat::expect_identical(names(x), "doc")
  testthat::expect_true(grepl("漢字", x$doc[[1L]], fixed=TRUE))
  testthat::expect_true(is.na(x$doc[[31L]]))
  testthat::expect_true(DBI::dbIsValid(cx))

  # Explicit data.table output must still work, and the text/base64 binary
  # conversion is deliberately left on its established R cast path.
  b64 <- DBI::dbGetQuery(cx,
    "SELECT CAST(0x00FF AS VARBINARY(2)) AS payload", binary="base64")
  testthat::expect_identical(b64$payload, "AP8=")
  dt <- DBI::dbGetQuery(cx,
    "SELECT CAST(0x00FF AS VARBINARY(2)) AS payload", data_table=TRUE)
  testthat::expect_s3_class(dt, "data.table")
  testthat::expect_identical(dt$payload[[1L]], as.raw(c(0x00, 0xFF)))
})
