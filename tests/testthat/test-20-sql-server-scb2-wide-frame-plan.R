# Integration regression for schema-cached SCB2 size planning (RC24).
# Lightweight enough for ordinary SQL Server integration checks.
testthat::test_that("mixed fixed and variable SCB2 column sizes roundtrip with NULLs", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(
    RSqlClientNET::SqlClientNET(),
    connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    tz = "UTC", batch_rows = 37L)
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

  sql <- paste0(
    "WITH nums AS (SELECT TOP (257) ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS i ",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b) ",
    "SELECT CAST(i AS INT) AS id, ",
    "CASE WHEN i % 7 = 0 THEN CAST(NULL AS FLOAT) ELSE CAST(i * 0.125 AS FLOAT) END AS dbl, ",
    "CAST(i % 2 AS BIT) AS flag, ",
    "CASE WHEN i % 5 = 0 THEN CAST(NULL AS DECIMAL(18,2)) ",
    "ELSE CAST(i * 0.25 AS DECIMAL(18,2)) END AS amount, ",
    "DATEADD(day, CONVERT(INT, i % 1000), CAST('2020-01-01' AS DATETIME2(7))) AS dt, ",
    "CASE WHEN i % 3 = 0 THEN CAST(NULL AS NVARCHAR(MAX)) ",
    "ELSE REPLICATE(CONVERT(NVARCHAR(MAX), N'æ'), CONVERT(INT, i % 23)) END AS txt, ",
    "CASE WHEN i % 4 = 0 THEN CAST(NULL AS VARBINARY(64)) ",
    "ELSE CONVERT(VARBINARY(64), REPLICATE(CONVERT(VARCHAR(64), 'x'), ",
    "CONVERT(INT, i % 16))) END AS bin ",
    "FROM nums ORDER BY i")

  got <- DBI::dbGetQuery(con, sql)
  testthat::expect_identical(dim(got), c(257L, 7L))
  testthat::expect_identical(got$id, seq_len(257L))
  testthat::expect_equal(got$dbl[[1L]], 0.125)
  testthat::expect_true(is.na(got$dbl[[7L]]))
  testthat::expect_true(is.na(got$amount[[5L]]))
  testthat::expect_equal(as.numeric(got$amount[[6L]]), 1.5)
  testthat::expect_s3_class(got$dt, "POSIXct")
  testthat::expect_identical(got$txt[[1L]], "æ")
  testthat::expect_identical(got$txt[[23L]], "")
  testthat::expect_true(is.na(got$txt[[3L]]))
  testthat::expect_identical(got$bin[[1L]], charToRaw("x"))
  testthat::expect_null(got$bin[[4L]])

  # The direct columnar interface uses the same budgeting/writer path.
  direct <- RSqlClientNET::sql_query(
    con, sql, result_format = "columnar", batch_rows = 37L,
    precision = "native")
  testthat::expect_identical(dim(direct), c(257L, 7L))
  testthat::expect_identical(direct$txt[[23L]], "")
  testthat::expect_true(is.na(direct$txt[[3L]]))
  testthat::expect_null(direct$bin[[4L]])
  testthat::expect_gt(RSqlClientNET::sql_timings(direct)$chunks_received, 1L)
  testthat::expect_true(DBI::dbIsValid(con))
})
