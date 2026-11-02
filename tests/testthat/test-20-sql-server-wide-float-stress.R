# Opt-in stress test for intermittent Windows helper AccessViolationException
# seen when fetching wide FLOAT/REAL/DECIMAL SQL Server results repeatedly.
# Pure SQL reads avoid testing the binary bulk-write path at the same time.
testthat::test_that("wide FLOAT/REAL columns survive repeated SCB2 cursor reads", {
  testthat::skip_if(
    Sys.getenv("RSQLCLIENTNET_RUN_WIDE_FLOAT_STRESS", "0") != "1",
    "Enable with RSQLCLIENTNET_RUN_WIDE_FLOAT_STRESS=1")
  .rsqlclientnet_skip_no_sql()

  cx <- DBI::dbConnect(
    RSqlClientNET::SqlClientNET(),
    connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    tz = "UTC", batch_rows = 32768L)
  on.exit(if (DBI::dbIsValid(cx)) DBI::dbDisconnect(cx), add = TRUE)

  n <- 100000L
  fields <- c(
    "CAST(n.i * 0.125 AS FLOAT)",
    "CAST(n.i * 0.125 AS REAL)",
    "CAST(n.i AS INT)",
    "CONVERT(NVARCHAR(40), n.i)",
    "CAST(n.i * 0.125 AS DECIMAL(18,4))",
    "DATEADD(day, CONVERT(INT, n.i % 365), CAST('2020-01-01' AS DATE))",
    "CONVERT(BIT, n.i % 2)",
    "CAST('00112233-4455-6677-8899-AABBCCDDEEFF' AS UNIQUEIDENTIFIER)"
  )
  source <- paste0(
    "WITH n AS (SELECT TOP (", n, ") ",
    "ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS i ",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b) ")

  for (width in c(25L, 100L)) {
    for (null_fraction in c(0, 0.5)) {
      cols <- vapply(seq_len(width), function(j) {
        expr <- fields[((j - 1L) %% length(fields)) + 1L]
        if (null_fraction != 0)
          expr <- paste0("CASE WHEN n.i % 2 = 0 THEN NULL ELSE ", expr, " END")
        paste0(expr, " AS c", j)
      }, character(1))
      sql <- paste0(source, "SELECT ", paste(cols, collapse = ", "),
                    " FROM n ORDER BY n.i")
      for (rep in seq_len(3L)) {
        got <- DBI::dbGetQuery(cx, sql)
        testthat::expect_identical(nrow(got), n)
        testthat::expect_identical(ncol(got), width)
        if (null_fraction == 0) {
          testthat::expect_equal(as.numeric(got$c1[c(1L, 100L)]),
                                 c(0.125, 12.5))
          testthat::expect_equal(as.numeric(got$c2[c(1L, 100L)]),
                                 c(0.125, 12.5), tolerance = 1e-6)
        } else {
          testthat::expect_true(is.na(got$c1[2L]))
          testthat::expect_true(is.na(got$c2[2L]))
        }
        rm(got)
      }
    }
  }
  testthat::expect_true(DBI::dbIsValid(cx))
})
