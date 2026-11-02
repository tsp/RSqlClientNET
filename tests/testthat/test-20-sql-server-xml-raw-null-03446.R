testthat::test_that("single-column XML preserves Unicode, NULL and chunk boundaries", {
  .rsqlclientnet_skip_no_sql()
  cx <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    tz = "UTC")
  on.exit(if (DBI::dbIsValid(cx)) DBI::dbDisconnect(cx), add = TRUE)

  nums <- paste0("WITH nums AS (SELECT TOP (16525) ",
    "ROW_NUMBER() OVER (ORDER BY (SELECT NULL)) AS i ",
    "FROM sys.all_objects a CROSS JOIN sys.all_objects b) ")
  sql <- paste0(nums,
    "SELECT CASE WHEN i % 29 = 0 THEN CAST(NULL AS XML) ",
    "WHEN i % 31 = 0 THEN CAST(N'<empty />' AS XML) ",
    "ELSE CAST(N'<root>æøå &amp; 漢字</root>' AS XML) END AS doc ",
    "FROM nums ORDER BY i")
  out <- DBI::dbGetQuery(cx, sql)$doc
  testthat::expect_identical(length(out), 16525L)
  testthat::expect_true(is.na(out[[29L]]))
  testthat::expect_true(grepl("漢字", out[[1L]], fixed = TRUE))
  testthat::expect_true(grepl("empty", out[[31L]], fixed = TRUE))
  testthat::expect_true(DBI::dbIsValid(cx))
})

testthat::test_that("single-column BINARY is unchanged after XML cleanup", {
  .rsqlclientnet_skip_no_sql()
  cx <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    tz = "UTC")
  on.exit(if (DBI::dbIsValid(cx)) DBI::dbDisconnect(cx), add = TRUE)
  x <- DBI::dbGetQuery(cx, paste0(
    "SELECT CAST(0x00FF AS VARBINARY(2)) AS payload ",
    "UNION ALL SELECT CAST(NULL AS VARBINARY(2))"))$payload
  testthat::expect_identical(x, list(as.raw(c(0x00, 0xFF)), NULL))
})
