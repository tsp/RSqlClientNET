testthat::test_that("SQL Server bulk DECIMAL and MONEY with NA round-trip", {
  .rsqlclientnet_skip_no_sql()
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string = cs, tz = "UTC")
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)

  values <- data.frame(
    id = 1:6,
    amount = c(123.4567, NA_real_, -9.5, NA_real_, 0, 17.125),
    price = c(123.4567, NA_real_, -9.5, NA_real_, 0, 17.125),
    label = c("ok", NA_character_, "neg", NA_character_, "zero", "ok")
  )
  # Batch boundaries deliberately cross NULL rows; both decimal code paths
  # must produce the bitmap bit rather than passing the characters "NA".
  testthat::expect_true(DBI::dbWriteTable(
    con, "#sbin_decimal_na_03434", values,
    field.types = c(id = "INT", amount = "DECIMAL(18,4)",
                    price = "MONEY", label = "NVARCHAR(32)"),
    batch_rows = 2L
  ))
  counts <- DBI::dbGetQuery(con, paste(
    "SELECT COUNT(*) AS n,",
    "SUM(CASE WHEN amount IS NULL THEN 1 ELSE 0 END) AS decimal_nulls,",
    "SUM(CASE WHEN price IS NULL THEN 1 ELSE 0 END) AS money_nulls",
    "FROM #sbin_decimal_na_03434"
  ))
  testthat::expect_equal(as.numeric(counts$n[[1L]]), 6)
  testthat::expect_equal(as.numeric(counts$decimal_nulls[[1L]]), 2)
  testthat::expect_equal(as.numeric(counts$money_nulls[[1L]]), 2)
  # SQL money style 2 renders exactly four decimal places. Verify that
  # fixing the source CLR type has not introduced an implicit float cast.
  exact <- DBI::dbGetQuery(con, paste(
    "SELECT id, CAST(amount AS varchar(40)) AS amount_txt,",
    "CONVERT(varchar(40), price, 2) AS price_txt",
    "FROM #sbin_decimal_na_03434 ORDER BY id"
  ))
  testthat::expect_identical(as.character(exact$price_txt[c(1L,3L,5L,6L)]),
                             c("123.4567", "-9.5000", "0.0000", "17.1250"))
  testthat::expect_identical(as.character(exact$amount_txt[1L]), "123.4567")
  testthat::expect_true(DBI::dbIsValid(con))

  # The same session must still process a second mixed upload.
  testthat::expect_true(DBI::dbWriteTable(
    con, "#sbin_decimal_followup_03434", values[1:3, ],
    field.types = c(id = "INT", amount = "DECIMAL(18,4)",
                    price = "MONEY", label = "NVARCHAR(32)"),
    batch_rows = 1L
  ))
  testthat::expect_true(DBI::dbIsValid(con))
  testthat::expect_true(DBI::dbRemoveTable(con, "#sbin_decimal_followup_03434"))
  testthat::expect_true(DBI::dbRemoveTable(con, "#sbin_decimal_na_03434"))
})
