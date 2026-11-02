testthat::test_that("bulk MONEY and SMALLMONEY use CLR decimal while DECIMAL remains precise", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(
    RSqlClientNET::SqlClientNET(),
    connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"), tz = "UTC"
  )
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add = TRUE)
  table <- "#sbin_money_types_03436"
  # Include both money types, SQL DECIMAL(38,10), batches crossing NULLs,
  # and numeric R values at *both* SMALLMONEY boundaries. The R double
  # representation of 214748.3647 may format as 214748.36470000001.
  vals <- data.frame(
    id = 1:6,
    price = c(123.4567, NA_real_, -10.5, 0, 17.125, -1.25),
    small = c(123.4567, NA_real_, -10.5, 0, 214748.3647, -214748.3648),
    exact = c("1234567890123456789012345678.1234567890", NA_character_,
              "-9.5000000000", "0.0000000000", "17.1250000000", "-1.2500000000")
  )
  testthat::expect_true(DBI::dbWriteTable(con, table, vals,
    field.types = c(id="INT", price="MONEY", small="SMALLMONEY",
                    exact="DECIMAL(38,10)"), batch_rows=2L))
  result <- DBI::dbGetQuery(con, paste(
    "SELECT id, CONVERT(varchar(48),price,2) AS price,",
    "CONVERT(varchar(48),small,2) AS small,",
    "CAST(exact AS varchar(48)) AS exact FROM", table, "ORDER BY id"
  ))
  testthat::expect_identical(as.character(result$price[c(1L,3L,4L,5L)]),
    c("123.4567", "-10.5000", "0.0000", "17.1250"))
  testthat::expect_identical(as.character(result$small[c(1L,3L,4L,5L,6L)]),
    c("123.4567", "-10.5000", "0.0000", "214748.3647", "-214748.3648"))
  testthat::expect_equal(sum(is.na(result$price)), 1L)
  testthat::expect_equal(sum(is.na(result$small)), 1L)
  testthat::expect_equal(sum(is.na(result$exact)), 1L)
  testthat::expect_identical(as.character(result$exact[1L]),
    "1234567890123456789012345678.1234567890")
  testthat::expect_true(DBI::dbIsValid(con))

  # Append of a character decimal vector to an existing MONEY destination.
  testthat::expect_equal(DBI::dbAppendTable(con,table,
    data.frame(id=7L,price="12.3456",small="214748.3647",
               exact="1.0000000001")), 1)
  appended <- DBI::dbGetQuery(con, paste(
    "SELECT CONVERT(varchar(48),price,2) AS price,",
    "CONVERT(varchar(48),small,2) AS small,",
    "CAST(exact AS varchar(48)) AS exact FROM", table, "WHERE id=7"
  ))
  testthat::expect_identical(as.character(appended$price),"12.3456")
  testthat::expect_identical(as.character(appended$small),"214748.3647")
  testthat::expect_identical(as.character(appended$exact),"1.0000000001")
  testthat::expect_equal(DBI::dbAppendTable(con, table,
    data.frame(id = 8L, price = NA_character_, small = "-214748.3648",
               exact = NA_character_)), 1)
  appended_lower <- DBI::dbGetQuery(con, paste(
    "SELECT CONVERT(varchar(48),small,2) AS small, price, exact FROM",
    table, "WHERE id=8"
  ))
  testthat::expect_identical(as.character(appended_lower$small), "-214748.3648")
  testthat::expect_true(is.na(appended_lower$price))
  testthat::expect_true(is.na(appended_lower$exact))
  testthat::expect_true(DBI::dbRemoveTable(con,table))
  testthat::expect_true(DBI::dbIsValid(con))
})
