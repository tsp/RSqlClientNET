test_that("SQL Server: SQL BIGINT conversions and NA quoting behave as DBI expects", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    bigint = "integer64", tz = "UTC")
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  # SQL bigint and SQL untyped decimal literals are distinct in T-SQL.
  x <- DBI::dbGetQuery(con, paste0(
    "SELECT CAST(336776 AS BIGINT) AS pos, ",
    "CAST(-9223372036854775807 AS BIGINT) AS neg"))
  expect_true(bit64::is.integer64(x$pos))
  expect_identical(as.character(x$pos), "336776")
  expect_identical(as.character(x$neg), "-9223372036854775807")

  # SQL Server promotes bare 10000000000 literals to DECIMAL, which does not
  # activate the SQL BIGINT conversion option in any DBI backend.
  bare <- DBI::dbGetQuery(con, "SELECT 10000000000 AS n")
  expect_type(bare$n, "double")

  typed_sql <- "SELECT CAST(10000000000 AS BIGINT) AS n"
  as_text <- DBI::dbGetQuery(con, typed_sql, bigint = "character")
  expect_identical(as_text$n, "10000000000")
  as_num <- DBI::dbGetQuery(con, typed_sql, bigint = "numeric")
  expect_identical(as_num$n, 1e10)
  as_int64 <- DBI::dbGetQuery(con, typed_sql, bigint = "integer64")
  expect_true(bit64::is.integer64(as_int64$n))
  expect_identical(as.character(as_int64$n), "10000000000")
  as_int <- DBI::dbGetQuery(con, "SELECT CAST(336776 AS BIGINT) AS n",
                            bigint = "integer")
  expect_identical(as_int$n, 336776L)

  for (quoted in list(DBI::dbQuoteString(con, NA_character_),
                      DBI::dbQuoteLiteral(con, NA_character_),
                      DBI::dbQuoteLiteral(con, NA_integer_))) {
    # The SQL expression must evaluate to NULL; avoid requiring one specific SQL literal spelling.
    z <- DBI::dbGetQuery(con, paste0("SELECT CASE WHEN ", quoted,
                                    " IS NULL THEN 1 ELSE 0 END AS ok"))
    expect_identical(as.integer(z$ok), 1L)
  }
})
