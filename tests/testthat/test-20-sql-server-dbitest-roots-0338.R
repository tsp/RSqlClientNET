# Live SQL Server tests: exercise the cases incompatible with DBItest's
# fixed ANSI SQL literal assumptions or session lifecycle test harness.
testthat::test_that("DBI SQL Server BIGINT exact boundaries and real temp scope", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs,
                         bigint="integer64")
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add=TRUE)
  # These are typed BIGINT, *not* SQL Server's untyped DECIMAL literals.
  sql <- paste0("SELECT CAST(9223372036854775807 AS BIGINT) AS hi, ",
                "CAST(-9223372036854775807 AS BIGINT) AS lo")
  z <- DBI::dbGetQuery(con,sql)
  testthat::expect_s3_class(z$hi,"integer64")
  testthat::expect_s3_class(z$lo,"integer64")
  testthat::expect_identical(as.character(z$hi),"9223372036854775807")
  testthat::expect_identical(as.character(z$lo),"-9223372036854775807")
  # The lowest BIGINT bit pattern is reserved as NA in bit64, so request
  # lossless text if callers need -2^63.
  cc <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs,
                        bigint="character")
  on.exit(if (DBI::dbIsValid(cc)) DBI::dbDisconnect(cc), add=TRUE)
  min_val <- DBI::dbGetQuery(cc,
    "SELECT CAST('-9223372036854775808' AS BIGINT) AS min_val")
  testthat::expect_identical(min_val$min_val,"-9223372036854775808")
  testthat::expect_warning(
    min_i64 <- DBI::dbGetQuery(con,
      "SELECT CAST('-9223372036854775808' AS BIGINT) AS min_val"),
    "converted to NA")
  testthat::expect_true(is.na(min_i64$min_val[[1L]]))
  # Factor metadata must not cause dbWriteTable() to warn. Conversely,
  # dbAppendTable() warns because it discards factor levels.
  testthat::expect_silent(DBI::dbWriteTable(con,"#factor0338",
                        data.frame(a=factor("first"))))
  testthat::expect_identical(DBI::dbReadTable(con,"#factor0338")$a,"first")
  testthat::expect_warning(DBI::dbAppendTable(con,"#factor0338",
                            data.frame(a=factor("second"))),"Factor columns")
  DBI::dbRemoveTable(con,"#factor0338")
})

testthat::test_that("DBI dbRemoveTable temporary=TRUE respects permanent tables", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs)
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add=TRUE)
  name <- "dbi0338_permanent_and_temporary"
  # Use an ordinary table with same logical name as the #local table.
  if (DBI::dbExistsTable(con,name)) DBI::dbRemoveTable(con,name)
  on.exit(if (DBI::dbIsValid(con) && DBI::dbExistsTable(con,name))
            DBI::dbRemoveTable(con,name), add=TRUE)
  DBI::dbWriteTable(con,name,data.frame(a=1.5))
  DBI::dbCreateTable(con,name,data.frame(b=2.5),temporary=TRUE)
  testthat::expect_true(DBI::dbRemoveTable(con,name,temporary=TRUE))
  testthat::expect_error(DBI::dbRemoveTable(con,name,temporary=TRUE),"does not exist")
  testthat::expect_identical(DBI::dbReadTable(con,name)$a,1.5)
  DBI::dbRemoveTable(con,name)
})

testthat::test_that("DBI session-local tables vanish on disconnect, not between statements", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  c1 <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs)
  on.exit(if (DBI::dbIsValid(c1)) DBI::dbDisconnect(c1), add=TRUE)
  c2 <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs)
  on.exit(if (DBI::dbIsValid(c2)) DBI::dbDisconnect(c2), add=TRUE)
  name <- "dbi0338_session_lifetime"
  DBI::dbCreateTable(c1,name,data.frame(a=1L),temporary=TRUE)
  testthat::expect_identical(names(DBI::dbReadTable(c1,name)),"a")
  testthat::expect_error(DBI::dbReadTable(c2,name))
  testthat::expect_true(DBI::dbExistsTable(c1,name))
  DBI::dbDisconnect(c1)
  c3 <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs)
  on.exit(if (DBI::dbIsValid(c3)) DBI::dbDisconnect(c3), add=TRUE)
  testthat::expect_false(DBI::dbExistsTable(c3,name))
  testthat::expect_error(DBI::dbReadTable(c3,name))
})
