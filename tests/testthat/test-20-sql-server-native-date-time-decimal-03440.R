testthat::test_that("native DATE TIME DECIMAL and MONEY write to SQL Server without text formatting", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),tz="UTC")
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con),add=TRUE)
  tab <- "#sbin_native_date_time_dec_03440"
  value <- data.frame(
    id=1:5,
    day=as.Date(c("1970-01-01","1900-02-28",NA,"2000-02-29","9999-12-31")),
    clock=as.difftime(c(0, 0.1234567,NA, 43200.5,86399.9999999),units="secs"),
    amount=c(123.4567, NA_real_, -9.5, 0, 17.125),
    price=c(123.4567, NA_real_, -9.5, 0, 17.125),
    small=c(123.4567, NA_real_, -9.5, 214748.3647,-214748.3648),
    exact=c("1234567890123456789012345678.1234567890", NA_character_,
            "-9.5000000000", "0.0000000000", "17.1250000000")
  )
  testthat::expect_true(DBI::dbWriteTable(con,tab,value,batch_rows=2L,
    field.types=c(id="INT",day="DATE",clock="TIME(7)",amount="DECIMAL(18,4)",
                  price="MONEY",small="SMALLMONEY",exact="DECIMAL(38,10)")))
  query <- paste("SELECT id, CAST(day AS varchar(10)) AS d,",
    "CAST(clock AS varchar(20)) AS t, CAST(amount AS varchar(30)) AS a,",
    "CONVERT(varchar(40),price,2) AS p, CONVERT(varchar(40),small,2) AS m,",
    "CAST(exact AS varchar(45)) AS e FROM",tab,"ORDER BY id")
  got <- DBI::dbGetQuery(con,query)
  testthat::expect_identical(as.character(got$d[c(1L,2L,4L,5L)]),
     c("1970-01-01","1900-02-28","2000-02-29","9999-12-31"))
  testthat::expect_true(is.na(got$d[3L]))
  testthat::expect_identical(as.character(got$t[c(1L,2L,4L,5L)]),
     c("00:00:00.0000000","00:00:00.1234567","12:00:00.5000000","23:59:59.9999999"))
  testthat::expect_true(is.na(got$t[3L]))
  testthat::expect_identical(as.character(got$p[c(1L,3L,4L,5L)]),
     c("123.4567","-9.5000","0.0000","17.1250"))
  testthat::expect_identical(as.character(got$m[c(4L,5L)]),
     c("214748.3647","-214748.3648"))
  testthat::expect_identical(as.character(got$e[1L]),
     "1234567890123456789012345678.1234567890")
  testthat::expect_true(all(is.na(got[2L,c("a","p","m","e")])))
  testthat::expect_true(DBI::dbIsValid(con))
  # On append, inferred R numeric types are FLOAT, but the actual SQL
  # destination columns are DECIMAL and MONEY. Check destination-aware decode.
  appended <- data.frame(id=6L, day=as.Date("2020-01-02"),
    clock=as.difftime(0.25, units="secs"), amount=12.5,
    price=12.5, small=12.5, exact=NA_character_)
  testthat::expect_equal(DBI::dbAppendTable(con,tab,appended),1)
  a <- DBI::dbGetQuery(con,paste(
    "SELECT CAST(day AS varchar(10)) AS d, CAST(clock AS varchar(20)) AS t,",
    "CAST(amount AS varchar(30)) AS a, CONVERT(varchar(40),price,2) AS p,",
    "CONVERT(varchar(40),small,2) AS m FROM",tab,"WHERE id=6"))
  testthat::expect_identical(as.character(a$d),"2020-01-02")
  testthat::expect_identical(as.character(a$t),"00:00:00.2500000")
  testthat::expect_identical(as.character(a$p),"12.5000")
  testthat::expect_identical(as.character(a$m),"12.5000")
  testthat::expect_true(DBI::dbIsValid(con))
  testthat::expect_true(DBI::dbRemoveTable(con,tab))
})
