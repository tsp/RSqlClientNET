testthat::test_that("DBI fetch NA means all rows but other logical limits are invalid", {
  n <- getFromNamespace(".dbi_n", "RSqlClientNET")
  expect_identical(n(NA), -1L)
  expect_identical(n(NA_real_), -1L)
  expect_identical(n(Inf), -1L)
  expect_identical(n(-1L), -1L)
  expect_identical(n(0L), 0L)
  expect_identical(n(10L), 10L)
  expect_error(n(TRUE), "n must")
  expect_error(n(FALSE), "n must")
  expect_error(n(NaN), "n must")
  expect_error(n(-2L), "n must")
})

testthat::test_that("Unnamed SQL expressions use matching metadata and result names", {
  normalize <- getFromNamespace(".dbi_result_names", "RSqlClientNET")
  expect_identical(normalize(c("", "a", "", "")), c("V1", "a", "V2", "V3"))
  expect_identical(normalize(c("", "V1", "")), c("V2", "V1", "V3"))
  expect_identical(normalize(c("id", "x")), c("id", "x"))
  expect_identical(normalize(character()), character())
})

testthat::test_that("DBI Arrow safe SQL identifier can create local temp table", {
  target <- getFromNamespace(".dbi_temp_target", "RSqlClientNET")
  fake <- new.env(parent=emptyenv())
  class(fake) <- "sqlclient_connection"
  fake$dbi_temp_aliases <- new.env(parent=emptyenv())
  con <- methods::new("SqlClientNETConnection", handle=fake)
  expect_identical(target(con, DBI::SQL("[dbit08]"), TRUE), "#dbit08")
  expect_identical(target(con, DBI::SQL("[#dbit08]"), TRUE), DBI::SQL("[#dbit08]"))
  expect_error(target(con, DBI::SQL("[dbo].[evil]"), TRUE), "safely quoted")
  expect_error(target(con, DBI::SQL("[x]; DROP TABLE xyz"), TRUE), "safely quoted")
})


testthat::test_that("Quoted Arrow table names resolve only registered local #temp aliases", {
  register <- getFromNamespace(".dbi_register_temp", "RSqlClientNET")
  resolve <- getFromNamespace(".dbi_temp_resolve", "RSqlClientNET")
  forget <- getFromNamespace(".dbi_forget_temp", "RSqlClientNET")
  fake <- new.env(parent=emptyenv())
  class(fake) <- "sqlclient_connection"
  fake$dbi_temp_aliases <- new.env(parent=emptyenv())
  fake$dbi_temp_names <- character()
  con <- methods::new("SqlClientNETConnection", handle=fake)
  quoted <- DBI::SQL("[dbit08]")
  # Without an alias, regular SQL identifiers must remain untouched.
  expect_identical(resolve(con,quoted), quoted)
  register(con,quoted,"#dbit08",temporary=TRUE)
  expect_identical(resolve(con,quoted), "#dbit08")
  expect_identical(resolve(con,"dbit08"), "#dbit08")
  expect_identical(resolve(con,DBI::SQL("[dbo].[dbit08]")), DBI::SQL("[dbo].[dbit08]"))
  expect_identical(resolve(con,DBI::SQL("[dbit08]; DROP TABLE x")),
                   DBI::SQL("[dbit08]; DROP TABLE x"))
  expect_identical(resolve(con,DBI::SQL("[#dbit08]")), DBI::SQL("[#dbit08]"))
  forget(con,"#dbit08")
  expect_identical(resolve(con,quoted), quoted)
})
