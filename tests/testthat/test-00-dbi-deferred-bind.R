testthat::test_that("DBI default immediate mode defers only recoverable SQL 137", {
  f <- getFromNamespace(".dbi_defer_on_unbound_parameter", "RSqlClientNET")
  e <- simpleError("Must declare the scalar variable @a")
  e$sql_number <- 137L
  class(e) <- c("sqlclient_recoverable_sql_error", class(e))
  expect_true(f(e, "SELECT @a AS a", NULL, NULL))
  expect_false(f(e, "SELECT @a AS a", NULL, TRUE))
  expect_false(f(e, "SELECT @a AS a", list(a=1L), NULL))
  expect_false(f(e, "SELECT @@ROWCOUNT", NULL, NULL))
  e$sql_number <- 102L
  expect_false(f(e, "SELECT @a AS a", NULL, NULL))
  e$sql_number <- 137L
  class(e) <- class(e)[-1L]
  expect_false(f(e, "SELECT @a AS a", NULL, NULL))
})

testthat::test_that("DBI recognizes named binds but not SQL comments or strings", {
  extract <- getFromNamespace(".dbi_sql_param_names", "RSqlClientNET")
  expect_identical(extract("SELECT @a, @b, @a, @@ROWCOUNT"), c("a", "b"))
  expect_identical(extract("SELECT '@fake', [@fake], @actual -- @ignored"), "actual")
  check <- getFromNamespace(".dbi_check_bound_names", "RSqlClientNET")
  expect_silent(check("SELECT @a", list(a=1L)))
  expect_error(check("SELECT @a", list(a=1L, bogus=2L)), "Unexpected DBI binding")
  expect_silent(check("EXEC dbo.proc @arg=@a", list(a=1L)))
})
