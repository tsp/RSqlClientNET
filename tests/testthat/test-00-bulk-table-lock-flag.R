testthat::test_that("TableLock is a strict per-operation boolean", {
  validate <- RSqlClientNET:::.dbi_validate_table_lock
  testthat::expect_identical(validate(FALSE), FALSE)
  testthat::expect_identical(validate(TRUE), TRUE)
  for (x in list(NULL, NA, NA_integer_, 0, 1L, "TRUE", c(TRUE,FALSE), list(TRUE)))
    testthat::expect_error(validate(x), "table_lock must be TRUE or FALSE")
})

testthat::test_that("TableLock is forwarded by DBI write and append methods", {
  # S4 may normalize selected MethodDefinition formals to the DBI generic.
  # Custom options are allowed through ...; the public SQL integration test
  # exercises both DBI entry points with table_lock=TRUE and FALSE.
  m <- methods::selectMethod("dbWriteTable", c("SqlClientNETConnection", "character"))
  n <- methods::selectMethod("dbAppendTable", c("SqlClientNETConnection", "character"))
  testthat::expect_false(is.null(m))
  testthat::expect_false(is.null(n))
  testthat::expect_true("..." %in% names(formals(m)))
  testthat::expect_true("..." %in% names(formals(n)))
  testthat::expect_true("table_lock" %in% names(formals(RSqlClientNET:::.dbi_write_table)))
  testthat::expect_true("table_lock" %in% names(formals(RSqlClientNET:::.dbi_append_table)))
  testthat::expect_true("table_lock" %in% names(formals(RSqlClientNET:::.dbi_bulk_upload)))
})

# Public DBI methods should agree with the internal upload path.
testthat::test_that("TableLock defaults to TRUE in every write implementation", {
  for (fn in c(".dbi_bulk_upload", ".dbi_write_table", ".dbi_append_table")) {
    actual <- formals(get(fn, envir = asNamespace("RSqlClientNET")))$table_lock
    testthat::expect_identical(actual, TRUE, info = fn)
  }
  # FALSE must remain an explicit supported override, not a removed option.
  testthat::expect_identical(RSqlClientNET:::.dbi_validate_table_lock(FALSE), FALSE)
})
