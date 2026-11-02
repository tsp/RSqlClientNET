testthat::test_that("declared S3 methods exist in the loaded namespace", {
  ns <- asNamespace("RSqlClientNET")
  expected <- c("print.sqlclient_connection",
                "sql_dialect.SqlClientNETConnection",
                "dbplyr_edition.SqlClientNETConnection")
  for (nm in expected) {
    testthat::expect_true(exists(nm, envir = ns, inherits = FALSE), info = nm)
    testthat::expect_true(is.function(get(nm, envir = ns, inherits = FALSE)), info = nm)
  }
})
