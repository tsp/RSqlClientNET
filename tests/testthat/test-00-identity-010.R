testthat::test_that("public driver API and S4 types use new names", {
  exports <- getNamespaceExports("RSqlClientNET")
  testthat::expect_true("SqlClientNET" %in% exports)
  testthat::expect_false("SQLServer" %in% exports)
  driver <- RSqlClientNET::SqlClientNET()
  testthat::expect_true(methods::is(driver, "SqlClientNETDriver"))
  testthat::expect_true(methods::is(driver, "DBIDriver"))
  testthat::expect_true(methods::isClass("SqlClientNETConnection"))
  testthat::expect_true(methods::isClass("SqlClientNETResult"))
})

testthat::test_that("native DLL is registered under new package name", {
  dlls <- getLoadedDLLs()
  testthat::expect_true("RSqlClientNET" %in% names(dlls))
  testthat::expect_true(is.loaded("sqlcpp_start", PACKAGE = "RSqlClientNET"))
})
