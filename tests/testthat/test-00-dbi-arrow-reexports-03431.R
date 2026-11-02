testthat::test_that("DBI Arrow generics are available from the installed namespace", {
  expected <- c("dbAppendTableArrow", "dbBindArrow", "dbCreateTableArrow",
                "dbFetchArrow", "dbFetchArrowChunk", "dbGetQueryArrow",
                "dbReadTableArrow", "dbSendQueryArrow", "dbWriteTableArrow")
  exported <- getNamespaceExports("RSqlClientNET")
  testthat::expect_true(all(expected %in% exported),
                       info = paste(setdiff(expected, exported), collapse = ", "))
  for (nm in expected) {
    testthat::expect_true(is.function(getExportedValue("RSqlClientNET", nm)),
                          info = nm)
  }
})
