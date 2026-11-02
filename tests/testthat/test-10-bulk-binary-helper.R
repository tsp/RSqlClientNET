testthat::test_that(".NET SBIN bulk decoder self-test (offline, no SQL Server)", {
  .rsqlclientnet_skip_no_dotnet()
  dll <- RSqlClientNET::sqlclient_build()
  result <- processx::run(Sys.which("dotnet"),
                          c(dll,"--bulk-binary-self-test"),
                          echo=FALSE, error_on_status=TRUE)
  reply <- jsonlite::fromJSON(result$stdout)
  testthat::expect_true(reply$ok)
  testthat::expect_identical(reply$rows,3L)
})
