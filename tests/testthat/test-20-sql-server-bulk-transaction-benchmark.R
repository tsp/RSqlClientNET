testthat::test_that("RC34 same-table transaction benchmark and logical log diagnostic", {
  testthat::skip_if(Sys.getenv("RSQLCLIENTNET_RUN_BULK_TX_TESTS", "") != "1",
                    "Run only with RSQLCLIENTNET_RUN_BULK_TX_TESTS=1")
  if (!nzchar(Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")))
    testthat::skip("SQL Server test connection missing")
  testthat::skip_if_not_installed("odbc")
  testthat::skip_if_not_installed("RSqlClientNET")
  script <- file.path("tools", "benchmarks", "benchmark-bulk-transactions.R")
  if (!file.exists(script)) script <- file.path("..", "..", script)
  if (!file.exists(script)) testthat::skip("Source-tree benchmark unavailable")
  # Script expects the source-tree working directory; testthat runs under
  # tests/testthat in some contexts. Temporarily change, then restore.
  root <- normalizePath(file.path(dirname(script), "..", ".."),
                        winslash="/", mustWork=TRUE)
  old <- getwd()
  on.exit(setwd(old), add=TRUE)
  setwd(root)
  env <- new.env(parent=globalenv())
  sys.source("tools/benchmarks/benchmark-bulk-transactions.R", envir=env)
  output <- tempfile("rscn-rc34-")
  res <- env$run_bulk_transaction_benchmark(
    n=40L, repetitions=1L,types="varchar",text_length=64L,
    null_fractions=0.25,server_diagnostics=FALSE,logical_log=TRUE,
    profile_separately=FALSE,
    connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    output_dir=output)
  testthat::expect_equal(nrow(res$summary),4L)
  testthat::expect_equal(nrow(res$attempts),4L)
  testthat::expect_true(all(res$attempts$status=="ok"))
  testthat::expect_equal(sort(res$summary$mode),sort(c(
    "rsql_standard", "rsql_table_lock", "odbc_default", "odbc_transaction")))
  testthat::expect_equal(nrow(res$logical_log),3L)
  testthat::expect_true(file.exists(file.path(output,"bulk-tx-summary.csv")))
})
