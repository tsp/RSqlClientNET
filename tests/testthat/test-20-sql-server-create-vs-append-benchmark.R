testthat::test_that("create versus append benchmark completes and checks stored bytes", {
  .rsqlclientnet_skip_no_sql()
  if (Sys.getenv("RSQLCLIENTNET_RUN_CREATE_APPEND_TESTS") != "1")
    testthat::skip("Set RSQLCLIENTNET_RUN_CREATE_APPEND_TESTS=1 for integration benchmark")
  file <- file.path("tools","benchmarks","benchmark-create-vs-append.R")
  if (!file.exists(file)) file <- file.path("..","..",file)
  if (!file.exists(file)) testthat::skip("benchmark source not available")
  env <- new.env(parent=globalenv())
  sys.source(file,envir=env)
  dest <- tempfile("ca35-results-")
  out <- env$run_create_append_benchmark(n=40L,repetitions=1L,text_length=100L,
    connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    logical_log=TRUE,output_dir=dest)
  testthat::expect_identical(nrow(out$summary),6L)
  testthat::expect_true(all(is.finite(out$attempts$total_s)))
  # Every one of the six cases must be attempted independently. A lack of
  # server DMV permissions may produce NA measurements, but never invalidate
  # the main timing connection or abort the benchmark.
  testthat::expect_identical(nrow(out$logical_log),6L)
  testthat::expect_true(all(out$logical_log$status %in%
    c("ok","dmv_unavailable")))
  testthat::expect_true(file.exists(file.path(dest,"create-append-summary.csv")))
  testthat::expect_true(file.exists(file.path(dest,"create-append-logical-log.csv")))
})
