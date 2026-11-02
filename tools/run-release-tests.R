# Executed from the package source directory, against a disposable test DB.
run_RSqlClientNET_release_tests <- function() {
  if (!nzchar(Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")))
    stop("Set RSQLCLIENTNET_TEST_CONNECTION_STRING to a disposable SQL Server database")
  source("tools/ensure-deps.R", local = TRUE)
  rscn_ensure_dev_deps("release")
  if (!requireNamespace("devtools", quietly = TRUE))
    stop("Install devtools before running release tests")
  old <- Sys.getenv(c("RSQLCLIENTNET_RUN_DBITEST", "RSQLCLIENTNET_DBITEST_SCOPE"), unset = NA_character_)
  on.exit({
    for (key in names(old)) {
      if (is.na(old[[key]])) Sys.unsetenv(key)
      else do.call(Sys.setenv, stats::setNames(list(old[[key]]),key))
    }
  }, add = TRUE)

  # Ordinary testthat regressions, INCLUDING SQL Server integration tests.
  Sys.setenv(RSQLCLIENTNET_RUN_DBITEST = "0")
  message("1/3: Running all regression tests with SQL Server integration enabled")
  devtools::test(stop_on_failure = TRUE)

  if (!requireNamespace("DBItest", quietly = TRUE))
    stop("Install DBItest to run the official conformance suite")
  installed_dbitest <- utils::packageVersion("DBItest")
  if (installed_dbitest < package_version("1.8.3")) {
    stop("DBItest >= 1.8.3 is required for the full release test suite; ",
         "installed: ", as.character(installed_dbitest), ". ",
         "Run install.packages('DBItest') and restart R.")
  }
  message("Using DBItest ", as.character(installed_dbitest),
          " with explicit compatibility target 1.8.3 and @name placeholders")
  # The raw upstream tests rely on T-SQL-incompatible queries:
  # SELECT 10000000000 is DECIMAL, not BIGINT; FROM (SELECT 1) a has
  # an unnamed derived column. Check equivalent SQL Server SQL instead.
  message("2/3: Running SQL Server dialect equivalents for BIGINT and NULL")
  devtools::test(filter = "20-sql-server-dbi-conformance-03419", stop_on_failure = TRUE)

  Sys.setenv(RSQLCLIENTNET_RUN_DBITEST = "1", RSQLCLIENTNET_DBITEST_SCOPE = "all")
  message("3/3: Running DBItest 1.8.3 conformance, including named bind tests")
  devtools::test(filter = "30-dbitest", stop_on_failure = TRUE)
  message("All three test stages completed without errors")
  invisible(TRUE)
}
run_RSqlClientNET_release_tests()
