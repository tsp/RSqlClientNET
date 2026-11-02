# Compatibility adapter for the existing regression scripts.
# The scripts live under tests/legacy so R CMD check does not execute them
# again outside testthat. Each script runs in a fresh source environment.
.rsqlclientnet_run_legacy <- function(filename) {
  script <- testthat::test_path("..", "legacy", filename)
  if (!file.exists(script)) {
    stop("Missing legacy test script: ", filename, call. = FALSE)
  }
  # One old test mutates this process-wide setting; restore it afterwards.
  withr::local_envvar(
    RSQLCLIENTNET_PIPE_POLL_MS = Sys.getenv("RSQLCLIENTNET_PIPE_POLL_MS", unset = NA_character_)
  )
  script_env <- new.env(parent = globalenv())
  # Developer helpers have been intentionally unexported in v0.0.32.
  # Legacy scripts still exercise the exact same implementations.
  developer <- c("sql_memory_test", "sql_types_test", "sql_edge_types_test",
    "sql_edge_types_report", "sql_timeout_test", "sql_stream_timeout_test",
    "sql_robustness_test", "sql_stress_test", "sql_stream_stress_test",
    "sql_stream_integrity_test", "sql_batch_benchmark", "sql_batch_summary")
  for (name in developer)
    assign(name, get(name, envir=asNamespace("RSqlClientNET")), envir=script_env)
  withCallingHandlers(
    source(script, local = script_env, chdir = FALSE, encoding = "UTF-8"),
    warning = function(w) {
      # These legacy fixtures sometimes call library(DBI). A test runner on
      # R 4.4.1 with DBI built under R 4.4.3 prints an environment-only
      # warning; keep all other warnings visible to the test framework.
      msg <- conditionMessage(w)
      if (grepl("^package .+ was built under R version ", msg) &&
          grepl("DBI", msg, fixed = TRUE))
        invokeRestart("muffleWarning")
    }
  )
  invisible(TRUE)
}

.rsqlclientnet_skip_no_dotnet <- function() {
  testthat::skip_if(
    !nzchar(Sys.which("dotnet")),
    ".NET SDK unavailable: helper integration test"
  )
  # The helper is built with dotnet publish; a runtime alone is insufficient.
  sdk <- tryCatch(
    suppressWarnings(system2(Sys.which("dotnet"), "--list-sdks",
                             stdout = TRUE, stderr = TRUE)),
    error = function(e) character()
  )
  majors <- suppressWarnings(as.integer(sub("^([0-9]+)\\..*$", "\\1", sdk)))
  testthat::skip_if(
    !any(!is.na(majors) & majors >= 8L),
    ".NET SDK 8+ unavailable: helper integration test"
  )
}

.rsqlclientnet_skip_no_sql <- function() {
  testthat::skip_if(
    !nzchar(Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", unset = "")),
    "RSQLCLIENTNET_TEST_CONNECTION_STRING not set"
  )
  .rsqlclientnet_skip_no_dotnet()
}
