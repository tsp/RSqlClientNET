# SQL-free behavioural tests for benchmark table reuse.
# A DBI test connection records physical write/read/drop calls; the benchmark
# still runs through its real plan, timings, result and cleanup logic.
testthat::test_that("benchmark_dbi reuses the last timed write for reads", {
  paths <- c(file.path("tools", "benchmarks", "benchmark_dbi.R"),
             file.path("..", "..", "tools", "benchmarks", "benchmark_dbi.R"))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) testthat::skip("Benchmark script is not in the installed package")
  benchmark <- new.env(parent = baseenv())
  sys.source(paths[[1L]], envir = benchmark)

  class_name <- "SqlClientNETBenchmarkMockConnection03448"
  if (!methods::isClass(class_name))
    methods::setClass(class_name, contains = "DBIConnection",
                      slots = c(state = "environment"))
  methods::setMethod("dbIsValid", class_name, function(dbObj, ...) dbObj@state$valid)
  # DBI provides DBIConnection#character methods. Matching only the mock
  # connection class here leaves S4 dispatch ambiguous (notably on Linux).
  # Match both arguments so the mock read/quote/drop methods win reliably.
  methods::setMethod("dbQuoteIdentifier",
    signature(conn = class_name, x = "character"), function(conn, x, ...)
    DBI::SQL(paste0("[", as.character(x), "]")))
  methods::setMethod("dbWriteTable", class_name,
    function(conn, name, value, ...) {
      state <- conn@state
      state$writes <- c(state$writes, as.character(name))
      if (isTRUE(state$fail_next_write) ||
          identical(as.integer(length(state$writes)), state$fail_on_write)) {
        state$fail_next_write <- FALSE
        stop("Mock write failure")
      }
      assign(as.character(name), value, envir = state$tables)
      Sys.sleep(0.003)  # Ensure a nonzero elapsed time on coarse clocks.
      TRUE
    })
  methods::setMethod("dbReadTable",
    signature(conn = class_name, name = "character"),
    function(conn, name, ...) {
      state <- conn@state
      name <- as.character(name)
      state$reads <- c(state$reads, name)
      if (!exists(name, envir = state$tables, inherits = FALSE))
        stop("Read fixture is missing: ", name)
      Sys.sleep(0.003)
      get(name, envir = state$tables, inherits = FALSE)
    })
  methods::setMethod("dbExecute",
    signature(conn = class_name, statement = "character"),
    function(conn, statement, ...) {
      state <- conn@state
      statement <- as.character(statement)
      if (!grepl("^DROP TABLE IF EXISTS \\[.*\\]$", statement))
        stop("Unexpected mock SQL: ", statement)
      name <- sub("^DROP TABLE IF EXISTS \\[(.*)\\]$", "\\1", statement)
      state$drops <- c(state$drops, name)
      if (exists(name, envir = state$tables, inherits = FALSE))
        rm(list = name, envir = state$tables)
      0L
    })

  mock <- function(fail_first_write = FALSE) {
    state <- new.env(parent = emptyenv())
    state$valid <- TRUE
    state$tables <- new.env(parent = emptyenv())
    state$writes <- state$reads <- state$drops <- character()
    state$fail_next_write <- fail_first_write
    state$fail_on_write <- NA_integer_
    list(con = methods::new(class_name, state = state), state = state)
  }
  run <- function(conn, w, r, nw = 11L, nr = 11L, ...) {
    benchmark$benchmark_dbi(
      con = list(mock = conn), types = "integer", null_fractions = 0,
      max_case_payload_mb = 1, n_write = nw, n_read = nr,
      repetitions_write = w, repetitions_read = r, verbose = FALSE, ...)
  }

  a <- mock()
  result <- run(a$con, w = 1L, r = 3L)
  testthat::expect_equal(length(a$state$writes), 1L)
  testthat::expect_equal(length(a$state$reads), 3L)
  testthat::expect_true(all(a$state$reads == a$state$writes[1L]))
  testthat::expect_equal(a$state$drops, a$state$writes)
  testthat::expect_equal(length(ls(a$state$tables)), 0L)
  testthat::expect_true(all(result$details$status == "ok"))
  testthat::expect_equal(result$summary$write_ok, 1L)
  testthat::expect_equal(result$summary$read_ok, 3L)
  # Very short successful reads/writes may measure 0 s on Windows.
  # Such operations must still count as successes in the summary.
  zero_clock <- result$details
  zero_clock$write_s[zero_clock$operation == "write"] <- 0
  zero_clock$read_s[which(zero_clock$operation == "read")[1L]] <- 0
  zero_summary <- benchmark$benchmark_summary(zero_clock)
  testthat::expect_equal(zero_summary$write_ok, 1L)
  testthat::expect_equal(zero_summary$read_ok, 3L)

  b <- mock()
  run(b$con, w = 2L, r = 2L)
  testthat::expect_equal(length(b$state$writes), 2L)
  testthat::expect_equal(length(b$state$reads), 2L)
  testthat::expect_true(all(b$state$reads == tail(b$state$writes, 1L)))
  testthat::expect_equal(length(b$state$drops), 2L)
  testthat::expect_equal(length(ls(b$state$tables)), 0L)

  read_only <- mock()
  out <- run(read_only$con, w = 0L, r = 2L)
  testthat::expect_equal(length(read_only$state$writes), 1L)
  testthat::expect_equal(out$summary$write_runs, 0L)
  testthat::expect_equal(length(read_only$state$reads), 2L)
  testthat::expect_true(all(read_only$state$reads == read_only$state$writes))

  unequal <- mock()
  run(unequal$con, w = 1L, r = 2L, nw = 9L, nr = 11L)
  testthat::expect_equal(length(unequal$state$writes), 2L)
  testthat::expect_true(all(unequal$state$reads == tail(unequal$state$writes, 1L)))
  testthat::expect_false(identical(unequal$state$writes[1L],
                                  unequal$state$writes[2L]))
  testthat::expect_equal(length(ls(unequal$state$tables)), 0L)

  failed <- mock(fail_first_write = TRUE)
  out <- run(failed$con, w = 1L, r = 2L)
  testthat::expect_equal(out$details$status,
                         c("write_error", "ok", "ok"))
  testthat::expect_equal(length(failed$state$writes), 2L)
  testthat::expect_true(all(failed$state$reads == tail(failed$state$writes, 1L)))
  testthat::expect_equal(length(ls(failed$state$tables)), 0L)

  late_failure <- mock()
  late_failure$state$fail_on_write <- 2L
  out <- run(late_failure$con, w = 2L, r = 2L)
  testthat::expect_equal(out$details$status,
                         c("ok", "write_error", "ok", "ok"))
  testthat::expect_true(all(late_failure$state$reads ==
                            late_failure$state$writes[1L]))
  testthat::expect_equal(length(ls(late_failure$state$tables)), 0L)

  # Checkpoint files cannot preserve a SQL Server session-local table.
  # If only an earlier write result is resumed, reads must stage a fresh one.
  folder <- tempfile("benchmark_reuse_03448_")
  on.exit(unlink(folder, recursive = TRUE), add = TRUE)
  before <- mock()
  first <- run(before$con, w = 1L, r = 2L, run_dir = folder)
  utils::write.csv(first$details[first$details$operation == "write", , drop = FALSE],
                   file.path(folder, "details.csv"), row.names = FALSE, na = "")
  after <- mock()
  resumed <- run(after$con, w = 1L, r = 2L, run_dir = folder, resume = TRUE)
  testthat::expect_equal(length(after$state$writes), 1L)
  testthat::expect_true(all(after$state$reads == after$state$writes))
  testthat::expect_equal(sum(resumed$details$operation == "write"), 1L)
  testthat::expect_equal(sum(resumed$details$operation == "read"), 2L)
})
