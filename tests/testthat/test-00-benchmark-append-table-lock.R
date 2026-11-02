# SQL-free coverage for overnight benchmark's additional append variants.
testthat::test_that("overnight benchmark isolates append and TableLock from timed CREATE", {
  paths <- c(file.path("tools", "benchmarks", "benchmark_dbi.R"),
             file.path("..", "..", "tools", "benchmarks", "benchmark_dbi.R"))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) testthat::skip("Benchmark script is not in the installed package")
  bm <- new.env(parent = baseenv())
  sys.source(paths[[1L]], envir = bm)

  cname <- "SqlClientNETBenchmarkAppendMock035a"
  if (!methods::isClass(cname))
    methods::setClass(cname, contains = "DBIConnection", slots = c(state = "environment"))
  methods::setMethod("dbIsValid", cname, function(dbObj, ...) TRUE)
  methods::setMethod("dbQuoteIdentifier", signature(conn = cname, x = "character"),
    function(conn, x, ...) DBI::SQL(paste0("[", x, "]")))
  methods::setMethod("dbWriteTable", signature(conn = cname, name = "character"),
    function(conn, name, value, ...) {
      conn@state$created <- c(conn@state$created, name)
      assign(name, value, envir = conn@state$tables)
      Sys.sleep(0.003)
      invisible(TRUE)
    })
  methods::setMethod("dbCreateTable", signature(conn = cname, name = "character"),
    function(conn, name, fields, ..., row.names = NULL, temporary = FALSE) {
      conn@state$created <- c(conn@state$created, name)
      assign(name, data.frame(value = integer()), envir = conn@state$tables)
      invisible(TRUE)
    })
  methods::setMethod("dbAppendTable", signature(conn = cname, name = "character"),
    function(conn, name, value, ..., row.names = NULL) {
      opts <- list(...)
      if (length(opts)) testthat::expect_identical(names(opts), "table_lock")
      conn@state$locks <- c(conn@state$locks,
                            if ("table_lock" %in% names(opts)) opts$table_lock else FALSE)
      if (!exists(name, envir = conn@state$tables, inherits = FALSE))
        stop("Append attempted without CREATE TABLE")
      assign(name, value, envir = conn@state$tables)
      Sys.sleep(0.003)
      nrow(value)
    })
  methods::setMethod("dbGetQuery", signature(conn = cname, statement = "character"),
    function(conn, statement, ...) {
      if (!grepl("^SELECT COUNT_BIG\\(\\*\\) AS n FROM \\[.*\\]$", statement))
        stop("Unexpected count query")
      name <- sub("^SELECT COUNT_BIG\\(\\*\\) AS n FROM \\[(.*)\\]$",
                  "\\1", statement)
      data.frame(n = nrow(get(name, envir = conn@state$tables, inherits = FALSE)))
    })
  methods::setMethod("dbReadTable", signature(conn = cname, name = "character"),
    function(conn, name, ...) {
      Sys.sleep(0.003)
      get(name, envir = conn@state$tables, inherits = FALSE)
    })
  methods::setMethod("dbExecute", signature(conn = cname, statement = "character"),
    function(conn, statement, ...) {
      if (!grepl("^DROP TABLE IF EXISTS \\[.*\\]$", statement))
        stop("Unexpected mock SQL")
      name <- sub("^DROP TABLE IF EXISTS \\[(.*)\\]$", "\\1", statement)
      if (exists(name, envir = conn@state$tables, inherits = FALSE))
        rm(list = name, envir = conn@state$tables)
      0L
    })
  mock <- function() {
    state <- new.env(parent = emptyenv())
    state$locks <- logical()
    state$created <- character()
    state$tables <- new.env(parent = emptyenv())
    list(con = methods::new(cname, state = state), state = state)
  }
  a <- mock()
  b <- mock()
  result <- bm$benchmark_dbi(
    con = list(RSqlClientNET = a$con, odbc = b$con),
    n_write = 8L, n_read = 8L, repetitions_write = 1L,
    repetitions_read = 1L, types = "integer", null_fractions = 0,
    append_modes = c("append", "append_table_lock"), verbose = FALSE)
  testthat::expect_equal(result$total, 7L)
  testthat::expect_equal(result$completed, 7L)
  testthat::expect_true(all(result$details$status == "ok"))
  testthat::expect_setequal(result$details$operation,
    c("write", "append", "append_table_lock", "read"))
  testthat::expect_identical(a$state$locks, c(FALSE, TRUE))
  testthat::expect_identical(b$state$locks, FALSE)
  testthat::expect_length(ls(a$state$tables), 0L)
  testthat::expect_length(ls(b$state$tables), 0L)
  rs <- result$summary[result$summary$driver == "RSqlClientNET", ]
  testthat::expect_equal(rs$append_ok, 1L)
  testthat::expect_equal(rs$append_table_lock_ok, 1L)
  testthat::expect_true(is.finite(rs$median_append_s))
  testthat::expect_true(is.finite(rs$median_append_table_lock_s))
  os <- result$summary[result$summary$driver == "odbc", ]
  testthat::expect_equal(os$append_ok, 1L)
  testthat::expect_equal(os$append_table_lock_runs, 0L)
  testthat::expect_true(is.na(os$median_append_table_lock_s))

  # Both physical target types must run independently, with matching input
  # shapes, retained read fixtures, and no tables left in either connection.
  a2 <- mock()
  b2 <- mock()
  both <- bm$benchmark_dbi(
    con = list(RSqlClientNET = a2$con, odbc = b2$con),
    n_write = 8L, n_read = 8L, repetitions_write = 1L,
    repetitions_read = 1L, types = "integer", null_fractions = 0,
    append_modes = c("append", "append_table_lock"),
    table_types = c("temporary", "permanent"), verbose = FALSE)
  testthat::expect_equal(both$total, 14L)
  testthat::expect_equal(both$completed, 14L)
  testthat::expect_true(all(both$details$status == "ok"))
  testthat::expect_setequal(unique(both$details$table_type),
                            c("temporary", "permanent"))
  testthat::expect_equal(nrow(both$summary), 4L)
  testthat::expect_equal(as.integer(table(both$details$table_type)),
                         c(7L, 7L))
  testthat::expect_identical(a2$state$locks, c(FALSE, TRUE, FALSE, TRUE))
  testthat::expect_true(any(startsWith(a2$state$created, "#br_")))
  testthat::expect_true(any(startsWith(a2$state$created, "br_")))
  testthat::expect_length(ls(a2$state$tables), 0L)
  testthat::expect_length(ls(b2$state$tables), 0L)
  testthat::expect_true(all(both$summary$read_ok == 1L))

  # A run_dir from the old target-type settings is not a compatible resume.
  run_dir <- tempfile("bench_targets_035f_")
  on.exit(unlink(run_dir, recursive = TRUE), add = TRUE)
  invisible(bm$benchmark_dbi(
    con = list(RSqlClientNET = mock()$con), n_write = 8L, n_read = 8L,
    repetitions_write = 1L, repetitions_read = 1L, types = "integer",
    null_fractions = 0, table_types = "temporary", run_dir = run_dir,
    verbose = FALSE))
  testthat::expect_error(bm$benchmark_dbi(
    con = list(RSqlClientNET = mock()$con), n_write = 8L, n_read = 8L,
    repetitions_write = 1L, repetitions_read = 1L, types = "integer",
    null_fractions = 0, table_types = c("temporary", "permanent"),
    run_dir = run_dir, resume = TRUE, verbose = FALSE),
    "different benchmark settings")
})
