# The DBI-to-backend bridge must work even without SQL Server or .NET.
test_that("DBI S4 wrappers resolve the same legacy environment", {
  h <- new.env(parent = emptyenv())
  class(h) <- "sqlclient_connection"
  h$transport <- "rcpp"
  h$closed <- TRUE
  h$invalid_reason <- "synthetic closed connection"
  h$dbi_options <- list(precision="native", bigint="character",
                        datetime="POSIXct", binary="raw",
                        unsupported="error", tz="UTC",
                        datetime_parser="base", batch_rows=8192L,
                        timeout=600)
  con <- methods::new("SqlClientNETConnection", handle=h)
  get_handle <- getFromNamespace(".sql_handle", "RSqlClientNET")
  expect_identical(get_handle(con), h)
  expect_identical(get_handle(h), h)
  expect_false(sql_is_valid(con))
  expect_false(sql_connection_status(con)$valid)
  expect_identical(sql_connection_status(con)$invalid_reason, "synthetic closed connection")
  expect_true(sql_cancel(con))
  expect_error(sql_pipe_stats(con), "requires an open")
  expect_error(sql_reconnect(con, transport="base64"), "preserves")
  expect_error(sql_reconnect(con, precision="full"), "preserves the DBI options")
})

test_that("active DBI cursor blocks extension commands without killing helper", {
  h <- new.env(parent = emptyenv())
  class(h) <- "sqlclient_connection"
  h$transport <- "rcpp"
  h$dbi_cursor_active <- TRUE
  h$closed <- TRUE  # any attempted I/O would fail; locks must fire first
  con <- methods::new("SqlClientNETConnection", handle=h)
  guard <- getFromNamespace(".sql_check_not_busy", "RSqlClientNET")
  expect_error(guard(h), "dbClearResult")
  expect_error(sql_ping(con), "dbClearResult")
  expect_error(sql_query_stream(con, "SELECT 1", callback=function(batch) NULL), "dbClearResult")
  expect_error(sql_query(con, "SELECT 1"), "dbClearResult")
  expect_error(sql_execute(con, "SELECT 1"), "dbClearResult")
  expect_error(sql_begin(con), "dbClearResult")
  expect_error(sql_commit(con), "dbClearResult")
  expect_error(sql_rollback(con), "dbClearResult")
  h$dbi_cursor_active <- FALSE
  h$stream_active <- TRUE
  expect_error(guard(h), "streaming callback")
  expect_error(sql_query_stream(con, "SELECT 1", callback=function(batch) NULL), "streaming callback")
})

test_that("developer fixtures are internal and documented API is exported", {
  exports <- getNamespaceExports("RSqlClientNET")
  for (f in c("SqlClientNET", "sql_query_stream", "sql_cancel", "sql_reconnect",
              "sql_pipe_stats", "sql_connection_status", "sql_param", "sql_timings"))
    expect_true(f %in% exports, info=f)
  for (f in c("sql_edge_types_report", "sql_timeout_test", "sql_stress_test",
              "sql_batch_benchmark", "sql_types_test")) {
    expect_false(f %in% exports, info=f)
    expect_true(exists(f, envir=asNamespace("RSqlClientNET"), inherits=FALSE))
  }
})
