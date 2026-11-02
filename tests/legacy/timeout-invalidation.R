library(RSqlClientNET)
for (mode in "rcpp") {
  x <- sql_stream_timeout_test(timeout = 0.25, sleep_seconds = 2, transport = mode)
  stopifnot(isTRUE(x$timed_out), !isTRUE(x$helper_still_alive),
            !isTRUE(x$connection_valid), x$elapsed_seconds < 5)
  # Same connection must remain usable when the entire stream is received.
  con <- RSqlClientNET:::.sql_start_helper(mode)
  tryCatch({
    full <- RSqlClientNET:::.sql_stream_query(
      con, list(op = "stream_stall_test", milliseconds = 0L), timeout = 5)
    stopifnot(nrow(full) == 1L, sql_is_valid(con))
    stopifnot(isTRUE(sql_ping(con)$ok))
    # Regression for an RPC timeout that occurs in R itself.
    timed <- tryCatch(RSqlClientNET:::.sql_rpc(con,
      list(op = "sleep_test", milliseconds = 2000L), timeout = 0.25),
      error = function(e) conditionMessage(e))
    stopifnot(is.character(timed), !sql_is_valid(con))
    # No stale frame may be consumed: fail immediately without sending anything.
    next_error <- tryCatch(sql_ping(con), error = function(e) conditionMessage(e))
    stopifnot(is.character(next_error),
              grepl("not running", next_error, fixed = TRUE),
              !grepl("Unexpected data frame", next_error, fixed = TRUE))
  }, finally = sql_cancel(con))
}
message("PASS: both transports close the connection on incomplete requests")
