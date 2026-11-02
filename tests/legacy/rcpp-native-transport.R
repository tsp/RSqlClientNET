library(RSqlClientNET)
for (mode in "rcpp") {
  pong <- sql_ping(transport = mode)
  stopifnot(isTRUE(pong$ok), identical(pong$reply, "pong"),
            identical(pong$transport, mode))
  # This test exercises small CSV byte chunks on the native Rcpp pipe.
  dt <- sql_memory_test(transport = mode, n_rows = 10000L, chunk_kib = 8L,
                        result_format = "csv")
  stopifnot(nrow(dt) == 10000L,
            identical(dt$label[[1L]], "æøå café"),
            identical(dt$label[[2L]], 'Line 1\nLine 2, "citat"'),
            is.na(dt$nullable[[2L]]),
            sql_timings(dt)$chunks_received > 1L)
  x <- sql_types_test(transport = mode, precision = "full", bigint = "character")
  stopifnot(identical(x$exact_decimal[[1]], "1234567890123456789012345678.1234567890"),
            identical(x$code[[1]], "000123"),
            identical(x$blob[[1]], as.raw(c(0, 1, 255))))
  # Unicode in control command on stdin; C# echoes unmodified value.
  con <- RSqlClientNET:::.sql_start_helper(mode)
  tryCatch({
    response <- RSqlClientNET:::.sql_rpc(con, list(op = "ping", echo = "æøå 日本語 🔬"), timeout = 10)
    stopifnot(identical(response$echo, "æøå 日本語 🔬"))
  }, finally = sql_disconnect(con))
}
message("Native Rcpp transport synthetic tests passed")
