library(RSqlClientNET)
Sys.setenv(RSQLCLIENTNET_PIPE_POLL_MS = "1")
con <- RSqlClientNET:::.sql_start_helper("rcpp")
tryCatch({
  sql_pipe_stats(con, reset = TRUE)
  answer <- RSqlClientNET:::.sql_rpc(con, list(op="ping", echo="native-pipe"), timeout=10)
  stopifnot(identical(answer$reply, "pong"))
  counts <- sql_pipe_stats(con, reset = TRUE)
  stopifnot(counts$read_calls >= 1, counts$bytes_read > 0,
            counts$poll_ms %in% c(0:20))
}, finally = sql_disconnect(con))
