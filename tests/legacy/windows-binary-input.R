# No SQL Server needed: exercise UTF-8 JSON stdin and raw framed stdout.
library(RSqlClientNET)
stopifnot(packageVersion("RSqlClientNET") >= "0.0.15")
for (transport in "rcpp") {
  stopifnot(isTRUE(sql_ping(transport = transport)$ok))
  x <- sql_memory_test(transport = transport)
  stopifnot(identical(x$label[[1]], "æøå café"),
            identical(x$label[[2]], 'Line 1\nLine 2, "citat"'))
  # Verify that UTF-8 requests also survive the stdin channel.
  con <- RSqlClientNET:::.sql_start_helper(transport)
  tryCatch({
    echo <- "æøå 日本語 🔬"
    r <- RSqlClientNET:::.sql_rpc(con, list(op = "ping", echo = echo), timeout = 10L)
    stopifnot(identical(r$echo, echo), identical(r$reply, "pong"))
  }, finally = sql_disconnect(con))
}
cat("Binary stdin transport tests completed successfully.\n")
