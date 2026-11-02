# Test helper exceptions, interrupted streams, fresh-helper recovery and
# different connection transports WITHOUT touching SQL Server.
library(RSqlClientNET)
for (transport in "rcpp") {
  test <- sql_robustness_test(transport)
  stopifnot(isTRUE(test$ok), isTRUE(test$fresh_helper_ok),
            length(test$errors) == 2L)
}
stopifnot(isTRUE(sql_stream_timeout_test(timeout=0.25, sleep_seconds=2,
                                        transport="rcpp")$timed_out))
message("PASS: v0.0.24 synthetic helper robustness")
