library(RSqlClientNET)
# No SQL Server; helper deliberately stalls for 3 seconds.
x <- sql_timeout_test(timeout = 0.25, sleep_seconds = 3)
stopifnot(isTRUE(x$timed_out), !isTRUE(x$helper_still_alive),
          x$elapsed_seconds < 2)
message("Native timeout and helper termination passed")
