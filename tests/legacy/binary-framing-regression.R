# No SQL Server connection or sensitive result data required.
library(RSqlClientNET)
stopifnot(isTRUE(sql_ping(transport = "rcpp")$ok))
a <- sql_memory_test(transport = "rcpp")
stopifnot(identical(a$label[[2L]], 'Line 1\nLine 2, "citat"'))
