# Run after installing RSqlClientNET; does not need access to SQL Server.
library(RSqlClientNET)
sqlclient_build()
answer <- sql_ping(timeout = 10)
print(answer)
stopifnot(isTRUE(answer$ok), identical(answer$reply, "pong"),
          identical(answer$connection_state, "Closed"))
