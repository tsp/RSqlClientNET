# No SQL Server, .NET or pipe startup is required for this class regression.
# Run after installing: source("tests/dbi-0281-class.R")
library(RSqlClientNET)
library(methods)

# sql_connect() returns a classed environment, not a bare "environment".
h <- new.env(parent = emptyenv())
class(h) <- "sqlclient_connection"
stopifnot(is.environment(h), inherits(h, "sqlclient_connection"))
conn <- methods::new("SqlClientNETConnection", handle = h)
stopifnot(methods::validObject(conn), identical(conn@handle, h))

# S4 validity must still reject arbitrary objects and unrelated environments.
for (invalid_handle in list(1L, new.env(parent = emptyenv()), list())) {
  result <- try(methods::new("SqlClientNETConnection", handle = invalid_handle),
                silent = TRUE)
  stopifnot(inherits(result, "try-error"))
}
message("RSqlClientNET 0.0.28.1 DBI S4 handle regression passed")
