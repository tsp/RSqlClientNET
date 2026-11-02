# Offline S4 compatibility smoke test; no SQL Server required.
library(DBI)
library(RSqlClientNET)
stopifnot(methods::isClass("SqlClientNETConnection"))
for (name_class in c("character", "Id", "SQL")) {
  signature <- c("SqlClientNETConnection", name_class)
  for (generic in c("dbWriteTable", "dbAppendTable", "dbCreateTable", "dbRemoveTable")) {
    method <- methods::selectMethod(generic, signature, optional=TRUE)
    if (is.null(method)) stop("Missing ",generic," for ",name_class)
  }
  append <- methods::selectMethod("dbAppendTable", signature)
  create <- methods::selectMethod("dbCreateTable", signature)
  stopifnot(identical(tail(names(formals(append)),1L),"row.names"),
            identical(tail(names(formals(create)),2L),c("row.names","temporary")))
}
message("RSqlClientNET 0.0.30.2 DBI table method signatures passed")
