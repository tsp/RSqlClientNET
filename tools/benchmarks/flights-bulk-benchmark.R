# Manual SQL Server benchmark - do not run as part of testthat.
# Requires two *existing, open* DBI connections in the calling environment:
#   sqlclient : RSqlClientNET DBI connection (Rcpp transport recommended)
#   odbc      : odbc DBI connection, pointing to the SAME SERVER / DATABASE
# And install.packages("nycflights13")
# Source this file from an R session; it creates/drops temporary benchmarking
# tables that have dedicated names, never your existing 'flights' table.
stopifnot(DBI::dbIsValid(sqlclient), DBI::dbIsValid(odbc))
stopifnot(requireNamespace("nycflights13",quietly=TRUE))
flights <- as.data.frame(nycflights13::flights)
local({
old_profile <- getOption("RSqlClientNET.bulk_profile")
options(RSqlClientNET.bulk_profile=TRUE)
on.exit(options(RSqlClientNET.bulk_profile=old_profile),add=TRUE)
rows <- c(10000L,50000L,nrow(flights))
bench <- list()
for (n in rows) {
  dat <- flights[seq_len(n),,drop=FALSE]
  for (backend in c("RSqlClientNET","odbc")) {
    con <- if (backend=="RSqlClientNET") sqlclient else odbc
    name <- paste0("RSqlClientNET_benchmark_",backend)
    if (DBI::dbExistsTable(con,name)) DBI::dbRemoveTable(con,name)
    elapsed <- system.time(DBI::dbWriteTable(con,name,dat))["elapsed"]
    actual <- DBI::dbGetQuery(con,paste0("SELECT COUNT(*) AS n FROM ",
                          as.character(DBI::dbQuoteIdentifier(con,name))))$n[[1L]]
    stopifnot(identical(as.numeric(actual),as.numeric(n)))
    DBI::dbRemoveTable(con,name)
    bench[[length(bench)+1L]] <- data.frame(
      backend=backend,rows=n,seconds=unname(elapsed),
      rows_per_second=as.numeric(n/elapsed))
  }
}
print(do.call(rbind,bench),row.names=FALSE)
})
# For investigating batch size after the default-path comparison:
# options(RSqlClientNET.bulk_profile=TRUE)
# system.time(DBI::dbWriteTable(sqlclient,"bench_32k",flights[1:50000,],batch_rows=32768L))
