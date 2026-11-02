# Opt-in test against a disposable SQL Server; no writes or result files.
library(RSqlClientNET)
cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
if (!nzchar(cs)) {
  message("SKIP: set RSQLCLIENTNET_TEST_CONNECTION_STRING for streaming SQL test")
} else {
  con <- sql_connect(connection_string=cs, transport="rcpp")
  tryCatch({
    query <- paste0("SELECT TOP (10000) CAST(ROW_NUMBER() OVER ",
       "(ORDER BY (SELECT NULL)) AS int) AS row_id ",
       "FROM sys.all_objects a CROSS JOIN sys.all_objects b")
    full <- sql_query(con, query, result_format="columnar", precision="native")
    ids <- integer(0)
    stream <- sql_query_stream(con, query, precision="native", batch_rows=256L,
      callback=function(batch) ids <<- c(ids, batch$row_id))
    stopifnot(identical(ids, full$row_id),
              stream$rows_received == nrow(full),
              is.finite(sql_timings(stream)$total_seconds),
              sql_is_valid(con))
    message("PASS: SQL stream matches full columnar query")
  }, finally=sql_disconnect(con))
}
