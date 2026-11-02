# v0.0.27: crash isolation without SQL Server, on Windows.
# Install and rebuild helper first: sqlclient_build(force=TRUE)
library(RSqlClientNET)
stopifnot(packageVersion("RSqlClientNET") >= "0.0.27")

# 65536-row SCB2 serialization is deliberately allowed for synthetic memory_test
# even though it is guarded for actual SQL queries.
for (bs in c(8192L, 16384L, 32768L, 65536L)) {
  x <- sql_memory_test(n_rows=100000L, result_format="columnar",
                       transport="rcpp", batch_rows=bs,
                       precision="native", tz="UTC")
  stopifnot(nrow(x)==100000L,
            identical(x$id, seq_len(100000L)),
            identical(x$label[1L], "æøå café"),
            is.na(x$nullable[2L]))
  message("PASS SCB2 synthetic batch_rows=", bs)
}

# A bad SQL batch-size setting is rejected before any bytes are sent.
# This test needs no SQL Server connection and must NOT invalidate helper.
start <- getFromNamespace(".sql_start_helper", "RSqlClientNET")
query <- getFromNamespace(".sql_columnar_query", "RSqlClientNET")
con <- start("rcpp")
message <- tryCatch(query(con, list(op="query", sql="SELECT 1"),
                          batch_rows=65536L, precision="native", tz="UTC"),
                    error=function(e) conditionMessage(e))
stopifnot(is.character(message), grepl("32768",message), sql_is_valid(con))
sql_disconnect(con)
stopifnot(sql_ping(transport="rcpp")$ok)
cat("PASS guarded SQL size and synthetic SCB2 serializer\n")
