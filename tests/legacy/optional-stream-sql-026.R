# Optional real SQL Server regression: set RSQLCLIENTNET_TEST_CONNECTION_STRING.
library(RSqlClientNET)
cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
if (!nzchar(cs)) {
  message("SKIP: set RSQLCLIENTNET_TEST_CONNECTION_STRING to test SQL integration")
} else {
  con <- sql_connect(connection_string=cs, transport="rcpp")
  tryCatch({
    query <- paste0("SELECT TOP (5000) CAST(ROW_NUMBER() OVER ",
      "(ORDER BY (SELECT NULL)) AS int) AS test_id ",
      "FROM sys.all_objects a CROSS JOIN sys.all_objects b")
    report <- sql_stream_integrity_test(con, query, expected_rows=5000,
      batch_rows=17L, precision="native", tz="UTC")
    stopifnot(report$ok, report$rows_received == 5000)
    bench <- sql_batch_benchmark(con, query,
      batch_sizes=c(128L, 1024L, 8192L), repetitions=2,
      precision="native", tz="UTC")
    stopifnot(nrow(bench) == 6, length(unique(bench$rows_received)) == 1,
      all(bench$rows_received == 5000), sql_is_valid(con))
    print(sql_batch_summary(bench))
  }, finally=sql_disconnect(con))
}
