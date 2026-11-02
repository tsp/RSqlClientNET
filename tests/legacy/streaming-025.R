# Test C#/Rcpp batch callbacks without SQL Server. Requires .NET SDK.
library(RSqlClientNET)
start <- getFromNamespace(".sql_start_helper", "RSqlClientNET")
run_columnar <- getFromNamespace(".sql_columnar_query", "RSqlClientNET")

memory <- sql_process_memory()
stopifnot(is.list(memory),
          all(c("rss_bytes", "peak_rss_bytes", "private_bytes", "source") %in% names(memory)),
          is.finite(memory$rss_bytes), memory$rss_bytes > 0)

# Exercise batches and compare against identical nonstreamed columnar data.
con <- start("rcpp")
all_ids <- integer(0)
labels <- character(0)
nullable_na <- 0L
batches <- 0L
result <- tryCatch({
  full <- run_columnar(con, list(op="memory_test", nRows=5000L),
                       timeout=30, batch_rows=128L, precision="native", tz="UTC")
  summary <- run_columnar(con, list(op="memory_test", nRows=5000L),
    callback=function(batch) {
      batches <<- batches + 1L
      stopifnot(inherits(batch, "data.table"), nrow(batch)<=128L,
                identical(names(batch), names(full)))
      all_ids <<- c(all_ids, batch$id)
      labels <<- c(labels, batch$label)
      nullable_na <<- nullable_na + sum(is.na(batch$nullable))
    }, timeout=30, batch_rows=128L, precision="native", tz="UTC")
  stopifnot(summary$rows_received == 5000,
            summary$batches_received == batches,
            length(all_ids) == 5000,
            identical(all_ids, full$id), identical(labels, full$label),
            nullable_na == 1L, sql_is_valid(con),
            identical(sql_schema(summary), sql_schema(full)),
            is.finite(sql_timings(summary)$total_seconds))
  sql_disconnect(con)
  summary
}, error=function(e) {sql_cancel(con);stop(e)})

# Callback error must terminate the helper and invalidate its connection.
con <- start("rcpp")
err <- tryCatch(run_columnar(con, list(op="memory_test", nRows=10000L),
         timeout=10, batch_rows=64L,
         callback=function(batch) stop("expected synthetic callback failure")),
         error=function(e) conditionMessage(e))
stopifnot(grepl("expected synthetic callback failure", err), !sql_is_valid(con))

# Reentrant SQL on the SAME helper is forbidden, to avoid protocol corruption.
con <- start("rcpp")
err <- tryCatch(run_columnar(con, list(op="memory_test", nRows=100L),
         timeout=10, batch_rows=10L, callback=function(batch) {
           sql_query(con, "SELECT 1", result_format="columnar")
         }), error=function(e) conditionMessage(e))
stopifnot(grepl("streaming callback",err), !sql_is_valid(con))

# Timeout while callback is busy: detected at the next receive boundary.
con <- start("rcpp")
err <- tryCatch(run_columnar(con, list(op="memory_test", nRows=100L),
         timeout=.2, batch_rows=10L, callback=function(batch) Sys.sleep(.4)),
         error=function(e) conditionMessage(e))
stopifnot(grepl("Timed out|timeout",err, ignore.case=TRUE), !sql_is_valid(con))
message("PASS: streaming values, class/schema, callback failure, reentrancy and timeout")
