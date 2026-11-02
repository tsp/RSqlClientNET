# Native exact ID integrity / callback failure / partial SCB2 failures.
# Requires installed RSqlClientNET v0.0.26 and .NET SDK; no SQL Server needed.
library(RSqlClientNET)
stopifnot(utils::packageVersion("RSqlClientNET") >= "0.0.26")
start <- getFromNamespace(".sql_start_helper", "RSqlClientNET")
run_columnar <- getFromNamespace(".sql_columnar_query", "RSqlClientNET")
new_bitmap <- function(n) .Call("sqlcpp_id_bitmap_new", as.numeric(n), PACKAGE="RSqlClientNET")
add_bitmap <- function(b, ids) .Call("sqlcpp_id_bitmap_add", b, ids, PACKAGE="RSqlClientNET")
report_bitmap <- function(b) .Call("sqlcpp_id_bitmap_report", b, PACKAGE="RSqlClientNET")

# Native bitmap unit tests: no helper or server needed, across batch boundaries.
b <- new_bitmap(8)
add_bitmap(b, c(8L, 3L, 1L))
add_bitmap(b, c(7, 6, 2, 5, 4))
res <- report_bitmap(b)
stopifnot(isTRUE(res$ok), res$bitmap_bytes == 1, res$unique_ids == 8)

b <- new_bitmap(5)
add_bitmap(b, c(1, 2, 2, 4, 6, NA_real_, 4.5))
res <- report_bitmap(b)
stopifnot(!res$ok, res$duplicate_ids == 1, res$invalid_ids == 3,
          res$unique_ids == 3, res$missing_ids == 2,
          identical(res$missing_examples, c(3, 5)))

# Run the actual .NET SCB2 serializer and Rcpp decoder on synthetic rows;
# verify order-independent IDs and injected duplicate/range errors.
run_fixture <- function(mode) {
  con <- start("rcpp")
  on.exit(sql_disconnect(con))
  bitmap <- new_bitmap(5000)
  reply <- run_columnar(con,
    list(op="memory_test", nRows=5000L, integrityMode=mode),
    callback=function(batch) add_bitmap(bitmap, batch$id),
    timeout=30, batch_rows=17L, precision="native", tz="UTC")
  r <- report_bitmap(bitmap)
  stopifnot(sql_is_valid(con), reply$rows_received == 5000,
            r$examined_ids == 5000, reply$batches_received > 200)
  r
}
normal <- run_fixture("normal")
reordered <- run_fixture("reordered")
duplicate <- run_fixture("duplicate")
out_of_range <- run_fixture("out_of_range")
stopifnot(normal$ok, reordered$ok,
          !duplicate$ok, duplicate$duplicate_ids == 1,
          duplicate$missing_ids == 1, duplicate$missing_examples[1] == 3,
          !out_of_range$ok, out_of_range$invalid_ids == 1,
          out_of_range$missing_ids == 1, out_of_range$missing_examples[1] == 3)

# Explicit failure and timeout AFTER valid SCB2 batch frames. Such requests
# must invalidate the helper, while a later *new* helper remains usable.
for (mode in c("error", "stall")) {
  con <- start("rcpp")
  seen <- 0L
  message <- tryCatch(run_columnar(con,
    list(op="columnar_fault_test", mode=mode, milliseconds=2500L),
    timeout=if (mode == "stall") 0.25 else 5,
    batch_rows=2L, precision="native", tz="UTC",
    callback=function(batch) seen <<- seen + nrow(batch)),
    error=function(e) conditionMessage(e))
  stopifnot(is.character(message), length(message)==1L,
            seen == 2L, !sql_is_valid(con))
  if (mode == "stall") stopifnot(grepl("timed out|timeout", message, ignore.case=TRUE))
  sql_disconnect(con)
}
stopifnot(sql_ping(transport="rcpp")$ok)
message("PASS v0.0.26: exact bitmap, order-independent SCB2 integrity, corrupt IDs, partial-stream fault, hang/timeout and fresh helper")
