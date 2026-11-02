# Run after installing package; no SQL Server necessary.
library(RSqlClientNET)

for (mode in "rcpp") {
  pong <- sql_ping(transport = mode)
  stopifnot(isTRUE(pong$ok), identical(pong$transport, mode))

  # chunk_kib controls CSV byte chunks, not columnar row batches.
  x <- sql_memory_test(transport = mode, n_rows = 5000L, chunk_kib = 64L,
                       result_format = "csv")
  stopifnot(nrow(x) == 5000L, identical(x$label[[1]], "æøå café"),
            identical(x$label[[2]], 'Line 1\nLine 2, "citat"'),
            is.na(x$nullable[[2]]),
            identical(sql_timings(x)$transport, mode),
            sql_timings(x)$chunks_received > 1L)

  y <- sql_types_test(transport = mode, precision = "full", bigint = "character")
  stopifnot(identical(y$code[[1]], "000123"),
            identical(y$empty_text[[1]], ""),
            identical(y$nullable_text[[1]], "NA"),
            is.na(y$nullable_text[[3]]),
            identical(y$exact_decimal[[1]], "1234567890123456789012345678.1234567890"),
            identical(y$blob[[1]], as.raw(c(0, 1, 255))))

  z <- sql_types_test(transport = mode, precision = "native", tz = "UTC")
  stopifnot(inherits(z$stamp, "POSIXct"),
            inherits(z$offset_dt, "POSIXct"))
}

cat("Both Rcpp binary transport passed synthetic data tests.\n")
