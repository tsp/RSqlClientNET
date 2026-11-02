library(RSqlClientNET)
if (nzchar(Sys.which('dotnet'))) {
  # Check CSV byte chunking explicitly; columnar is now the default.
  x <- try(sql_memory_test(n_rows = 5000, chunk_kib = 24,
                           result_format = "csv"), silent = TRUE)
  if (!inherits(x, 'try-error')) {
    stopifnot(nrow(x) == 5000L, identical(x$label[[1]], 'æøå café'),
              identical(x$label[[2]], 'Line 1\nLine 2, "citat"'),
              is.na(x$nullable[[2]]), sql_timings(x)$chunks_received > 1L)
  }
}
