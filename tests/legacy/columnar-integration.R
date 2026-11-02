# Synthetic C# fixtures: requires locally built .NET helper but no SQL Server.
library(RSqlClientNET)
base <- sql_types_test(result_format="csv", precision="full", bigint="character")
col  <- sql_types_test(result_format="columnar", precision="full", bigint="character")
# SQL real/single has only ~7 decimal digits of precision: the C# CSV
# serializer emits round-trip decimal text while SCB2 transports the float
# as its binary value widened to double. Compare with a tolerance suited
# to REAL, but keep exact comparison of all character, raw and integer fields.
check_synthetic <- function(base, col, label) {
  stopifnot(identical(sql_schema(base), sql_schema(col)),
            identical(names(base), names(col)),
            identical(lapply(base, class), lapply(col, class)),
            identical(lapply(base, is.na), lapply(col, is.na)))
  types <- tolower(sql_schema(base)$sql_type)
  for (j in seq_along(base)) {
    typ <- types[[j]]
    exact <- is.character(base[[j]]) || is.list(base[[j]]) ||
             is.integer(base[[j]]) || is.logical(base[[j]]) ||
             inherits(base[[j]], "integer64")
    eq <- if (exact) identical(base[[j]], col[[j]]) else
      isTRUE(all.equal(base[[j]], col[[j]], check.attributes=TRUE,
                       tolerance=1e-6))
    if (!eq) {
      detail <- all.equal(base[[j]], col[[j]], check.attributes=TRUE, tolerance=1e-6)
      stop(sprintf("%s: column %s (SQL %s) mismatch: %s", label,
                   names(base)[[j]], typ, paste(detail, collapse="; ")),
           call.=FALSE)
    }
  }
  invisible(TRUE)
}
check_synthetic(base, col, "full/default timezone")
stopifnot(
          identical(col$big[[1]], "9223372036854775807"),
          identical(col$empty_text[[1]], ""),
          identical(col$nullable_text[[1]], "NA"),
          is.na(col$nullable_text[[3]]),
          identical(col$blob[[1]], as.raw(c(0,1,255))),
          identical(col$empty_blob[[1]], raw(0)),
          is.null(col$nullable_blob[[1]]))

for (mode in c("full", "native")) {
  old <- sql_types_test(result_format="csv", precision=mode, bigint="character", tz="UTC")
  new <- sql_types_test(result_format="columnar", precision=mode, bigint="character", tz="UTC")
  check_synthetic(old, new, paste(mode, "UTC"))
}
large <- sql_memory_test(result_format="columnar", n_rows=15000L, batch_rows=2048L)
stopifnot(nrow(large)==15000L, sql_timings(large)$chunks_received>1,
          identical(large$label[[2]], 'Line 1\nLine 2, "citat"'))
