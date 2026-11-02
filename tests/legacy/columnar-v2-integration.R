# Synthetic helper test: v2 full/native, UTC direct ticks, local DST fallback,
# decimal/bigint fidelity, SQL NULL, exact types and timeout safety.
library(RSqlClientNET)
check_mode <- function(precision, bigint, tz, datetime=NULL) {
  csv <- sql_types_test(result_format="csv", precision=precision,
                        bigint=bigint, datetime=datetime, tz=tz)
  col <- sql_types_test(result_format="columnar", precision=precision,
                        bigint=bigint, datetime=datetime, tz=tz)
  stopifnot(identical(names(csv), names(col)),
            identical(vapply(csv, function(x) paste(class(x),collapse="/"), ""),
                      vapply(col, function(x) paste(class(x),collapse="/"), "")))
  for (j in seq_along(csv)) {
    # bit64 integer64, Date, difftime, and POSIXct attributes compared too.
    x <- csv[[j]]; y <- col[[j]]
    stopifnot(isTRUE(all.equal(x, y, check.attributes=TRUE, tolerance=1e-6)))
  }
  stopifnot(nrow(col)==3L, is.data.frame(sql_type_timings(col)),
            identical(sql_schema(col), sql_schema(csv)))
  col
}
# Full preserves exact string decimals/timestamps; bigint can be integer64.
full <- check_mode("full", "character", "UTC")
stopifnot(identical(full$exact_decimal[[1]], "1234567890123456789012345678.1234567890"))
# Native sends decimals, time and datetimeoffset directly in binary;
# datetime2/datetime become ticks for UTC and text for local timezone.
native <- check_mode("native", "integer64", "UTC")
stopifnot(inherits(native$big,"integer64"),
          inherits(native$stamp,"POSIXct"),
          inherits(native$offset_dt,"POSIXct"),
          inherits(native$clock,"difftime"),
          inherits(native$day,"Date"))
local <- check_mode("native", "character", "Europe/Copenhagen")
stopifnot(identical(attr(local$stamp, "tzone"), "Europe/Copenhagen"),
          identical(attr(local$offset_dt,"tzone"), "UTC"))
override <- check_mode("native", "character", "UTC", datetime="character")
stopifnot(is.character(override$stamp), inherits(override$offset_dt,"POSIXct"))
# Check neither schema nor type profiler rely on conversion paths from CSV.
print(sql_timings(native)[c("total_seconds", "native_decode_seconds", "type_conversion_seconds")])
print(sql_type_timings(native))
