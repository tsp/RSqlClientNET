library(RSqlClientNET)

# No SQL Server required. The synthetic fixture contains decimal(38,*), bigint,
# datetime2 (100ns precision), datetimeoffset, time, NULL, and text/binary.
full <- sql_types_test(precision = "full")
native <- sql_types_test(precision = "native")

str(full)
str(native)
stopifnot(
  is.character(full$exact_decimal),
  is.numeric(native$exact_decimal),
  is.character(full$stamp),
  inherits(native$stamp, "POSIXct"),
  inherits(native$offset_dt, "POSIXct"),
  inherits(native$clock, "difftime"),
  inherits(native$big, "integer64"),
  identical(attr(native$stamp, "tzone"), ""),
  identical(attr(native$offset_dt, "tzone"), "UTC"),
  identical(full$code, native$code),
  is.na(native$exact_decimal[3L]),
  identical(sql_timings(native)$precision, "native")
)

# Same options on real SQL queries:
# x <- sql_query(con, "SELECT * FROM dbo.MyTable", precision = "native")
# y <- sql_query(con, "SELECT * FROM dbo.MyTable", precision = "full")

# Interpret SQL datetime2 wall-clock values in a chosen time zone:
x_cph <- sql_query(con, query, precision = "native", tz = "Europe/Copenhagen")
# SQL datetimeoffset is still an actual UTC instant:
# attr(x_cph$datetimeoffset_column, "tzone")  # "UTC"
