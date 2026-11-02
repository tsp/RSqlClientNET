library(RSqlClientNET)
parse <- getFromNamespace(".sql_parse_csv", "RSqlClientNET")

# Deliberately exercise parser modes without requiring dotnet/SQL Server.
columns <- c("id", "amount", "cash", "big", "stamp", "offset_dt", "clock",
             "literal", "empty_or_null")
types <- c("int", "decimal", "money", "bigint", "datetime2",
           "datetimeoffset", "time", "nvarchar", "nvarchar")
header <- list(columns = columns, types = types, valueEncoding = "tagged-v1")

# Every cell is tagged V<value> or N (SQL NULL); CSV quote escaping is explicit.
encode <- function(x) {
  if (is.na(x)) return("N")
  paste0('"', gsub('"', '""', paste0("V", x), fixed = TRUE), '"')
}
rows <- list(
  c("1", "1234567890123456789012345678.1234567890", "12.3400",
    "9007199254740993", "2026-10-16T12:34:56.1234567",
    "2026-10-16T12:34:56.1234567+02:00", "12:34:56.1234567",
    'æøå "citat", og\nny linje', ""),
  c("2", "-0.0000000000000000000000000001", "0.0001",
    "-5000000000", "2000-01-01T00:00:00",
    "2000-01-01T00:00:00-05:00", "00:00:00",
    "NA", NA_character_),
  rep(NA_character_, length(columns))
)
header_line <- paste(sprintf('"%s"', columns), collapse = ",")
body <- vapply(rows, function(v) paste(vapply(v, encode, character(1)), collapse = ","), character(1))
csv <- paste(c(header_line, body), collapse = "\n")

for (parser in c("fread", "read.csv")) {
  full <- parse(csv, header, parser = parser, precision = "full")
  native <- parse(csv, header, parser = parser, precision = "native")
  stopifnot(
    identical(full$amount[1], "1234567890123456789012345678.1234567890"),
    is.character(full$amount), is.numeric(native$amount),
    abs(native$cash[1] - 12.34) < 1e-10,
    is.na(native$amount[3]),
    identical(as.character(native$big[1]), "9007199254740993"),
    inherits(native$big, "integer64"),
    inherits(native$stamp, "POSIXct"),
    inherits(native$offset_dt, "POSIXct"),
    inherits(native$clock, "difftime"),
    abs(as.numeric(native$clock[1], units = "secs") - 45296.1234567) < 1e-5,
    identical(format(native$stamp[1], "%Y-%m-%d %H:%M:%S", tz = ""),
              "2026-10-16 12:34:56"),
    identical(format(native$stamp[2], "%Y-%m-%d %H:%M:%S", tz = ""),
              "2000-01-01 00:00:00"),
    identical(format(native$offset_dt[1], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
              "2026-10-16 10:34:56"),
    identical(format(native$offset_dt[2], "%Y-%m-%d %H:%M:%S", tz = "UTC"),
              "2000-01-01 05:00:00"),
    identical(full$literal[1], 'æøå "citat", og\nny linje'),
    identical(native$literal[1], full$literal[1]),
    identical(full$empty_or_null[1], ""),
    is.na(full$empty_or_null[2]),
    identical(full$literal[2], "NA"),
    is.na(native$literal[3])
  )
  override <- parse(csv, header, parser = parser, precision = "native",
                    datetime = "character", bigint = "character")
  stopifnot(is.character(override$stamp), is.character(override$big))
}

# Boundary: bit64 integer64 uses the minimum int64 value as NA sentinel.
min_sql <- '-9223372036854775808'
stopifnot(identical(getFromNamespace('.sql_convert_column', 'RSqlClientNET')(
  min_sql, "bigint", bigint = "character", datetime = "character",
  binary = "raw", unsupported = "error", precision = "full"), min_sql))
warns <- character()
min_val <- withCallingHandlers(getFromNamespace('.sql_convert_column', 'RSqlClientNET')(
  min_sql, "bigint", bigint = "integer64", datetime = "POSIXct",
  binary = "raw", unsupported = "error", precision = "native"),
  warning = function(w) {warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")})
stopifnot(bit64::is.integer64(min_val), is.na(min_val), length(warns) == 1L,
          grepl("converted to NA", warns, fixed = TRUE))
