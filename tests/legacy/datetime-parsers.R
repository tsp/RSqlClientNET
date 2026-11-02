library(RSqlClientNET)

native_ts <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")
parse_csv <- getFromNamespace(".sql_parse_csv", "RSqlClientNET")

# Unambiguous SQL timestamps (local wall clock) and explicit SQL datetimeoffset
# are comparable across the two parser engines to within floating-point precision.
stamps <- c("2026-01-15T12:34:56.1234567",
            "2026-07-15T12:34:56.1234567", NA_character_)
values <- c("2026-10-25T02:30:00.1234567+02:00",
            "2026-10-25T02:30:00.1234567+01:00",
            "2026-07-15T12:34:56.1234567-05:00",
            "2026-07-15T12:34:56.1234567Z", NA_character_)
expected_epoch <- as.numeric(as.POSIXct(
  c("2026-10-25 00:30:00", "2026-10-25 01:30:00",
    "2026-07-15 17:34:56", "2026-07-15 12:34:56"),
  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) + 0.1234567

for (p in c("lubridate", "base")) {
  for (zone in c("", "UTC", "Europe/Copenhagen", "America/New_York")) {
    x <- native_ts(stamps, tz = zone, datetime_parser = p)
    stopifnot(inherits(x, "POSIXct"),
              identical(attr(x, "tzone"), zone),
              is.na(x[[3]]),
              identical(format(x[1], "%Y-%m-%d %H:%M:%S", tz = zone),
                        "2026-01-15 12:34:56"))
    offset <- native_ts(values, tz = zone, with_offset = TRUE,
                        datetime_parser = p)
    stopifnot(inherits(offset, "POSIXct"),
              identical(attr(offset, "tzone"), "UTC"),
              is.na(offset[[5]]),
              all(abs(as.numeric(offset[1:4]) - expected_epoch) < 1e-5),
              abs(as.numeric(difftime(offset[2], offset[1], units = "hours")) - 1) < 1e-8)
  }
}

# Full precision must remain *character* without parsing. Verify that the
# public-facing argument is passed to the SQL schema converter correctly,
# and both CSV parsers preserve the raw timestamps.
headers <- list(columns = c("id", "stamp", "offset"),
                types = c("int", "datetime2", "datetimeoffset"),
                valueEncoding = "tagged-v1")
csv <- paste0(
  '"id","stamp","offset"\n',
  '"V1","V2026-07-15T12:34:56.1234567",',
  '"V2026-10-25T02:30:00.1234567+02:00"\n',
  '"V2","N","N"\n'
)
for (csv_parser in c("fread", "read.csv")) {
  for (p in c("lubridate", "base")) {
    exact <- parse_csv(csv, headers, parser = csv_parser,
                       precision = "full", datetime_parser = p)
    native <- parse_csv(csv, headers, parser = csv_parser,
                        precision = "native", datetime_parser = p,
                        tz = "Europe/Copenhagen")
    stopifnot(identical(exact$stamp[1], "2026-07-15T12:34:56.1234567"),
              identical(exact$offset[1], "2026-10-25T02:30:00.1234567+02:00"),
              is.character(exact$stamp), is.character(exact$offset),
              is.na(exact$stamp[2]), is.na(exact$offset[2]),
              inherits(native$stamp, "POSIXct"),
              identical(attr(native$stamp, "tzone"), "Europe/Copenhagen"),
              inherits(native$offset, "POSIXct"),
              identical(attr(native$offset, "tzone"), "UTC"),
              abs(as.numeric(native$offset[1]) - expected_epoch[1]) < 1e-5,
              is.na(native$stamp[2]), is.na(native$offset[2]))
  }
}

# DST spring-forward gap: fast lubridate parser may normalize nonexistent wall
# times in tz=Europe/Copenhagen; base and lubridate need not agree. Do not use
# native POSIXct for lossless local-clock reproduction at DST boundaries.
stopifnot(identical(
  parse_csv(csv, headers, precision = "full")$offset[1],
  "2026-10-25T02:30:00.1234567+02:00"
))
