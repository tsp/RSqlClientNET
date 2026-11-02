# Run after con <- sql_connect(...) against a test SQL Server database.
# SQL VALUES avoids creating a table and tests both sides of the Danish
# autumn DST transition, an offset west of UTC, and SQL NULL.
library(RSqlClientNET)

query_offset <- "
SELECT v.id, CAST(v.dt AS datetimeoffset(7)) AS value_offset
FROM (VALUES
  (1, '2026-10-25T02:30:00.1234567+02:00'),
  (2, '2026-10-25T02:30:00.1234567+01:00'),
  (3, '2026-07-15T12:34:56.1234567-05:00'),
  (4, NULL)
) AS v(id, dt)
ORDER BY v.id
"

expected <- as.numeric(as.POSIXct(
  c("2026-10-25 00:30:00", "2026-10-25 01:30:00",
    "2026-07-15 17:34:56"),
  format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) + 0.1234567

for (method in c("lubridate", "base")) {
  z <- sql_query(con, query_offset, precision = "native",
                 tz = "Europe/Copenhagen", datetime_parser = method)
  stopifnot(nrow(z) == 4L,
            inherits(z$value_offset, "POSIXct"),
            identical(attr(z$value_offset, "tzone"), "UTC"),
            all(abs(as.numeric(z$value_offset[1:3]) - expected) < 1e-5),
            is.na(z$value_offset[4]))
  print(sql_timings(z)[c("datetime_parser", "type_conversion_seconds", "total_seconds")])
}

raw <- sql_query(con, query_offset, precision = "full")
stopifnot(is.character(raw$value_offset),
          grepl("+02:00", raw$value_offset[1], fixed = TRUE),
          grepl("+01:00", raw$value_offset[2], fixed = TRUE),
          is.na(raw$value_offset[4]))
