library(RSqlClientNET)
to_timestamp <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")

wall <- "2026-10-16T12:34:56.1234567"
local <- to_timestamp(c(wall, NA_character_))
expected <- as.POSIXct(wall, format = "%Y-%m-%dT%H:%M:%OS", tz = "")
stopifnot(
  inherits(local, "POSIXct"),
  identical(attr(local, "tzone"), ""),
  isTRUE(all.equal(as.numeric(local[[1L]]), as.numeric(expected))),
  is.na(local[[2L]]),
  identical(format(local[[1L]], "%Y-%m-%d %H:%M:%S", tz = ""),
            "2026-10-16 12:34:56")
)

# datetimeoffset MUST remain UTC and preserve the actual moment in time.
offset <- to_timestamp("2026-10-16T12:34:56.1234567+02:00", with_offset = TRUE)
stopifnot(
  identical(attr(offset, "tzone"), "UTC"),
  identical(format(offset, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
            "2026-10-16 10:34:56")
)

# Ensure both public parser modes use local timestamps.
test <- sql_types_test(precision = "native")
stopifnot(
  inherits(test$stamp, "POSIXct"),
  identical(attr(test$stamp, "tzone"), ""),
  identical(attr(test$offset_dt, "tzone"), "UTC")
)

# Different tz values change interpretation of time-zone-naive timestamps.
for (zone in c("UTC", "Europe/Copenhagen", "America/New_York")) {
  v <- to_timestamp(wall, tz = zone)
  stopifnot(
    identical(attr(v, "tzone"), zone),
    identical(format(v, "%Y-%m-%d %H:%M:%S", tz = zone),
              "2026-10-16 12:34:56")
  )
  # datetimeoffset retains the stored offset and returns a UTC instant.
  offset_zone <- to_timestamp("2026-10-16T12:34:56.1234567+02:00",
                              with_offset = TRUE, tz = zone)
  stopifnot(identical(attr(offset_zone, "tzone"), "UTC"),
            identical(format(offset_zone, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
                      "2026-10-16 10:34:56"))
}
stopifnot(
  as.numeric(to_timestamp(wall, tz = "UTC")) !=
    as.numeric(to_timestamp(wall, tz = "Europe/Copenhagen")),
  inherits(try(to_timestamp(wall, tz = "Not/A_Time_Zone"), silent = TRUE), "try-error")
)

# The public query fixture accepts the new argument without SQL Server.
for (parser in c("fread", "read.csv")) {
  test_zone <- sql_types_test(precision = "native", tz = "Europe/Copenhagen",
                              parser = parser)
  stopifnot(identical(attr(test_zone$stamp, "tzone"), "Europe/Copenhagen"),
            identical(attr(test_zone$offset_dt, "tzone"), "UTC"),
            identical(sql_timings(test_zone)$tz, "Europe/Copenhagen"))
}

# Full mode remains exact strings regardless of tz, even if an IANA zone is supplied.
for (zone in c("", "UTC", "Europe/Copenhagen")) {
  full_zone <- sql_types_test(precision = "full", tz = zone)
  stopifnot(is.character(full_zone$stamp),
            identical(full_zone$stamp[1], "2026-10-16T12:34:56.1234567"))
}

# An explicit POSIXct override is respected even in precision = "full".
override_zone <- sql_types_test(precision = "full", datetime = "POSIXct",
                               tz = "UTC")
stopifnot(identical(attr(override_zone$stamp, "tzone"), "UTC"))
