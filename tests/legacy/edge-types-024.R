# v0.0.24 regression: adversarial SQL values, no SQL Server needed.
library(RSqlClientNET)

compare_columns <- function(a, b, tolerance=1e-6) {
  stopifnot(identical(sql_schema(a), sql_schema(b)),
            identical(lapply(a, class), lapply(b, class)),
            identical(lapply(a, is.na), lapply(b, is.na)),
            identical(names(attributes(a)), names(attributes(b))))
  for (j in seq_along(a)) {
    stopifnot(isTRUE(all.equal(a[[j]], b[[j]], tolerance=tolerance,
                               check.attributes=TRUE)))
  }
}

for (prec in c("full", "native")) {
  for (tz in c("UTC", "Europe/Copenhagen")) {
    # Capture expected DST conversion warnings without hiding unexpected ones.
    capture_edge <- function(format) {
      warnings <- character()
      value <- withCallingHandlers(
        sql_edge_types_test(result_format=format, precision=prec,
                            bigint="character", tz=tz, batch_rows=2L),
        warning=function(w) {
          warnings <<- c(warnings, conditionMessage(w))
          invokeRestart("muffleWarning")
        })
      list(value=value, warnings=warnings)
    }
    csv_run <- capture_edge("csv")
    col_run <- capture_edge("columnar")
    csv <- csv_run$value
    col <- col_run$value
    expected_warning <- prec == "native" && tz == "Europe/Copenhagen"
    for (w in list(csv_run$warnings, col_run$warnings)) {
      if (expected_warning) {
        stopifnot(length(w) >= 1L, all(grepl("converted to NA", w, fixed=TRUE)))
      } else stopifnot(!length(w))
    }
    if (expected_warning) {
      stopifnot(all(is.na(col$wall_clock[1:2])),
                all(is.na(csv$wall_clock[1:2])))
      for (warnings in list(csv_run$warnings, col_run$warnings)) {
        stopifnot(any(grepl("Row(s): 1", warnings, fixed=TRUE)),
                  any(grepl("Row(s): 2", warnings, fixed=TRUE)),
                  any(grepl("ambiguous", warnings, fixed=TRUE)))
      }
    }
    compare_columns(csv, col)
    expected_unicode <- "\u00e6\u00f8\u00e5 \u6f22\u5b57 \U0001F600 cafe\u0301\r\n\"citat\", tab\t"
    if (!identical(col$unicode[[1L]], expected_unicode)) {
      cps <- function(s) paste(sprintf("U+%04X", utf8ToInt(enc2utf8(s))), collapse=" ")
      stop(sprintf("SCB2 Unicode fixture mismatch (precision=%s, tz=%s).\nExpected code points: %s\nActual code points:   %s",
                   prec, tz, cps(expected_unicode), cps(col$unicode[[1L]])),
           call.=FALSE)
    }
    stopifnot(is.character(col$sql_bigint),
              identical(col$sql_bigint[1], "-9223372036854775808"),
              identical(col$sql_bigint[2], "9223372036854775807"),
              identical(col$sql_int[1], -2147483648),
              identical(col$nullable_unicode[1], "NA"),
              identical(col$nullable_unicode[2], ""),
              is.na(col$nullable_unicode[3]),
              is.null(col$null_binary[[1]]),
              identical(col$binary_bytes[[2]], raw()),
              identical(col$binary_bytes[[1]], as.raw(c(0,1,34,44,10,13,255))),
              identical(col$unicode[2], ""),
              identical(col$unicode[1], "\u00e6\u00f8\u00e5 \u6f22\u5b57 \U0001F600 cafe\u0301\r\n\"citat\", tab\t"))
    if (prec=="full") {
      stopifnot(identical(col$precise[1], "9999999999999999999999999999.1234567890"),
                identical(col$precise[2], "-9999999999999999999999999999.1234567890"),
                identical(col$near_zero[1], "0.00000000000000000000000000000000000001"),
                grepl("2026-03-29T02:30:00", col$wall_clock[1], fixed=TRUE),
                grepl("2026-10-25T02:30:00", col$wall_clock[2], fixed=TRUE))
    } else {
      stopifnot(is.numeric(col$precise), inherits(col$wall_clock,"POSIXct"),
                inherits(col$absolute_clock,"POSIXct"),
                inherits(col$clock_time,"difftime"),
                identical(attr(col$absolute_clock,"tzone"), "UTC"))
      # 2000-01-01 12:00 at +14 => 1999-12-31 22:00 UTC;
      # 2000-01-01 12:00 at -12 => 2000-01-02 00:00 UTC.
      utc <- function(s) as.numeric(as.POSIXct(s, tz="UTC"))
      stopifnot(abs(as.numeric(col$absolute_clock[3])-utc("1999-12-31 22:00:00")) < 1e-6,
                abs(as.numeric(col$absolute_clock[5])-utc("2000-01-02 00:00:00")) < 1e-6)
    }
    csv_timing_names <- names(sql_timings(csv))
    col_timing_names <- names(sql_timings(col))
    if (!identical(csv_timing_names, col_timing_names)) {
      stop(sprintf(
        "Timing schemas differ (precision=%s, tz=%s). CSV-only: %s; columnar-only: %s",
        prec, tz,
        paste(setdiff(csv_timing_names, col_timing_names), collapse=", "),
        paste(setdiff(col_timing_names, csv_timing_names), collapse=", ")
      ), call.=FALSE)
    }
    stopifnot(identical(sql_timings(csv)$stream_callback_seconds, 0),
              identical(sql_timings(col)$stream_callback_seconds, 0),
              all(is.na(sql_type_timings(csv)$post_decode_seconds)),
              is.data.frame(sql_type_timings(col)))
  }
}
# SQL bigint minimum now becomes NA_integer64_ with a warning.
warns <- character()
result <- withCallingHandlers(sql_edge_types_test(result_format="columnar", precision="native",
                                    bigint="integer64", tz="UTC"),
                warning=function(w) {warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning")})
stopifnot(is.na(result$sql_bigint[1]),
          any(grepl("-9223372036854775808", warns, fixed=TRUE)))
message("PASS: v0.0.24 edge types, DST, Unicode, NULL, binary and metadata")
