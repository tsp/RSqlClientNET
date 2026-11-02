test_that("nonexistent DST wall time becomes NA with one warning", {
  parser <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")
  x <- c("2026-03-29T02:30:00.1234567",
         "2026-03-29T03:30:00.1234567", NA_character_)
  for (engine in c("lubridate", "base")) {
    warnings <- character()
    result <- withCallingHandlers(
      parser(x, tz = "Europe/Copenhagen", datetime_parser = engine),
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      })
    expect_s3_class(result, "POSIXct")
    expect_true(is.na(result[1]))
    expect_false(is.na(result[2]))
    expect_true(is.na(result[3]))
    expect_length(warnings, 1L)
    expect_match(warnings, "converted to NA", fixed = TRUE)
  }
})

test_that("datetimeoffset with legal UTC offset remains unambiguous", {
  parser <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")
  z <- parser("2026-03-29T03:30:00.1234567+02:00", with_offset = TRUE,
              tz = "Europe/Copenhagen")
  expect_false(is.na(z))
  expect_identical(attr(z, "tzone"), "UTC")
})

test_that("ambiguous Copenhagen fall-back hour gives NA and exact row numbers", {
  parser <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")
  stamps <- c("2026-10-25T01:59:59.1234567",
              "2026-10-25T02:30:00.7654321",
              "2026-10-25T03:00:00.7654321",
              "2026-10-25T02:00:00.0000000",
              NA_character_)
  for (engine in c("base", "lubridate")) {
    w <- character()
    parsed <- withCallingHandlers(
      parser(stamps, tz="Europe/Copenhagen", datetime_parser=engine,
             row_offset=100, column_name="event_time"),
      warning=function(e) {
        w <<- c(w, conditionMessage(e))
        invokeRestart("muffleWarning")
      })
    expect_s3_class(parsed, "POSIXct")
    expect_identical(is.na(parsed), c(FALSE, TRUE, FALSE, TRUE, TRUE))
    expect_length(w, 1L)
    expect_match(w, "ambiguous", fixed=TRUE)
    expect_match(w, "event_time", fixed=TRUE)
    expect_match(w, "Row(s): 102, 104", fixed=TRUE)
    expect_match(w, "converted to NA", fixed=TRUE)
  }
})

test_that("America/New_York overlap is ambiguous but explicit offset is not", {
  parser <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")
  w <- character()
  parsed <- withCallingHandlers(
    parser(c("2026-11-01T00:30:00", "2026-11-01T01:30:00",
             "2026-11-01T02:30:00"), tz="America/New_York",
           column_name="stamp"),
    warning=function(e) {w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning")})
  expect_identical(is.na(parsed), c(FALSE, TRUE, FALSE))
  expect_length(w, 1L)
  expect_match(w, "Row(s): 2", fixed=TRUE)
  instant <- parser(c("2026-11-01T01:30:00-04:00",
                      "2026-11-01T01:30:00-05:00"), with_offset=TRUE,
                    tz="America/New_York")
  expect_false(anyNA(instant))
  expect_equal(as.numeric(difftime(instant[2], instant[1], units="hours")), 1)
})

test_that("UTC is unambiguous and original full-precision values are untouched", {
  parser <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")
  convert <- getFromNamespace(".sql_convert_column", "RSqlClientNET")
  wall <- "2026-10-25T02:30:00.7654321"
  expect_no_warning(parser(wall, tz="UTC"))
  unchanged <- convert(wall, "datetime2", bigint="character",
                       datetime="character", binary="raw", unsupported="error",
                       precision="full", tz="Europe/Copenhagen")
  expect_identical(unchanged, wall)
})

test_that("columnar batch warnings show absolute row offsets", {
  cast <- getFromNamespace(".sql_columnar_cast", "RSqlClientNET")
  batch <- data.table::data.table(
    stamp=c("2026-10-25T01:30:00", "2026-10-25T02:30:00", "2026-10-25T03:30:00"))
  w <- character()
  out <- withCallingHandlers(cast(batch, types="datetime2", expected_kinds="S",
    bigint="character", datetime="POSIXct", binary="raw", unsupported="error",
    precision="native", tz="Europe/Copenhagen", datetime_parser="lubridate",
    row_offset=32768), warning=function(e) {
      w <<- c(w, conditionMessage(e)); invokeRestart("muffleWarning")
    })
  expect_identical(is.na(out$data$stamp), c(FALSE, TRUE, FALSE))
  expect_length(w, 1L)
  expect_match(w, "Row(s): 32770", fixed=TRUE)
})
