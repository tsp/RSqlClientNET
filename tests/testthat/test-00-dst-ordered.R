test_that("ordered DST fallback defaults to NA and may opt in", {
  parse <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")
  x <- c("2026-10-25T01:59:59", "2026-10-25T02:10:00",
         "2026-10-25T02:40:00", "2026-10-25T02:05:00",
         "2026-10-25T02:20:00", "2026-10-25T03:00:00")
  for (engine in c("base", "lubridate")) {
    w <- character()
    y <- withCallingHandlers(parse(x, tz="Europe/Copenhagen",
        datetime_parser=engine, dst_fallback="ordered", column_name="stamp"),
        warning=function(c) {w <<- c(w, conditionMessage(c)); invokeRestart("muffleWarning")})
    expect_false(anyNA(y))
    expect_true(all(diff(as.numeric(y)) > 0))
    expect_equal(as.numeric(difftime(y[4], y[3], units="mins")), 25)
    expect_true(any(grepl("resolved using row order", w, fixed=TRUE)))
    expect_true(any(grepl("stamp", w, fixed=TRUE)))
    expect_true(all(grepl("Row(s): 2, 3, 4, 5", w, fixed=TRUE)))
    # Default behavior stays conservative.
    expect_warning(z <- parse(x, tz="Europe/Copenhagen"), "converted to NA")
    expect_true(all(is.na(z[2:5])))
  }
})

test_that("ordered DST fallback retains state between columnar batches", {
  cast <- getFromNamespace(".sql_columnar_cast", "RSqlClientNET")
  state <- new.env(parent=emptyenv())
  run <- function(x, offset) withCallingHandlers(cast(
    data.table::data.table(stamp=x), types="datetime2", expected_kinds="S",
    bigint="character", datetime="POSIXct", binary="raw", unsupported="error",
    precision="native", tz="Europe/Copenhagen", datetime_parser="base",
    row_offset=offset, dst_fallback="ordered", dst_states=state),
    warning=function(e) invokeRestart("muffleWarning"))$data$stamp
  first <- run(c("2026-10-25T02:30:00", "2026-10-25T02:59:59"),0)
  second <- run(c("2026-10-25T02:00:00", "2026-10-25T02:30:00"),2)
  expect_true(all(diff(as.numeric(c(first,second))) > 0))
  expect_equal(as.numeric(difftime(second[1], first[2], units="secs")), 1)
  # The policy does not convert nonexistent spring times into real instants.
  expect_warning(z <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")(
    "2026-03-29T02:30:00", tz="Europe/Copenhagen", dst_fallback="ordered"),
    "converted to NA")
  expect_true(is.na(z))
})

test_that("datetimeoffset is not subjected to DST row-order guessing", {
  parse <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")
  actual <- parse(c("2026-10-25T02:30:00+02:00",
                    "2026-10-25T02:30:00+01:00"),
                  with_offset=TRUE, tz="Europe/Copenhagen", dst_fallback="ordered")
  expect_equal(as.numeric(difftime(actual[2], actual[1], units="hours")),1)
})
