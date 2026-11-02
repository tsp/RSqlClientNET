test_that("binary wall-clock parser agrees with existing text parser outside transitions", {
  direct <- getFromNamespace(".sql_binary_wall_to_posixct", "RSqlClientNET")
  textual <- getFromNamespace(".sql_native_timestamp", "RSqlClientNET")
  strings <- c("2026-01-10T12:34:56.1234567", "2026-07-10T12:34:56.1234567",
               "2026-10-26T11:12:13.4567890", NA_character_)
  wall <- as.numeric(as.POSIXct(strings, format="%Y-%m-%dT%H:%M:%OS", tz="UTC"))
  for (tz in c("", "Europe/Copenhagen", "America/New_York", "Asia/Kolkata")) {
    got <- direct(wall, tz=tz)
    expected <- textual(strings, tz=tz)
    expect_equal(as.numeric(got), as.numeric(expected), tolerance=1e-5)
    expect_identical(attr(got, "tzone"), tz)
  }
})

test_that("binary local DST gaps and folds are NA, preserving SQL wall times", {
  direct <- getFromNamespace(".sql_binary_wall_to_posixct", "RSqlClientNET")
  wall <- as.numeric(as.POSIXct(c("2026-03-29 02:30:00", "2026-10-25 02:30:00",
                                "2026-10-25 03:30:00"), tz="UTC"))
  msgs <- character()
  x <- withCallingHandlers(direct(wall, tz="Europe/Copenhagen"),
    warning=function(w) {msgs <<- c(msgs, conditionMessage(w));invokeRestart("muffleWarning")})
  expect_true(is.na(x[1]))
  expect_true(is.na(x[2]))
  expect_false(is.na(x[3]))
  expect_true(any(grepl("nonexistent", msgs)))
  expect_true(any(grepl("ambiguous", msgs)))
})

test_that("binary ordered fallback works across batches", {
  direct <- getFromNamespace(".sql_binary_wall_to_posixct", "RSqlClientNET")
  at <- as.numeric(as.POSIXct(c("2026-10-25 01:59:59", "2026-10-25 02:30:00",
      "2026-10-25 02:00:00", "2026-10-25 02:30:00", "2026-10-25 03:00:00"), tz="UTC"))
  state <- new.env(parent=emptyenv())
  one <- suppressWarnings(direct(at[1:2], tz="Europe/Copenhagen",
    dst_fallback="ordered", dst_state=state))
  two <- suppressWarnings(direct(at[3:4], tz="Europe/Copenhagen",
    dst_fallback="ordered", dst_state=state, row_offset=2))
  three <- suppressWarnings(direct(at[5], tz="Europe/Copenhagen",
    dst_fallback="ordered", dst_state=state, row_offset=4))
  expect_true(all(diff(c(as.numeric(one), as.numeric(two), as.numeric(three))) > 0))
})

test_that("native SQL local timestamps choose binary wire kind W", {
  kind <- getFromNamespace(".sql_wire_kind", "RSqlClientNET")
  types <- c("datetime", "datetime2", "smalldatetime", "datetimeoffset")
  expect_identical(kind(types, "native", "POSIXct", "Europe/Copenhagen"),
                   c("W", "W", "W", "O"))
  expect_identical(kind(types, "native", "POSIXct", ""),
                   c("W", "W", "W", "O"))
  expect_identical(kind(types, "native", "POSIXct", "UTC"),
                   c("T", "T", "T", "O"))
})


test_that("SCB2 W frame decodes ticks without text or timezone conversion", {
  u32 <- function(x) writeBin(as.integer(x), raw(), size=4L, endian="little")
  u64 <- function(x) writeBin(unclass(bit64::as.integer64(x)), raw(), size=8L,
                               endian="little")
  frame <- c(charToRaw("SCB2"), u32(2L), u32(1L), charToRaw("W"),
             as.raw(0x01), u64("639192836960000000"), u64("0"))
  d <- .Call("sqlcpp_decode_batch", frame, FALSE, "Europe/Copenhagen",
             PACKAGE="RSqlClientNET")
  expect_identical(as.character(d$kinds), "W")
  expect_true(is.numeric(d$columns[[1L]]))
  expect_false(inherits(d$columns[[1L]], "POSIXct"))
  expect_equal(d$columns[[1L]][1L],
    as.numeric(as.POSIXct("2026-07-10 12:34:56", tz="UTC")))
  expect_true(is.na(d$columns[[1L]][2L]))
  expect_error(.Call("sqlcpp_decode_batch", frame, FALSE, "UTC",
    PACKAGE="RSqlClientNET"), "non-UTC")
})
