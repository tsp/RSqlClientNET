test_that("text BIGINT minimum is converted to NA_integer64_ with one informative warning", {
  convert <- getFromNamespace(".sql_convert_column", "RSqlClientNET")
  src <- c("42", NA_character_, "-9223372036854775808", "-9223372036854775807",
           "-9223372036854775808")
  warnings <- character()
  x <- withCallingHandlers(convert(src, "bigint", bigint="integer64",
    datetime="POSIXct", binary="raw", unsupported="error", precision="native",
    row_offset=100L, column_name="test_bigint"),
    warning=function(w) { warnings <<- c(warnings, conditionMessage(w))
                           invokeRestart("muffleWarning") })
  expect_s3_class(x, "integer64")
  expect_identical(as.character(x[1]), "42")
  expect_true(all(is.na(x[c(2,3,5)])))
  expect_identical(as.character(x[4]), "-9223372036854775807")
  expect_length(warnings, 1L)
  expect_match(warnings, "2 SQL BIGINT", fixed=TRUE)
  expect_match(warnings, "test_bigint", fixed=TRUE)
  expect_match(warnings, "Row(s): 103, 105", fixed=TRUE)
  expect_match(warnings, "bigint='character'", fixed=TRUE)
  expect_identical(convert(src, "bigint", bigint="character", datetime="POSIXct",
      binary="raw", unsupported="error", precision="native"), src)
})

test_that("SCB2 decoder preserves NULL distinction in loss warning metadata", {
  le32 <- function(v) writeBin(as.integer(v), raw(), size=4L, endian="little")
  le64 <- function(v) writeBin(unclass(bit64::as.integer64(v)), raw(), size=8L, endian="little")
  min_bytes <- as.raw(c(rep(0L, 7L), 0x80L))
  # Three rows: SQL minimum, SQL 42, SQL NULL.
  wire <- c(charToRaw("SCB2"), le32(3L), le32(1L), charToRaw("L"),
            as.raw(0x03L), min_bytes, le64("42"), as.raw(rep(0L, 8L)))
  decoded <- .Call("sqlcpp_decode_batch", wire, TRUE, "UTC", PACKAGE="RSqlClientNET")
  expect_s3_class(decoded$columns[[1]], "integer64")
  expect_identical(is.na(decoded$columns[[1]]), c(TRUE, FALSE, TRUE))
  expect_identical(as.character(decoded$columns[[1]][2]), "42")
  expect_identical(decoded$bigint_min_counts[[1L]], 1L)
  expect_identical(decoded$bigint_min_rows[[1L]], 1L)
  warn <- getFromNamespace(".sql_warn_decoded_bigint_min", "RSqlClientNET")
  warnings <- character()
  withCallingHandlers(warn(decoded, "amount", row_offset=8192L),
    warning=function(w) {warnings <<- c(warnings, conditionMessage(w))
                         invokeRestart("muffleWarning")})
  expect_length(warnings, 1L)
  expect_match(warnings, "amount", fixed=TRUE)
  expect_match(warnings, "Row(s): 8193", fixed=TRUE)
  expect_match(warnings, "converted to NA", fixed=TRUE)
  text_decoded <- .Call("sqlcpp_decode_batch", wire, FALSE, "UTC", PACKAGE="RSqlClientNET")
  expect_identical(text_decoded$columns[[1]], c("-9223372036854775808", "42", NA_character_))
  expect_true(all(text_decoded$bigint_min_counts == 0L))
})
