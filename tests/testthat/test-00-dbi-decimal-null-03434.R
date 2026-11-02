# Regression: numeric NA must remain SQL NULL after decimal text conversion.
# Earlier format(numeric) serialized NA_real_ as literal "NA"; .NET's
# SqlDecimal.Parse("NA") then threw FormatException during SqlBulkCopy.
testthat::test_that("numeric DECIMAL and MONEY preserve SQL NULL in SBIN", {
  convert <- getFromNamespace(".dbi_bulk_atomic_values", "RSqlClientNET")
  payload <- getFromNamespace(".dbi_bulk_binary_payload", "RSqlClientNET")

  values <- c(1.125, NA_real_, -9.5, NA_real_)
  strings <- convert(values, "decimal")
  testthat::expect_type(strings, "character")
  testthat::expect_identical(is.na(strings), is.na(values))
  testthat::expect_equal(as.numeric(strings[c(1L, 3L)]), values[c(1L, 3L)])

  input <- data.frame(amount = values, money = values,
                      note = c("present", NA_character_, "present", NA_character_))
  encoded <- payload(input, c("decimal", "decimal", "text"), 1L, 4L)
  testthat::expect_identical(rawToChar(encoded[1:4]), "SBIN")
  # Native IEEE-double DECIMAL columns use kind 19 and preserve bitmap 0b1010.
  testthat::expect_identical(encoded[17L], as.raw(19L))
  testthat::expect_identical(encoded[18L], as.raw(10L))
  at <- 19L + 8L * 4L
  testthat::expect_identical(encoded[at], as.raw(19L))
  testthat::expect_identical(encoded[at + 1L], as.raw(10L))
  # Character decimal must still take the exact UTF-8 path (38 digits).
  exact <- payload(data.frame(x="1234567890123456789012345678.1234567890"),
                   "decimal", 1L, 1L)
  testthat::expect_identical(exact[17L], as.raw(6L))

  factor_values <- factor(c("1.25", NA_character_, "-3.50"))
  testthat::expect_identical(is.na(convert(factor_values, "decimal")),
                             is.na(factor_values))
  testthat::expect_error(convert(c(1, Inf), "decimal"), "NaN/Inf")
  testthat::expect_error(convert(c(1, -Inf), "decimal"), "NaN/Inf")
  testthat::expect_error(convert(c(1, NaN), "decimal"), "NaN/Inf")
  testthat::expect_error(payload(data.frame(amount = c(1, NaN)),
                                 "decimal", 1L, 2L), "NaN/Inf")
  testthat::expect_identical(convert(c(NA_real_, 1.25), "decimal"),
                             c(NA_character_, "1.25"))
})
