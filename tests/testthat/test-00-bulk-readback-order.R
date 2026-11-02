# Database tables do not have inherent row order. This reproduces the
# alignment rule used by the live SqlBulkCopy roundtrip integration test.
test_that("bulk readback is compared by unique ID, not physical row order", {
  expected <- data.frame(id = c(1L, 2L, 3L),
                         text = c("æøå 🐱", "", NA_character_),
                         stringsAsFactors = FALSE, check.names = FALSE)
  readback <- expected[c(3L, 1L, 2L), , drop = FALSE]
  expect_false(identical(readback$text, expected$text))
  expect_identical(sort(readback$id), sort(expected$id))
  aligned <- readback[order(readback$id), , drop = FALSE]
  expect_identical(aligned$text, expected$text)
  # A genuine data corruption must still fail, including NA vs literal "NA".
  aligned$text[[3L]] <- "NA"
  expect_false(identical(aligned$text, expected$text))
  aligned$text[[3L]] <- NA_character_
  aligned$text[[1L]] <- "æøå cat"
  expect_false(identical(aligned$text, expected$text))
})
