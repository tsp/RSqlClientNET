testthat::test_that("direct SCB2 chunks assemble VARBINARY list columns with NULL and empty values", {
  chunks <- list(
    list(value = list(as.raw(c(0x00, 0xA5)), NULL, raw(0))),
    list(value = list(as.raw(c(0x01, 0x02)), NULL, raw(0)))
  )
  assembled <- data.table::rbindlist(chunks, use.names=TRUE, fill=FALSE)
  testthat::expect_s3_class(assembled, "data.table")
  testthat::expect_identical(names(assembled), "value")
  testthat::expect_equal(nrow(assembled), 6L)
  testthat::expect_identical(assembled$value[[1L]], as.raw(c(0x00, 0xA5)))
  testthat::expect_null(assembled$value[[2L]])
  testthat::expect_identical(assembled$value[[3L]], raw(0))
  testthat::expect_identical(assembled$value[[4L]], as.raw(c(0x01, 0x02)))
  testthat::expect_null(assembled$value[[5L]])
})

testthat::test_that("direct SCB2 chunks assemble XML string columns and SQL NULL", {
  chunks <- list(
    list(doc = c("<x>æ</x>", NA_character_)),
    list(doc = c("<x>漢字</x>", ""))
  )
  assembled <- data.table::rbindlist(chunks, use.names=TRUE, fill=FALSE)
  testthat::expect_equal(nrow(assembled), 4L)
  testthat::expect_identical(assembled$doc,
                             c("<x>æ</x>", NA_character_, "<x>漢字</x>", ""))
})
