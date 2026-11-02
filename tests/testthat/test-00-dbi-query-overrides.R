test_that("query option overrides are validated and do not mutate connection defaults", {
  options <- getFromNamespace(".dbi_query_options", "RSqlClientNET")
  base <- list(precision="native", bigint="integer64", datetime="POSIXct",
    datetime_automatic=TRUE, tz="UTC", dst_fallback="na",
    datetime_parser="base", binary="raw", unsupported="error",
    batch_rows=8192L, timeout=600, data_table=FALSE)
  exact <- options(base, list(precision="full", bigint="character"))
  expect_equal(exact$datetime, "character")
  expect_equal(exact$bigint, "character")
  expect_identical(base$datetime, "POSIXct")
  expect_identical(base$bigint, "integer64")
  ordered <- options(base, list(tz="Europe/Copenhagen", dst_fallback="ordered",
                                data_table=TRUE, batch_rows=2L))
  expect_identical(ordered$dst_fallback, "ordered")
  expect_identical(ordered$tz, "Europe/Copenhagen")
  expect_identical(ordered$data_table, TRUE)
  expect_identical(ordered$batch_rows, 2L)
  expect_error(options(base, list(dst_fallback="unspecified")))
  expect_error(options(base, list(tz="Not/AZone")))
  expect_error(options(base, list(unexpected="unused")), "Unsupported")
  expect_error(options(base, structure(list("native"), names="")), "uniquely named")
  expect_error(options(base, list(batch_rows=32769L)), "batch_rows")
  expect_equal(options(base, list(datetime=NULL,precision="full"))$datetime,"character")
  fixed <- base; fixed$datetime_automatic <- FALSE
  expect_equal(options(fixed,list(precision="full"))$datetime,"POSIXct")
})
