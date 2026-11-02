test_that("legacy Base64 process transport is rejected before any helper is launched", {
  validate <- getFromNamespace(".sql_transport", "RSqlClientNET")
  expect_identical(validate("rcpp"), "rcpp")
  expect_error(validate("base64"), "Only the native Rcpp")
  expect_error(validate("binary"), "Only the native Rcpp")
  expect_error(RSqlClientNET::sql_ping(transport="base64"), "Only the native Rcpp")
  expect_error(RSqlClientNET::sql_connect(transport="base64"), "Only the native Rcpp")
})

test_that("both result formats are available through native framed transport", {
  format <- getFromNamespace(".sql_result_format", "RSqlClientNET")
  expect_identical(format("csv", "rcpp"), "csv")
  expect_identical(format("columnar", "rcpp"), "columnar")
  expect_error(format("csv", "base64"), "Only the native Rcpp")
})
