# unit regression tests — migrated from tests/legacy.

test_that("unit: columnar-frame.R", {
  expect_no_error(.rsqlclientnet_run_legacy("columnar-frame.R"))
})

# unit regression tests — migrated from tests/legacy.

test_that("unit: columnar-v2-native-frame.R", {
  expect_no_error(.rsqlclientnet_run_legacy("columnar-v2-native-frame.R"))
})

# unit regression tests — migrated from tests/legacy.

test_that("unit: datetime-parsers.R", {
  expect_no_error(.rsqlclientnet_run_legacy("datetime-parsers.R"))
})

# unit regression tests — migrated from tests/legacy.

test_that("unit: dbi-0281-class.R", {
  expect_no_error(.rsqlclientnet_run_legacy("dbi-0281-class.R"))
})

# unit regression tests — migrated from tests/legacy.

test_that("unit: dbi-0291-id-quote.R", {
  expect_no_error(.rsqlclientnet_run_legacy("dbi-0291-id-quote.R"))
})

# unit regression tests — migrated from tests/legacy.

test_that("unit: dbi-0292-dispatch.R", {
  expect_no_error(.rsqlclientnet_run_legacy("dbi-0292-dispatch.R"))
})

# unit regression tests — migrated from tests/legacy.

test_that("unit: dbi-write-signatures-0302.R", {
  expect_no_error(.rsqlclientnet_run_legacy("dbi-write-signatures-0302.R"))
})

# unit regression tests — migrated from tests/legacy.

test_that("unit: dbplyr-029-dialect.R", {
  skip_if_not_installed("dbplyr", minimum_version = "2.6.0")
  expect_no_error(.rsqlclientnet_run_legacy("dbplyr-029-dialect.R"))
})

# unit regression tests — migrated from tests/legacy.

test_that("unit: dotnet-dispose-0303.R", {
  expect_no_error(.rsqlclientnet_run_legacy("dotnet-dispose-0303.R"))
})

# unit regression tests — migrated from tests/legacy.

test_that("unit: precision-modes.R", {
  expect_no_error(.rsqlclientnet_run_legacy("precision-modes.R"))
})
