# helper regression tests — migrated from tests/legacy.

test_that("helper: binary-framing-regression.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("binary-framing-regression.R"))
})

test_that("helper: binary-transport.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("binary-transport.R"))
})

test_that("helper: columnar-batch-027.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("columnar-batch-027.R"))
})

test_that("helper: columnar-integration.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("columnar-integration.R"))
})

test_that("helper: columnar-v2-integration.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("columnar-v2-integration.R"))
})

test_that("helper: default-format-024.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("default-format-024.R"))
})

test_that("helper: edge-types-024.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("edge-types-024.R"))
})

test_that("helper: local-timestamp.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("local-timestamp.R"))
})

test_that("helper: parser-modes.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("parser-modes.R"))
})

test_that("helper: rcpp-native-transport.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("rcpp-native-transport.R"))
})

test_that("helper: rcpp-pipe-stats.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("rcpp-pipe-stats.R"))
})

test_that("helper: robustness-024.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("robustness-024.R"))
})

test_that("helper: stream-chunks.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("stream-chunks.R"))
})

test_that("helper: streaming-025.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("streaming-025.R"))
})

test_that("helper: streaming-026.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("streaming-026.R"))
})

test_that("helper: timeout-cancel.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("timeout-cancel.R"))
})

test_that("helper: timeout-invalidation.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("timeout-invalidation.R"))
})

test_that("helper: windows-binary-input.R", {
  .rsqlclientnet_skip_no_dotnet()
  expect_no_error(.rsqlclientnet_run_legacy("windows-binary-input.R"))
})
