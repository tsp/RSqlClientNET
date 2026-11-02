# SQL Server regression tests — migrated from tests/legacy.

test_that("SQL Server: dbi-028.R", {
  .rsqlclientnet_skip_no_sql()
  expect_no_error(.rsqlclientnet_run_legacy("dbi-028.R"))
})

# SQL Server regression tests — migrated from tests/legacy.

test_that("SQL Server: dbi-write-030.R", {
  .rsqlclientnet_skip_no_sql()
  expect_no_error(.rsqlclientnet_run_legacy("dbi-write-030.R"))
})

# SQL Server regression tests — migrated from tests/legacy.

test_that("SQL Server: dbplyr-029.R", {
  .rsqlclientnet_skip_no_sql()
  skip_if_not_installed("dbplyr", minimum_version = "2.6.0")
  skip_if_not_installed("dplyr")
  expect_no_error(.rsqlclientnet_run_legacy("dbplyr-029.R"))
})

# SQL Server regression tests — migrated from tests/legacy.

test_that("SQL Server: sql-error-recovery-0301.R", {
  .rsqlclientnet_skip_no_sql()
  expect_no_error(.rsqlclientnet_run_legacy("sql-error-recovery-0301.R"))
})

# SQL Server regression tests — migrated from tests/legacy.

test_that("SQL Server: optional-sql-server-024.R", {
  .rsqlclientnet_skip_no_sql()
  expect_no_error(.rsqlclientnet_run_legacy("optional-sql-server-024.R"))
})

# SQL Server regression tests — migrated from tests/legacy.

test_that("SQL Server: optional-stream-sql-025.R", {
  .rsqlclientnet_skip_no_sql()
  expect_no_error(.rsqlclientnet_run_legacy("optional-stream-sql-025.R"))
})

# SQL Server regression tests — migrated from tests/legacy.

test_that("SQL Server: optional-stream-sql-026.R", {
  .rsqlclientnet_skip_no_sql()
  expect_no_error(.rsqlclientnet_run_legacy("optional-stream-sql-026.R"))
})
