# Historical RSqlClientNET regression scripts

These 35 scripts are retained as regression reference tests. They are not
executed directly by `R CMD check`. Instead, each is invoked from exactly
one `testthat::test_that()` block in `../testthat/`.

Run from the package root using `devtools::test()` or `testthat::test_local()`.

- **unit:** native Rcpp decoder, DBI methods and R conversion, without .NET or SQL Server.
- **helper:** .NET/C#/Rcpp synthetic integration, without SQL Server. Requires .NET SDK 8+.
- **SQL Server:** real SQL Server integration, requires the environment variable
  `RSQLCLIENTNET_TEST_CONNECTION_STRING`. Tests use session-local `#temp` tables.

To run an individual historical test manually from the package root:
`source("tests/legacy/edge-types-024.R")`.

`optional-sql-server-024.R` was updated for recoverable SQL syntax errors:
the connection and session-local temporary table must survive a syntax error.
