# DBI and SQL Server compatibility

RSqlClientNET implements DBI operations for SQL Server and is tested against the official **DBItest 1.8.3** suite. The package also contains SQL Server-specific regression tests. The maintainer reported successful Windows, Ubuntu/WSL2 and macOS Apple Silicon release-candidate runs, including the SQL Server integration suite and DBItest. Test skips are **not** claims of conformance.

## Known SQL Server dialect differences

Some upstream DBItest checks assume SQL syntax or type rules that differ from T-SQL. The opt-in configuration is maintained in [`tests/testthat/test-30-dbitest.R`](../tests/testthat/test-30-dbitest.R) and is the authoritative list of exclusions. In particular:

- Large untyped numeric literals such as `10000000000` are `DECIMAL` in T-SQL, not necessarily `BIGINT`. The driver tests `BIGINT` conversion using explicitly typed SQL expressions; the `bit64::integer64` representation cannot distinguish SQL `-2^63` from missing values.
- SQL Server rejects unnamed columns in derived tables used by some generic NULL-quoting tests, and does not implement the ANSI `CURRENT_TIME` expression. Valid SQL Server syntax is exercised separately.
- SQL Server session-local `#temp` tables persist until dropped or the creating session ends. Some generic temporary-table lifecycle checks assume different behavior.
- Arrow APIs that fall back through R `POSIXct` cannot guarantee lossless round-trips for extended/nanosecond timestamps. The corresponding precision-specific tests are excluded pending a dedicated implementation; ordinary supported Rcpp timestamp operations are a separate path.

Other exclusions in the test configuration, including `package_name`, are test-harness assumptions rather than a claim of unsupported SQL Server operations.

## Reproducing the checks

Set `RSQLCLIENTNET_TEST_CONNECTION_STRING` to a **disposable SQL Server test database** (tests can create/drop objects), then run from the project root:

```r
source("tools/run-release-tests.R")
```

The full suite is opt-in and must run against a real server; ordinary GitHub checks without database credentials do **not** establish database integration compatibility. See [DEVELOPING](DEVELOPING.md) for contributor prerequisites. Results can depend on R, DBItest and SQL Server versions.
