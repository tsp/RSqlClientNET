# Contributing and testing

Contributions and reproducible bug reports are welcome. A working R installation, C++17 toolchain and .NET SDK 8+ are assumed for source builds. The helper targets `net8.0` and uses `RollForward=LatestMajor`; test against both .NET 8 and newer runtimes where possible.

## Package checks

From the project root:

```r
source("tools/document.R")
source("tools/run-release-tests.R")
devtools::check(args = "--as-cran")
```

The release test runner performs offline tests and, **only when `RSQLCLIENTNET_TEST_CONNECTION_STRING` is set**, SQL Server integration and DBItest. Use a disposable database with permission to create and remove tables; do not commit credentials. Without a live SQL Server, `sql_ping()` verifies communication with the local .NET helper, not the server.

On macOS, `sh tools/test-macos.sh` provides an install-and-check workflow. For Linux and macOS, `configure` and `cleanup` must be executable (`chmod +x configure cleanup` if archive extraction removed the permissions). Source installation builds the helper with the .NET SDK.

Development scripts can install missing R test dependencies with `pak`. To disable this, set `RSQLCLIENTNET_AUTO_INSTALL_DEPS=0`. For checks, `DOTNET_EnableDiagnostics_IPC=0` can prevent .NET diagnostic sockets from appearing in monitored temporary directories.

## Binary and benchmark tools

On Windows with R 4.6.x, Rtools and the .NET SDK, run `source("tools/build-binary.R")` to build and smoke-test an installable binary ZIP. Alternatively use GitHub Actions → **Build Windows binary**. The separate release-upload step requires a review of the actual binary's transitive NuGet licenses, as described in [THIRD_PARTY](THIRD_PARTY.md).

Bulk uploads request a table lock by default (`table_lock = TRUE`). The regression suite must also exercise explicit `table_lock = FALSE`; the unlocked benchmark baseline is deliberately passed explicitly. Avoid judging production concurrency from single-writer benchmarks.

Benchmark tools and their measurement limits are documented in [`tools/benchmarks/README.md`](../tools/benchmarks/README.md). For SQL Server-specific test exclusions see [DBI compatibility](DBI-COMPATIBILITY.md).
