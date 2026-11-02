# RSqlClientNET

**DBI backend for Microsoft SQL Server using `Microsoft.Data.SqlClient` and .NET, without ODBC.**

> **AI development disclosure:** RSqlClientNET was **vibe-coded with GPT-6 SOL – high** (GPT-6 with high reasoning effort), under the direction of Tonny Studsgaard Petersen, who maintains and tests the project. AI-generated code can contain defects, including security defects; assess the package for your own use case.

[![R CMD check](https://github.com/tsp/RSqlClientNET/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tsp/RSqlClientNET/actions/workflows/R-CMD-check.yaml)

RSqlClientNET provides a persistent .NET helper and Rcpp binary transport for parameterized SQL queries, DBI results and cursors, transactions, SQL Server temporary tables and `SqlBulkCopy` uploads. The maintainer has tested development builds on Windows, Linux (Ubuntu/WSL2) and macOS (Apple Silicon). Intel macOS awaits CI verification.

## Requirements

- **Running the package:** R >= 4.1 and a supported installation of the **.NET runtime 8 or newer**. The helper targets `net8.0` with **`RollForward=LatestMajor`**: it uses the newest installed compatible major runtime (for example .NET 10, even when .NET 8 is also installed). No need to set `DOTNET_ROLL_FORWARD` manually. .NET 10 LTS is recommended; .NET 8 support ends on 10 November 2026.
- **Installing from source:** a C++17 toolchain, .NET SDK 8 or newer and access to required NuGet packages (or a populated cache). Normal R/toolchain setup is not described here.
- **Windows binary:** compiled for R 4.6.x; another R minor release requires its own binary or a source build. It includes the published helper and does not require Rtools or a .NET SDK at install time.

## Installation

**Windows (R 4.6.x):** download `RSqlClientNET_0.1.0.zip` from [GitHub Releases](https://github.com/tsp/RSqlClientNET/releases) **when the Windows binary has been published**. Install its R package dependencies and then the downloaded ZIP:

```r
install.packages(c("DBI", "Rcpp", "data.table", "jsonlite", "processx", "bit64", "lubridate"))
install.packages("path/to/RSqlClientNET_0.1.0.zip", repos = NULL, type = "win.binary")
```

**Source (Windows, Linux or macOS):** with the source-build prerequisites above:

```r
install.packages("remotes")  # If not already installed
remotes::install_github("tsp/RSqlClientNET")
```

The .NET helper is built during source installation. GitHub is the distribution channel for this first release; the package is not on CRAN.

## Usage

```r
library(DBI)
library(RSqlClientNET)

# Set SQLSERVER_CONNECTION_STRING in your environment; do not hard-code secrets.
con <- dbConnect(
  SqlClientNET(),
  connection_string = Sys.getenv("SQLSERVER_CONNECTION_STRING"),
  tz = "Europe/Copenhagen"  # fast binary datetime, preserving local wall-clock semantics
)

print(dbGetQuery(con, "SELECT @number AS number", params = list(number = 42L)))
dbDisconnect(con)
```

Run `sql_ping()` to test **local R ↔ .NET helper communication only**; it does not test SQL Server connectivity. Use `SqlClientNET()`, not the unexported prototype constructor `SQLServer()`.

### Fast result formats and `data.table`

DBI reads use the native binary columnar transport by default and return a
`data.frame` for DBI compatibility. To return a `data.table` directly and
avoid the final conversion, set `data_table = TRUE` on the connection (or
override it for an individual `dbGetQuery()` call):

```r
con <- dbConnect(SqlClientNET(),
                 connection_string = Sys.getenv("SQLSERVER_CONNECTION_STRING"),
                 data_table = TRUE)
dt <- dbGetQuery(con, "SELECT TOP (1000) * FROM dbo.MyTable")
stopifnot(data.table::is.data.table(dt))
dbDisconnect(con)
```

The direct `sql_query()` API now also defaults to the binary `columnar`
format. CSV is available explicitly with `result_format = "csv"` or
`sql_default_format("csv")` (applies to direct queries only). Direct
`sql_query()` returns a `data.table` regardless of the DBI option.

DBI read batch size defaults to 32,768 rows (`batch_rows` in `dbConnect()`),
independently of bulk-upload batch sizing. Optional short benchmark:
`Rscript tools/benchmarks/read-batch-size-benchmark.R` (requires a test
SQL Server connection; see [benchmarks](tools/benchmarks/README.md)).

### High-throughput writes (`table_lock = TRUE` by default)

`dbWriteTable()` and `dbAppendTable()` use the same binary SBIN / `SqlBulkCopy` path. **Both request `SqlBulkCopyOptions.TableLock` by default** to improve bulk-upload throughput. This may substantially reduce transaction logging when SQL Server's minimal-logging conditions are met.

```r
dbWriteTable(con, "#staging", data.frame(id = 1:3))  # table_lock = TRUE
dbAppendTable(con, "#staging", data.frame(id = 4:6)) # table_lock = TRUE

# Opt out on an individual operation if concurrent access is important:
dbAppendTable(con, "#staging", data.frame(id = 7:9), table_lock = FALSE)
```

`table_lock = TRUE` can block other operations, particularly concurrent writers. Set `table_lock = FALSE` for workloads where this is a concern; SQL Server still manages normal locking and may escalate locks. A bulk table lock does **not** guarantee minimal logging: recovery model, indexes, table state, constraints and other SQL Server conditions matter. Standard DBI transaction/rollback semantics are unchanged. See [benchmark documentation](tools/benchmarks/README.md).

## Data handling and limitations

- Native SQL `datetime`, `datetime2` and `smalldatetime` reads use binary wall-clock ticks for `tz = ""`, `tz = "UTC"` and named IANA zones; timezone offsets are resolved in batches. Nonexistent/ambiguous local times become `NA` with warnings by default; `dst_fallback = "ordered"` is available for chronologically ordered rows. `precision = "full"` preserves exact source text. `datetime_parser` only applies to text/CSV parsing.
- Conversion options are available for SQL decimals, BIGINT, binary values and timestamps. The SQL BIGINT minimum (`-2^63`) is reserved as `NA` by `bit64::integer64`; select character conversion to preserve it exactly.
- Binary bulk uploads **and binary columnar reads** automatically split batches to stay within the 128 MiB frame limit. `batch_rows = 32768` is a row-count ceiling, not a guarantee that each frame contains that many rows; wide text/XML/VARBINARY results may use smaller frames. An individual SQL row larger than 128 MiB cannot be transmitted in this format. Local `#temp` tables belong to the SQL Server connection that created them.
- One active DBI result per connection; clear results when finished (`dbClearResult()`).
- SQL Server dialect differences and Arrow timestamp limitations are described in [DBI compatibility](docs/DBI-COMPATIBILITY.md).

See the [benchmark vignette](vignettes/benchmarks.Rmd) for a reproducible
SQL Server comparison with `odbc` and the older RODBC backends.

See [NEWS](NEWS.md) for the release history, [contributor instructions](docs/DEVELOPING.md) for testing, and [third-party licenses](docs/THIRD_PARTY.md) for redistribution notes.

## License, provenance and contributions

Copyright © 2026 Tonny Studsgaard Petersen (`tsp@person.dk`). Licensed under [MIT](LICENSE.md). The AI-assisted development process and the limits of the source review are described in [PROVENANCE](docs/PROVENANCE.md). Please report issues through [GitHub Issues](https://github.com/tsp/RSqlClientNET/issues).

## Third-party licenses

The bundled Windows .NET helper includes Microsoft's native SQL Server SNI runtime.
It is governed by separate Microsoft Software License Terms, **not the project's MIT
license**. The full SNI terms and consolidated NuGet dependency notices are included
under `inst/LICENSES/` (installed as `LICENSES/`). See `docs/THIRD_PARTY.md`.
