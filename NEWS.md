# RSqlClientNET 0.1.0

Initial release of the DBI backend for Microsoft SQL Server using Microsoft.Data.SqlClient, a persistent .NET helper and Rcpp native binary transport.

## Features

- DBI connections, parameterized queries, result cursors, transactions, metadata, SQL Server local temporary tables, and optional dbplyr integration.
- High-throughput SBIN/SqlBulkCopy writes and SCB2 columnar reads with automatic reduction of batches exceeding the 128 MiB frame cap.
- Configurable SQL data conversions (BIGINT, decimal, binary, XML, datetime and timezone/DST handling).
- Windows, Linux and macOS source-build tooling, with optional SQL Server and DBItest conformance tests.

## Bulk-write defaults

- **`table_lock = TRUE` is now the default** for `DBI::dbWriteTable()` and `DBI::dbAppendTable()`; both request `SqlBulkCopyOptions.TableLock` on each bulk upload.
- Use `table_lock = FALSE` per operation when avoiding a bulk table lock is important for concurrency. This does not guarantee absence of other SQL Server locks.
- The default binary-upload row batch size remains 65,536 (independent of the 32,768-row DBI read batch default).
- Table locking may reduce SQL Server transaction-log use on eligible targets under SIMPLE/BULK_LOGGED recovery; logging is not guaranteed to be minimal.

## Benchmarks and validation

- The development SQL Server 2025 Express / SIMPLE recovery example using 10,000 rows showed append without/with TableLock: 3.99/1.11 s (non-indexed), 4.08/1.29 s (indexed). Corresponding logical transaction-log byte measurements were ~106.7/1.09 MB and ~106.8/0.57 MB. These are single-run, workload-dependent examples, not performance guarantees.
- Benchmarks retain explicit locked/unlocked variants; the historical `rsql_standard` label in the transaction benchmark represents `table_lock = FALSE`, not the package default.
- Historical regression tests and reproducible developer benchmarks remain in the source tree. Two obsolete stand-alone examples have been removed.

## Dependency licenses and security

- Bundle full Microsoft SNI 7.1.0 license terms and third-party dependency notices
  separately from the project MIT license.
- Pin `System.Security.Cryptography.Pkcs` to patched version 8.0.2 instead of the
  previously bundled 8.0.1; confirm the resolved version in the final Windows binary.

## Known limits

- A SQL Server connection and .NET runtime are required for database use; installing from source requires the .NET SDK and C++17 toolchain.
- A single SQL row exceeding 128 MiB cannot be sent through the binary frame transport.
- For limitations, supported type conversions, concurrency considerations and platform prerequisites, see `README.md` and `docs/DBI-COMPATIBILITY.md`.
