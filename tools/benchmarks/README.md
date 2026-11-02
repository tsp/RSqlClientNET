# Benchmarks (0.1.0)

All database benchmarks are opt-in, require a disposable test database, and may create/drop their own test tables. Never run them on production databases. Specify `RSQLCLIENTNET_TEST_CONNECTION_STRING`, `ODBC_CONNECTION_STRING`, or a local `con_str` as documented in each script. Diagnostics and data generation are excluded from the timed intervals wherever practical.

## Core benchmarks

- `benchmark-bulk-transactions.R`: four-mode same-table, same-data test: RSqlClientNET `table_lock = FALSE` and `TRUE` (the historical `rsql_standard` label means `FALSE`, **not** the 0.1.0 default), odbc default, and odbc explicit transaction. Reports optional logical log bytes measured inside a **separate** explicit transaction, plus SQL Server CPU/waits and database-wide physical I/O. On SQL Server 2022+ these DMV measurements may require `VIEW SERVER PERFORMANCE STATE`; missing permission yields `NA`.
- `benchmark-large-writes-same-table.R`: focused RSqlClientNET vs odbc comparison, same destination table, SQL type and NULL mask; uses explicit `table_lock = FALSE` for the RSqlClientNET non-locking baseline.
- `benchmark-create-vs-append.R`: compares `dbWriteTable()` (CREATE + bulk copy in one transaction) to `dbCreateTable()` plus `dbAppendTable()` on a previously created table. Covers both `table_lock` settings and reports the create and append times separately. Not a substitute for the same-table comparisons.
- `benchmark_dbi.R` and `run_benchmark_overnight.R`: general DBI type/performance comparisons across **both session-local temporary (`#br_...`) and ordinary permanent (`br_...`) tables**. Includes writes, reads, and append to pre-created tables with and without `table_lock`. The timed `dbWriteTable` (create + write, now with default TableLock) and read comparisons are preserved. The overnight runner enables both table targets and append modes by default; direct `benchmark_dbi()` retains the original temporary-only default.
- `flights-vignette-benchmark.R`, `flights-batch-size-benchmark.R`, `read-batch-size-benchmark.R`, `profile-wide-reads.R`: reproducible real-world and wide-result checks.

Example:

```r
source("tools/benchmarks/benchmark-bulk-transactions.R")
x <- run_bulk_transaction_benchmark(n=10000L, repetitions=5L,
  types=c("varchar","unicode","binary"), dsn="SqlClientRTest",
  logical_log=TRUE)
x$summary
x$logical_log
x$recovery_model

source("tools/benchmarks/benchmark-create-vs-append.R")
y <- run_create_append_benchmark(n=10000L, repetitions=3L)
y$summary
```

**Interpretation:** TableLock is the default for RSqlClientNET 0.1.0, but may not suit workloads with concurrent writers. Minimally logged bulk insert needs suitable SQL Server conditions (e.g., appropriate recovery model, eligible target/index characteristics). `sys.dm_io_virtual_file_stats` captures database-wide bytes and can be affected by other sessions; use `sys.dm_tran_database_transactions` inside an active transaction for logical log generation. Always report variation across repeats, SQL Server version, recovery model and target definition.

Historical exploratory benchmarks for alternate XML accessors, `DbDataReader` streams, receive-buffer reuse and pre-materialized `DataTable` sources were removed because they did not produce consistent benefits. The production reader and 256 KiB Windows pipe defaults are retained.

## Overnight benchmark: temporary vs permanent tables and TableLock

Run on a disposable SQL Server test database. Use fresh output directory when
changing benchmark options (resume/checkpoints are configuration-specific).

```r
# Interactive R session with pre-existing connections:
benchmark_connections <- list(RSqlClientNET = con_r, odbc = con_o)
Sys.setenv(BENCH_MODE = "smoke",
           BENCH_TABLE_TYPES = "temporary,permanent",
           BENCH_APPEND_MODES = "append,append_table_lock",
           BENCH_OUTPUT = "tools/benchmarks/benchmark_results/smoke_table_types")
source("tools/benchmarks/run_benchmark_overnight.R")
benchmark_result$summary[, c("type", "driver", "table_type",
  "median_write_s", "median_read_s", "median_append_s",
  "median_append_table_lock_s", "speedup_table_lock_vs_append")]

# Compare identical scenarios using the same driver and input rows:
# (summaries are separate for each scenario / driver / table_type)
reshape(benchmark_result$summary[
  benchmark_result$summary$driver == "RSqlClientNET",
  c("scenario", "table_type", "median_write_s", "median_read_s")],
  idvar = "scenario", timevar = "table_type", direction = "wide")
```

`BENCH_APPEND_MODES=append,append_table_lock` (default): for every scenario,
add an **untimed table creation**, then a timed `dbAppendTable()` for both
RSqlClientNET and odbc, plus a timed RSqlClientNET-only append with
`table_lock=TRUE`. The RSqlClientNET `append` baseline explicitly sets
`table_lock=FALSE`, overriding the package default. The empty destination table is removed outside timing.
The create-and-write and read operations are measured for **each table type**.
All modes and table types use the same deterministic generated rows, schema and
NULL positions; only the table name and table storage type vary. Tables are
created in the same connected test database: `#br_...` for session-local
temporary tables in `tempdb`, `br_...` for ordinary tables in the user's
default schema. The latter are real database tables **only for the duration of
the test**. No special schema privileges (such as `dbo` ownership) are assumed,
but `CREATE TABLE` and `DROP TABLE` permission is required on the test database.
The two append variants use separately pre-created empty targets.

To disable the extra runs, set `BENCH_APPEND_MODES=none`. To run only the
TableLock append alongside the original baseline, set
`BENCH_APPEND_MODES=append_table_lock`, but for a meaningful ratio include
`append`. `details.csv` stores the added `operation` values `append` and
`append_table_lock`; `summary.csv` adds `median_append_s`,
`median_append_table_lock_s` and `speedup_table_lock_vs_append`. The ratio is
only computed where both modes have successful, positive timed results.

`BENCH_TABLE_TYPES=temporary,permanent` (default) doubles the scenarios and
evaluates the same write, append and read operations against both targets.
Use `BENCH_TABLE_TYPES=temporary` to reproduce the original overnight benchmark,
or `BENCH_TABLE_TYPES=permanent` to test normal tables only. `table_type`
is included in `details.csv` and `summary.csv`, in checkpoint identity, and in
completion keys, so resuming cannot confuse target types. Changing this or
another setting requires a new `BENCH_OUTPUT` directory (the new default
output folder name includes the selected table types). Results are checkpointed
after every operation; both target types will resume correctly even if the
benchmark is paused or a connection is recreated. Table names are unique and
normal completion/error handling drops them; an OS/process crash can leave
permanent `br_...` tables, which should be removed manually from the **test
database only** after verifying their identities.

**Important:** Table creation for append is outside its timing, but
`dbWriteTable()` still includes table creation, as in the original benchmark.
The `read` step reuses the last successful timed write when row counts match;
otherwise an untimed fixture is populated. Thus, `read` measures data retrieval,
not table creation. These benchmarks compare API/data transfer performance,
not production concurrency or logical-log bytes. Use
`benchmark-bulk-transactions.R` for transaction log diagnostics.

## CREATE-vs-APPEND logical logging

`benchmark-create-vs-append.R` records timed write operations first, then
optionally probes `sys.dm_tran_database_transactions` (`logical_log=TRUE`).
Each diagnostic uses a separate SQL connection and reads transaction log
usage before `COMMIT`. The disposable table is dropped afterward. This avoids
large `ROLLBACK` operations timing out and invalidating the helper. DMV access
failures are reported in `logical_log$status`/`note`; completed timing CSVs
are preserved independently. Do not run the benchmark in a production database.
