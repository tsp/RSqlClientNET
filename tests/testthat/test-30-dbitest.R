# DBItest is intentionally opt-in.  It covers much more than the project's
# existing regression tests; failures must be investigated, never silenced.
testthat::test_that("DBItest official SQL Server conformance (opt in)", {
  testthat::skip_if(Sys.getenv("RSQLCLIENTNET_RUN_DBITEST", "") != "1",
                    "Set RSQLCLIENTNET_RUN_DBITEST=1 to enable official conformance tests")
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  testthat::skip_if_not_installed("DBItest", minimum_version = "1.8.3")
  # Use a dedicated disposable test database; DBItest creates and drops tables.
  # DBItest assumptions that conflict with T-SQL syntax or literal types
  # are explicitly skipped below; see docs/DBI-COMPATIBILITY.md for the documented categories.
  # Exercise actual SQL BIGINT and NULL quoting in the separate SQL Server
  # dialect regressions (test-20-sql-server-dbi-conformance-03419.R).
  # DBItest defaults its *compatibility* target to 1.7.1, regardless of
  # the actual installed version.  Pin a reviewed target and enable the
  # named SQL Server placeholders explicitly; otherwise dozens of newer
  # checks and more than a hundred bind checks are silently skipped.
  # DBItest >= 1.8.3 is enforced in the release runner.
  tweaks <- DBItest::tweaks(
    dbitest_version="1.8.3",
    placeholder_pattern="@name",
    constructor_name="SqlClientNET",
    create_table_as=function(table_name,query="SELECT 1.5 AS a")
      paste0("SELECT * INTO ",table_name," FROM (",query,") AS sc_source"),
    create_table_empty=function(table_name)
      paste0("CREATE TABLE ",table_name," (a INTEGER)"),
    # SQL Server uses VARBINARY, not ANSI BLOB, and 0x... hex literals.
    blob_cast=function(x) paste0("CAST(",x," AS VARBINARY(MAX))"),
    date_cast=function(x) paste0("CAST('",x,"' AS DATE)"),
    time_cast=function(x) paste0("CAST('",x,"' AS TIME(7))"),
    timestamp_cast=function(x) paste0("CAST('",x,"' AS DATETIME2(7))")
  )
  ctx <- DBItest::make_context(RSqlClientNET::SqlClientNET(),
                               list(connection_string=cs,tz="UTC"),
                               tweaks=tweaks,
                               default_skip=c(
                                 "package_name",
                                 # In DBItest, SELECT 10000000000 produces
                                 # DECIMAL in T-SQL rather than BIGINT.
                                 # bigint=... only applies to SQL BIGINT.
                                 "connect_bigint_.*",
                                 # DBItest uses FROM (SELECT 1) a without a
                                 # column alias; T-SQL rejects it (error 8155).
                                 # Quoting NULL itself works (see dialect test).
                                 "quote_string_na_is_null",
                                 "quote_literal_na_is_null",
                                 # SQL Server has neither CURRENT_TIME nor an
                                 # unnamed column in FROM (SELECT 1) AS a.
                                 "data_time_current",
                                 # Untyped large numeric literals in T-SQL
                                 # evaluate as DECIMAL, not BIGINT. DBItest's
                                 # lossless integer64 and coercion-warning tests
                                 # therefore do not target SQL BIGINT here.
                                 # See explicit BIGINT boundary regressions.
                                 "data_64_bit_numeric_warning",
                                 "data_64_bit_lossless",
                                 # DBItest uses the same global_con in both
                                 # stages. SQL Server #temp tables persist
                                 # until the creating session disconnects;
                                 # a later test on that SAME session must see
                                 # the table. Verify real reconnect semantics
                                 # with independent connections below.
                                 "create_table_temporary_2",
                                 "temporary_table_2",
                                 # DBItest's second-stage check uses the SAME
                                 # SQL session. SQL Server #temp tables persist
                                 # until that connection actually closes.
                                 "arrow_create_table_arrow_temporary_2",
                                 "arrow_write_table_arrow_temporary_2",
                                 # Inherited Arrow fallback materializes Arrow
                                 # nanosecond/extended timestamps as R POSIXct
                                 # (double seconds), potentially losing bits.
                                 # Don't suppress warnings; mark these two
                                 # lossless-Arrow cases as unsupported until a
                                 # dedicated precision-preserving Arrow path.
                                 "arrow_write_table_arrow_roundtrip_timestamp_extended",
                                 "arrow_append_table_arrow_roundtrip_timestamp_extended"))
  # The incompatible raw cases remain runnable explicitly as a diagnostic;
  # they are NOT a release gate because they exercise a different SQL dialect.
  scope <- Sys.getenv("RSQLCLIENTNET_DBITEST_SCOPE", "all")
  if (identical(scope, "all")) {
    DBItest::test_all(ctx = ctx)
  } else if (identical(scope, "bind-only")) {
    message("Testing SQL Server named parameter binding using @name placeholders")
    DBItest::test_some("bind_.*", ctx = ctx)
  } else if (identical(scope, "dialect-diagnostic")) {
    message("Raw DBItest dialect diagnostic: failures are expected on SQL Server; ",
            "see docs/DBI-COMPATIBILITY.md")
    DBItest::test_some(
      c("connect_bigint_.*", "quote_string_na_is_null", "quote_literal_na_is_null"),
      ctx = ctx
    )
  } else {
    stop("RSQLCLIENTNET_DBITEST_SCOPE must be 'all', 'bind-only', or 'dialect-diagnostic'.")
  }
})
