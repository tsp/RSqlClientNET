# SQL Server / dbplyr integration regression test (requires a real SQL Server).
# Set RSQLCLIENTNET_TEST_CONNECTION_STRING before running from the package root:
# source('tests/dbplyr-029.R')
run_dbplyr_029 <- function() {
  stopifnot(requireNamespace('dbplyr', quietly=TRUE),
            utils::packageVersion('dbplyr') >= '2.6.0',
            requireNamespace('dplyr', quietly=TRUE))
  library(DBI)
  library(RSqlClientNET)
  cs <- Sys.getenv('RSQLCLIENTNET_TEST_CONNECTION_STRING', '')
  if (!nzchar(cs)) stop('Set RSQLCLIENTNET_TEST_CONNECTION_STRING first.')
  con <- DBI::dbConnect(SqlClientNET(), connection_string=cs,
                        batch_rows=8192L, precision='native', tz='UTC')
  # Self-contained fixture: never require a pre-existing database schema.
  fixture_created <- FALSE
  on.exit({
    if (isTRUE(tryCatch(DBI::dbIsValid(con), error = function(e) FALSE))) {
      if (fixture_created)
        try(DBI::dbRemoveTable(con, fixture, fail_if_missing = FALSE),
            silent = TRUE)
      try(DBI::dbDisconnect(con), silent = TRUE)
    }
  }, add = TRUE)
  schema <- as.character(DBI::dbGetQuery(
    con, "SELECT SCHEMA_NAME() AS schema_name")$schema_name[[1L]])
  stopifnot(length(schema) == 1L, !is.na(schema), nzchar(schema))
  table <- sprintf("rscn_dbplyr_%d_%09d", Sys.getpid(), sample.int(1e9, 1L))
  fixture <- DBI::Id(schema = schema, table = table)
  # Also clean up a table created by a partially failed dbWriteTable().
  fixture_created <- TRUE
  DBI::dbWriteTable(con, fixture, data.frame(id = seq_len(10L)),
                    row.names = FALSE)

  # DBI info is cached. Works even when a cursor is open.
  info <- DBI::dbGetInfo(con)
  stopifnot(identical(info$dbms.name,'Microsoft SQL Server'),
            is.character(info$db.version), length(info$db.version)==1L,
            is.character(info$dbname),
            inherits(dbplyr::sql_dialect(con),'sql_dialect_mssql'),
            identical(dbplyr::dbplyr_edition(con),2L))

  q <- as.character(DBI::dbQuoteIdentifier(con, fixture))
  expected_q <- paste0(
    as.character(DBI::dbQuoteIdentifier(con, schema)), ".",
    as.character(DBI::dbQuoteIdentifier(con, table)))
  if (!identical(q, expected_q))
    stop('dbQuoteIdentifier(Id) mismatch: got ', sQuote(q),
         '; expected ', sQuote(expected_q))
  stopifnot(
    identical(as.character(DBI::dbQuoteIdentifier(con,
      DBI::Id(table = table, schema = schema))), expected_q),
    DBI::dbExistsTable(con, fixture),
    'id' %in% DBI::dbListFields(con, fixture)
  )

  # Stable SQL Server dialect, not generic LIMIT. Lazy SQL stays on the server.
  remote <- dplyr::tbl(con, dbplyr::in_schema(schema, table))
  limit_sql <- as.character(dbplyr::sql_render(utils::head(remote,5L)))
  stopifnot(grepl('TOP',limit_sql,ignore.case=TRUE),
            !grepl('\\bLIMIT\\b',limit_sql,ignore.case=TRUE))
  test_sql <- remote |>
    dplyr::filter(id <= 5L) |>
    dplyr::mutate(next_id=id+1L) |>
    dplyr::select(id,next_id) |>
    dplyr::arrange(id) |>
    dplyr::collect()
  stopifnot(identical(as.integer(test_sql$id),1:5),
            identical(as.integer(test_sql$next_id),2:6))
  grouped <- remote |>
    dplyr::filter(id <= 5L) |>
    dplyr::summarise(n=dplyr::n()) |>
    dplyr::collect()
  stopifnot(nrow(grouped)==1L, as.numeric(grouped$n[[1]])==5)

  # A cursor stays valid while client code requests dbGetInfo and dbListResults.
  res <- DBI::dbSendQuery(con, 'SELECT n FROM (VALUES (1),(2),(3)) v(n) ORDER BY n')
  stopifnot(length(DBI::dbListResults(con))==1L)
  stopifnot(identical(DBI::dbFetch(res,n=2L)$n,1:2),
            identical(DBI::dbGetInfo(res)$row.count,2))
  stopifnot(DBI::dbGetInfo(con)$db.version==info$db.version)
  stopifnot(identical(DBI::dbFetch(res,n=2L)$n,3L))
  DBI::dbClearResult(res)
  stopifnot(length(DBI::dbListResults(con))==0L)
  DBI::dbRemoveTable(con, fixture)
  fixture_created <- FALSE
  DBI::dbDisconnect(con)
  stopifnot(!DBI::dbIsValid(con))
  message('RSqlClientNET v0.0.29 DBI/dbplyr integration test passed')
}
run_dbplyr_029()
