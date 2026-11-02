# Offline test of dialect dispatch (no SQL Server or helper required).
# The test builds a deliberately invalid connection object whose cached
# metadata is sufficient for the SQL dialect method to work.
library(RSqlClientNET)
library(DBI)
if (!requireNamespace('dbplyr', quietly=TRUE) ||
    utils::packageVersion('dbplyr') < '2.6.0') {
  message('Skipping dbplyr dialect test: dbplyr >= 2.6.0 not installed')
} else {
  h <- new.env(parent=emptyenv())
  class(h) <- 'sqlclient_connection'
  h$closed <- TRUE
  h$transport <- 'rcpp'
  h$dbi_info <- list(db.version='16.0.1000.6', dbname='test',
                     username='test', host=NA_character_, port=NA_integer_)
  x <- methods::new('SqlClientNETConnection', handle=h)
  stopifnot(
    identical(DBI::dbGetInfo(x)$db.version, '16.0.1000.6'),
    identical(dbplyr::dbplyr_edition(x), 2L),
    inherits(dbplyr::sql_dialect(x), 'sql_dialect_mssql')
  )
  id_quoted <- as.character(DBI::dbQuoteIdentifier(
    x, DBI::Id(schema='my]schema', table='a.b')))
  id_reversed <- as.character(DBI::dbQuoteIdentifier(
    x, DBI::Id(table='a.b', schema='my]schema')))
  expected <- '[my]]schema].[a.b]'
  if (!identical(id_quoted, expected) || !identical(id_reversed, expected))
    stop('Incorrect DBI::Id quoting: forward=', sQuote(id_quoted),
         ', reversed=', sQuote(id_reversed), ', expected=', sQuote(expected))
  quoted <- DBI::dbQuoteIdentifier(x, 'x]y')
  stopifnot(identical(as.character(quoted), '[x]]y]'),
            identical(DBI::dbQuoteIdentifier(x, quoted), quoted))
  message('RSqlClientNET v0.0.29 offline SQL Server dialect test passed')
}
