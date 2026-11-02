# dbplyr >= 2.6.0 uses a dialect object to separate how SQL is sent from
# which SQL syntax is generated. Reuse dbplyr's maintained SQL Server
# translation rather than duplicating T-SQL SELECT, TOP, OFFSET/FETCH,
# window, joins, date/time and boolean translations in RSqlClientNET.
#
# These S3 methods are registered lazily in NAMESPACE: dbplyr is Suggested,
# not Imported, because basic DBI connections must work without dplyr/dbplyr.

#' @exportS3Method dbplyr::sql_dialect
#' @noRd
sql_dialect.SqlClientNETConnection <- function(con) {
  if (!requireNamespace("dbplyr", quietly=TRUE) ||
      utils::packageVersion("dbplyr") < "2.6.0") {
    stop("RSqlClientNET dbplyr integration requires dbplyr >= 2.6.0.", call.=FALSE)
  }
  version <- DBI::dbGetInfo(con)$db.version
  if (length(version) != 1L || is.na(version) || !nzchar(version) ||
      !grepl("^[0-9]+(\\.[0-9]+)+$", version)) {
    # Fallback affects SQL generation only, not server metadata reporting.
    version <- "15.0"
  }
  dbplyr::dialect_mssql(version=version)
}

#' @exportS3Method dbplyr::dbplyr_edition
#' @noRd
dbplyr_edition.SqlClientNETConnection <- function(con) 2L
