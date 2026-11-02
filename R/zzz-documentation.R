#' Native Microsoft.Data.SqlClient SQL Server DBI interface
#'
#' Namespace registrations for DBI S4 methods, lazy dbplyr methods, and native code.
#' @name RSqlClientNET-package
#' @docType package
#' @rawNamespace useDynLib(RSqlClientNET, .registration = TRUE)
#' @rawNamespace importFrom(Rcpp,evalCpp)
#' @rawNamespace importFrom(bit64,as.integer64)
#' @rawNamespace export(sqlclient_build)
#' @rawNamespace export(sql_connect)
#' @rawNamespace export(sql_ping)
#' @rawNamespace export(sql_query)
#' @rawNamespace export(sql_execute)
#' @rawNamespace export(sql_disconnect)
#' @rawNamespace export(sql_is_valid)
#' @rawNamespace S3method(print,sqlclient_connection)
#' @rawNamespace export(sql_timings)
#' @rawNamespace export(sql_schema)
#' @rawNamespace export(sql_param)
#' @rawNamespace export(sql_pipe_stats)
#' @rawNamespace export(sql_cancel)
#' @rawNamespace export(sql_type_timings)
#' @rawNamespace export(sql_default_format)
#' @rawNamespace export(sql_connection_status)
#' @rawNamespace export(sql_begin)
#' @rawNamespace export(sql_commit)
#' @rawNamespace export(sql_rollback)
#' @rawNamespace export(sql_reconnect)
#' @rawNamespace export(sql_query_stream)
#' @rawNamespace export(sql_process_memory)
#' @rawNamespace export(SqlClientNET)
#' @rawNamespace importClassesFrom(DBI,DBIDriver,DBIConnection,DBIResult)
#' @rawNamespace import(DBI)
#' @rawNamespace export(Id,dbCanConnect,dbExecute,dbGetQuery,dbIsReadOnly,dbListObjects,dbQuoteLiteral,dbUnquoteIdentifier,dbWithTransaction)
#' @rawNamespace export(dbAppendTableArrow,dbBindArrow,dbCreateTableArrow,dbFetchArrow,dbFetchArrowChunk,dbGetQueryArrow,dbReadTableArrow,dbSendQueryArrow,dbWriteTableArrow)
#' @rawNamespace exportClasses(SqlClientNETDriver,SqlClientNETConnection,SqlClientNETResult)
#' @rawNamespace exportMethods(dbConnect,dbDisconnect,dbIsValid,dbSendQuery,dbSendStatement,dbFetch,dbBind,dbHasCompleted,dbClearResult,dbColumnInfo,dbGetRowCount,dbGetRowsAffected,dbGetStatement,dbBegin,dbCommit,dbRollback,dbQuoteIdentifier,dbQuoteString,dbListTables,dbExistsTable,dbGetInfo,dbDataType,dbListFields,dbReadTable)
#' @rawNamespace S3method(dbplyr::sql_dialect,SqlClientNETConnection)
#' @rawNamespace S3method(dbplyr::dbplyr_edition,SqlClientNETConnection)
#' @rawNamespace exportMethods(dbListResults,dbUnquoteIdentifier,dbQuoteLiteral)
#' @rawNamespace exportMethods(dbWriteTable,dbAppendTable,dbCreateTable,dbRemoveTable)
"_PACKAGE"

#' Experimental SQL Server DBI backend
#'
#' @name SqlClientNET
#' @aliases Id
#' @aliases SqlClientNETDriver-class
#' @aliases SqlClientNETConnection-class
#' @aliases SqlClientNETResult-class
#' @description A persistent Microsoft.Data.SqlClient-based SQL Server connection, using Rcpp binary SCB2 batches and paged SqlDataReader results. No SQL result files are created.
#' @details Create a connection with \code{DBI::dbConnect(SqlClientNET(), connection_string = con_str,
#' precision = "native", tz = "UTC", batch_rows = 32768L)}.
#' DBI uses native Rcpp / SCB2 columnar transport; optional CSV results also use native Rcpp frames.
#' One DBI result cursor can be active on a connection; clear it with \code{DBI::dbClearResult()} before starting another operation.
#' \code{dbFetch(n = 0L)} returns a typed, empty result, while positive \code{n} reads the next rows without re-executing SQL.
#' \code{DBI::dbGetQuery()}, \code{dbExecute()}, and \code{dbBind()} accept
#' named, equal-length parameter vectors or data.frames. Query parameter sets are
#' executed sequentially on the same connection, and \code{dbFetch(n)} combines
#' results in parameter order. \code{dbBind()} may be repeated before
#' \code{dbClearResult()}; rebinding a query discards remaining rows from the
#' previous binding. Statement result counts refer to the latest binding.
#' \code{dbFetch(n = NA)} fetches all remaining rows. A statement result fetched
#' with \code{dbFetch()} returns an empty data.frame with a warning.
#' Bulk updates with millions of rows should use \code{dbAppendTable()} rather
#' than the per-parameter RPC mechanism. Zero-length parameter vectors currently
#' return no rows without schema metadata; this is not yet full DBI conformity.
#' \code{DBI::dbWriteTable()}, \code{dbAppendTable()}, \code{dbCreateTable()}, and \code{dbRemoveTable()} are implemented with SqlBulkCopy on the existing SQL session. Local \code{#temp} tables exist until that SQL session ends (usually at disconnect).
#' Optional \code{dbplyr >= 2.6.0} integrates with \code{dplyr::tbl()}, \code{collect()}, and T-SQL translation.
#' In native precision mode, ambiguous SQL wall-clock timestamps normally become \code{NA} with a warning; \code{dst_fallback="ordered"} resolves autumn repeated-hour times by ordered rows with a warning. Nonexistent local times remain \code{NA}. Use \code{precision="full"} to preserve timestamp text. SQL BIGINT minimum (-2^63) is also converted to NA with a warning when \code{bigint="integer64"}, as that value is reserved by bit64; use \code{bigint="character"} for lossless reads.
#' Refer to \code{\link{RSqlClientNET-db-connect}} for all connection arguments,\code{\link{RSqlClientNET-db-query}} for queries and parameters, \code{\link{RSqlClientNET-db-result}} for cursor handling, \code{\link{RSqlClientNET-db-write}} for uploads, and \code{\link{RSqlClientNET-db-read}} for metadata and reads. The package remains experimental; see README.md for limitations.
#' @usage SqlClientNET()
#' @return An S4 \code{DBIDriver} for use with \code{DBI::dbConnect()}.
#' @examples \dontrun{
#' con <- DBI::dbConnect(SqlClientNET(), connection_string=con_str)
#' res <- DBI::dbSendQuery(con, "SELECT TOP (10) * FROM dbo.MyTable")
#' part <- DBI::dbFetch(res, n = 5L)
#' DBI::dbClearResult(res)
#' DBI::dbDisconnect(con)
#' }
NULL

#' Connect to SQL Server with DBI
#'
#' @name RSqlClientNET-db-connect
#' @aliases dbGetInfo,SqlClientNETConnection-method
#' @aliases dbGetInfo,SqlClientNETDriver-method
#' @aliases dbGetInfo,SqlClientNETResult-method
#' @aliases dbIsValid,SqlClientNETConnection-method
#' @aliases dbIsValid,SqlClientNETDriver-method
#' @aliases dbIsValid,SqlClientNETResult-method
#' @aliases dbConnect
#' @aliases dbConnect,SqlClientNETDriver-method
#' @aliases dbDisconnect
#' @aliases dbDisconnect,SqlClientNETConnection-method
#' @description Open and close a SQL Server DBI connection through Microsoft.Data.SqlClient. The driver is created by \code{SqlClientNET()}, not by \code{odbc::odbc()}.
#' @usage dbConnect(drv, ...)
#'   dbDisconnect(conn, ...)
#' @param drv A \code{SqlClientNETDriver} returned by \code{SqlClientNET()}. The \code{dbConnect()} method is specific to this driver.
#' @param ... For \code{dbConnect()}, named arguments passed to \code{sql_connect()}, including \code{connection_string}, \code{server}, \code{database}, \code{integrated_security}, \code{username}, \code{password}, \code{encrypt}, and \code{trust_server_certificate}. For \code{dbDisconnect()}, no additional arguments are used.
#' @param conn An active \code{SqlClientNETConnection} returned by \code{dbConnect()}.
#' @section Connection arguments passed through \code{...}:
#' These are recognized named arguments to \code{DBI::dbConnect()} through
#' its \code{...}, with the defaults shown below:
#' \describe{
#'   \item{\code{connection_string}}{Optional complete Microsoft.Data.SqlClient connection string, e.g. \code{"Server=localhost;Database=MyDb;Integrated Security=true;TrustServerCertificate=true"}. Pass it via \code{...}; unlike odbc, the argument is named \code{connection_string} (without a leading dot).}
#'   \item{\code{server}}{SQL Server hostname or named instance, supplied via \code{...} if a complete connection string is not used.}
#'   \item{\code{database}}{Initial SQL Server database, supplied via \code{...}.}
#'   \item{\code{integrated_security}}{Use Windows authentication when \code{TRUE} (default), supplied via \code{...}.}
#'   \item{\code{username}}{SQL login name for SQL authentication, supplied via \code{...}.}
#'   \item{\code{password}}{SQL login password, supplied via \code{...}; avoid storing passwords in source code.}
#'   \item{\code{encrypt}}{Request an encrypted SQL connection; defaults to \code{TRUE}.}
#'   \item{\code{trust_server_certificate}}{Whether to bypass certificate-chain validation; defaults to \code{FALSE}. Use only with an explicitly trusted server.}
#' }
#' @section Connection and cursor options passed through \code{...}:
#' These named arguments are supplied through \code{...} in the documented
#' DBI generic signatures; they are not additional formal parameters:
#' \describe{
#'   \item{\code{transport}}{Only \code{"rcpp"} is supported for DBI cursors. Other values cause an error.}
#'   \item{\code{result_format}}{Only \code{"columnar"} is supported for DBI cursors.}
#'   \item{\code{batch_rows}}{Maximum rows per SCB2 frame (default 32768; range 1 to 32768). Large text and binary values automatically trigger smaller batches to keep each frame below 128 MiB. This is NOT the bulk-write batch size.}
#'   \item{\code{precision}}{\code{"native"} (default) converts supported decimal and datetime values into R numeric/POSIXct types; \code{"full"} retains exact decimal and datetime text.}
#'   \item{\code{bigint}}{One of \code{"integer64"} (default), \code{"character"}, \code{"numeric"}, or \code{"integer"}. The SQL BIGINT minimum value cannot be represented by bit64 and becomes NA with a warning when using integer64; character preserves it exactly.}
#'   \item{\code{datetime}}{\code{NULL} chooses POSIXct for native precision and character for full precision; may also be set explicitly to \code{"POSIXct"} or \code{"character"}.}
#'   \item{\code{binary}}{\code{"raw"} (default) returns SQL binary columns as list-columns of raw vectors; \code{"base64"} requests text encoding of binary values (not a transport protocol).}
#'   \item{\code{unsupported}}{\code{"error"} (default) rejects unsupported SQL types; \code{"character"} requests a character representation.}
#'   \item{\code{tz}}{Time zone used to interpret SQL datetime wall-clock values. \code{""} (default) uses the system time zone; use \code{"UTC"} or an IANA name, e.g. \code{"Europe/Copenhagen"}.}
#'   \item{\code{datetime_parser}}{\code{"lubridate"} (default) or \code{"base"} for text/CSV datetime parsing. Native columnar datetime values use binary ticks for all timezones.}
#'   \item{\code{dst_fallback}}{\code{"na"} (default) converts ambiguous local timestamps during autumn clock rollback to NA with a warning. \code{"ordered"} assumes SQL result rows are chronologically ordered, uses the first repeated hour with the earlier (daylight-saving) UTC offset, and switches to the later (standard-time) offset when the local wall clock decreases. A warning is still generated. State is preserved across \code{dbFetch()} calls. Use an explicit SQL \code{ORDER BY} on an independent chronological sequence or UTC time (not on the ambiguous local timestamp) and \code{tz = "Europe/Copenhagen"} or another IANA timezone. Identical repeated wall times with no decrease cannot be disambiguated by this heuristic.}
#'   \item{\code{timeout}}{Positive timeout in seconds for DBI queries and cursor operations (default 600); this does not change the lower-level initial connection timeout.}
#'   \item{\code{data_table}}{\code{FALSE} (default) returns base data.frames from DBI fetches; \code{TRUE} returns data.tables.}
#' }
#' @details The .NET helper is built when installing RSqlClientNET from source. Only one DBI cursor may be active per connection. Clear query results before issuing other operations. \code{dbDisconnect()} closes the SQL Server session and discards any local temporary tables or open transactions. Ambiguous/nonexistent datetime wall times become NA with a warning under POSIXct conversion by default. \code{dst_fallback="ordered"} may resolve repeated autumn clock hours from ordered rows, with a warning; nonexistent spring times still become NA.
#' @return For \code{dbConnect()}, a \code{SqlClientNETConnection} (DBIConnection); for \code{dbDisconnect()}, invisibly TRUE.
#' @seealso \code{\link{RSqlClientNET-db-query}}, \code{\link{RSqlClientNET-db-write}}
#' @examples \dontrun{
#' library(DBI)
#' con <- dbConnect(RSqlClientNET::SqlClientNET(),
#'   connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
#'   precision = "native", bigint = "integer64", tz = "UTC")
#' DBI::dbDisconnect(con)
#' }
NULL

#' Run SQL queries and statements with DBI
#'
#' @name RSqlClientNET-db-query
#' @aliases dbQuoteIdentifier,SqlClientNETConnection,Id-method
#' @aliases dbQuoteIdentifier,SqlClientNETConnection,SQL-method
#' @aliases dbQuoteLiteral,SqlClientNETConnection,ANY-method
#' @aliases dbQuoteLiteral,SqlClientNETConnection,POSIXct-method
#' @aliases dbQuoteLiteral,SqlClientNETConnection,POSIXlt-method
#' @aliases dbQuoteLiteral,SqlClientNETConnection,list-method
#' @aliases dbQuoteLiteral,SqlClientNETConnection,raw-method
#' @aliases dbQuoteString,SqlClientNETConnection,ANY-method
#' @aliases dbQuoteString,SqlClientNETConnection,SQL-method
#' @aliases dbQuoteString,SqlClientNETConnection,character-method
#' @aliases dbSendQuery,SqlClientNETConnection,SQL-method
#' @aliases dbSendStatement,SqlClientNETConnection,SQL-method
#' @aliases dbQuoteIdentifier,SqlClientNETConnection,character-method
#' @aliases dbGetQuery
#' @aliases dbExecute
#' @aliases dbSendQuery
#' @aliases dbSendStatement
#' @aliases dbSendQuery,SqlClientNETConnection,character-method
#' @aliases dbSendStatement,SqlClientNETConnection,character-method
#' @description Query and execute T-SQL using DBI. \code{dbGetQuery()} and \code{dbExecute()} use DBI convenience implementations atop RSqlClientNET cursor and statement methods.
#' @usage dbGetQuery(conn, statement, ...)
#'   dbExecute(conn, statement, ...)
#'   dbSendQuery(conn, statement, ...)
#'   dbSendStatement(conn, statement, ...)
#' @param conn An open \code{SqlClientNETConnection} from \code{DBI::dbConnect(SqlClientNET(), ...)}.
#' @param statement A single SQL statement as a character string or a \code{DBI::SQL()} object. Named parameters appear in T-SQL as \code{@parameter_name}.
#' @param ... For \code{dbGetQuery()} and \code{dbSendQuery()}, named conversion options described in the Per-query read options section. They are not supported for \code{dbExecute()} or \code{dbSendStatement()}.
#' @section Query arguments passed through \code{...}:
#' These named arguments are supplied through \code{...} in the documented
#' DBI generic signatures; they are not additional formal parameters:
#' \describe{
#'   \item{\code{n}}{Rows to return from \code{dbGetQuery()}: \code{-1} or \code{Inf} for all, \code{0} for a typed zero-row result, or a nonnegative integer for a partial result.}
#'   \item{\code{params}}{\code{NULL} (default) or a named list or data.frame of values for \code{@name} placeholders. Scalar values execute once; equal-length vectors execute repeated parameter sets in order. Use \code{sql_param()} to specify SQL types precisely.}
#'   \item{\code{immediate}}{\code{NULL} (default), \code{TRUE} or \code{FALSE}. Controls immediate execution versus deferred binding. With \code{FALSE}, an unbound result is returned and must be executed by \code{dbBind()}. With the default \code{NULL}, a recoverable SQL Server error for an undeclared named \code{@name} parameter also leaves a pending result for \code{dbBind()}; explicit \code{TRUE} retains immediate SQL errors.}
#' }
#' @details By default, results use the precision, BIGINT, binary, timezone, and data_table options set by \code{dbConnect()}. You can override any of these per \code{dbGetQuery()} or \code{dbSendQuery()} call, e.g. \code{precision="full"} or \code{tz="Europe/Copenhagen", dst_fallback="ordered"}. Overrides do not modify the connection and remain fixed throughout the result's \code{dbFetch()} calls. If \code{precision} is overridden and \code{datetime} was not explicitly chosen at connection or query level, the datetime default follows the selected precision. \code{dbGetQuery()} is inherited from DBI rather than overridden by RSqlClientNET; it uses the specialized \code{dbSendQuery()}, \code{dbFetch()}, and \code{dbClearResult()} methods. An active result should be cleared before other SQL operations; starting another DBI query invalidates the previous cursor with a warning. SQL parameter values are bound, not interpolated into SQL text. When repeated parameter sets are supplied, query results are concatenated in input order. For very large table loads, use \code{dbAppendTable()}, not per-row parameter execution.
#' @section Per-query read options:
#' The following named arguments are accepted through \code{...} by
#' \code{dbGetQuery()}, \code{dbSendQuery()}, and \code{dbReadTable()}.
#' Unspecified options inherit the connection defaults from \code{dbConnect()}:
#' \describe{
#'   \item{\code{precision}}{\code{"native"} (R numeric/POSIXct where possible) or \code{"full"} (exact decimal and datetime text).}
#'   \item{\code{bigint}}{\code{"integer64"}, \code{"character"}, \code{"numeric"}, or \code{"integer"}. \code{"character"} preserves the SQL BIGINT minimum exactly; with \code{"integer64"} that sentinel becomes NA with a warning.}
#'   \item{\code{datetime}}{\code{NULL} to choose from precision mode, or \code{"POSIXct"} / \code{"character"} explicitly. An explicit connection-level setting is not reset by a query's precision override.}
#'   \item{\code{binary}}{\code{"raw"} for list-columns of raw vectors, or \code{"base64"} for encoded text values. This is not the transport protocol.}
#'   \item{\code{unsupported}}{\code{"error"} or \code{"character"} for unsupported SQL Server types.}
#'   \item{\code{tz}}{\code{""} for the system time zone, \code{"UTC"}, or an IANA zone such as \code{"Europe/Copenhagen"}. Applies to zone-less SQL local timestamps.}
#'   \item{\code{datetime_parser}}{\code{"lubridate"} or \code{"base"}.}
#'   \item{\code{dst_fallback}}{\code{"na"} (default, ambiguous repeated-hour times become NA with a warning) or \code{"ordered"} (infer summer time before the clock rollback and winter time after a backwards jump, with a warning). Only use the latter when SQL rows have a reliable independent chronological order.}
#'   \item{\code{data_table}}{\code{FALSE} for a data.frame or \code{TRUE} for a data.table.}
#'   \item{\code{batch_rows}}{Integer 1--32768, default 32768; this is a row-count ceiling. Frames automatically contain fewer rows if needed to stay within the 128 MiB byte limit; it does not control SqlBulkCopy writing.}
#'   \item{\code{timeout}}{Positive finite number of seconds, default 600.}
#' }
#' The option snapshot remains fixed for the result and for every subsequent
#' \code{dbFetch()} call, without changing the underlying connection defaults.
#' @return \code{dbGetQuery()} returns a data.frame (or data.table with \code{data_table=TRUE}); \code{dbExecute()} returns the affected-row count; \code{dbSendQuery()} and \code{dbSendStatement()} return \code{SqlClientNETResult} objects, which must be cleared.
#' @seealso \code{\link{RSqlClientNET-db-result}}, \code{\link{RSqlClientNET-db-connect}}
#' @examples \dontrun{
#' con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
#'   connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
#' x <- DBI::dbGetQuery(con,
#'   "SELECT name FROM sys.objects WHERE object_id > @min_id",
#'   params = list(min_id = 100L))
#' res <- DBI::dbSendQuery(con, "SELECT name FROM sys.objects")
#' first <- DBI::dbFetch(res, n = 10L)
#' DBI::dbClearResult(res)
#' exact <- DBI::dbGetQuery(con, "SELECT CAST(1.123456789 AS decimal(18,9)) AS x",
#'   precision = "full")
#' local <- DBI::dbGetQuery(con,
#'   "SELECT CAST('2026-10-25T02:30:00' AS datetime2) AS stamp",
#'   precision = "native", tz = "Europe/Copenhagen", dst_fallback = "ordered")
#' DBI::dbDisconnect(con)
#' }
NULL

#' Fetch, bind, and clear DBI results
#'
#' @name RSqlClientNET-db-result
#' @aliases dbBind,SqlClientNETResult-method
#' @aliases dbClearResult,SqlClientNETResult-method
#' @aliases dbColumnInfo,SqlClientNETResult-method
#' @aliases dbFetch,SqlClientNETResult-method
#' @aliases dbGetRowCount,SqlClientNETResult-method
#' @aliases dbGetRowsAffected,SqlClientNETResult-method
#' @aliases dbGetStatement,SqlClientNETResult-method
#' @aliases dbHasCompleted,SqlClientNETResult-method
#' @aliases dbFetch
#' @aliases dbBind
#' @aliases dbHasCompleted
#' @aliases dbClearResult
#' @aliases dbColumnInfo
#' @aliases dbGetRowCount
#' @aliases dbGetRowsAffected
#' @aliases dbGetStatement
#' @description Work with paged query and statement results returned by \code{dbSendQuery()} and \code{dbSendStatement()}.
#' @usage dbFetch(res, n = -1, ...)
#'   dbBind(res, params, ...)
#'   dbHasCompleted(res, ...)
#'   dbClearResult(res, ...)
#'   dbColumnInfo(res, ...)
#'   dbGetRowCount(res, ...)
#'   dbGetRowsAffected(res, ...)
#'   dbGetStatement(res, ...)
#' @param res An active \code{SqlClientNETResult} obtained with \code{dbSendQuery()} or \code{dbSendStatement()}.
#' @param n Rows to retrieve in this call: \code{-1}, \code{Inf}, or \code{NA} reads all remaining rows; \code{0} returns typed empty columns; a nonnegative whole number fetches at most that many rows.
#' @param params Named list or data.frame of parameters for \code{dbBind()}; values may be scalars or equal-length vectors. Names match SQL \code{@name} placeholders.
#' @param ... Additional arguments passed to DBI methods; no RSqlClientNET-specific options are currently supported here.
#' @details Call \code{dbClearResult()} after finishing a cursor, including partial reads. Repeated \code{dbBind()} is allowed on an uncleared result; rebinding a query drops unread rows from the previous binding. A query sent with \code{immediate=FALSE} must be bound before fetching. Only one active result per SQL Server connection is supported. Fetching a statement result produces an empty data.frame with a warning; use \code{dbGetRowsAffected()} for affected-row counts. A partially consumed result must not be used after a different query invalidates it.
#' @return \code{dbFetch()} yields a data.frame or data.table; \code{dbHasCompleted()} a logical scalar; \code{dbColumnInfo()} column metadata; \code{dbGetRowCount()} and \code{dbGetRowsAffected()} counts; \code{dbGetStatement()} a SQL string.
#' @seealso \code{\link{RSqlClientNET-db-query}}
#' @examples \dontrun{
#' con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
#'   connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
#' res <- DBI::dbSendQuery(con, "SELECT name FROM sys.objects")
#' repeat {
#'   chunk <- DBI::dbFetch(res, n = 1000L)
#'   if (nrow(chunk) == 0L) break
#'   print(nrow(chunk))
#' }
#' DBI::dbClearResult(res)
#' DBI::dbDisconnect(con)
#' }
NULL

#' Read and inspect SQL Server tables
#'
#' @name RSqlClientNET-db-read
#' @aliases dbDataType,SqlClientNETConnection-method
#' @aliases dbDataType,SqlClientNETDriver-method
#' @aliases dbExistsTable,SqlClientNETConnection,Id-method
#' @aliases dbExistsTable,SqlClientNETConnection,SQL-method
#' @aliases dbExistsTable,SqlClientNETConnection,character-method
#' @aliases dbListFields,SqlClientNETConnection,Id-method
#' @aliases dbListFields,SqlClientNETConnection,SQL-method
#' @aliases dbListFields,SqlClientNETConnection,character-method
#' @aliases dbListResults,SqlClientNETConnection-method
#' @aliases dbListTables,SqlClientNETConnection-method
#' @aliases dbReadTable,SqlClientNETConnection,Id-method
#' @aliases dbReadTable,SqlClientNETConnection,SQL-method
#' @aliases dbReadTable,SqlClientNETConnection,character-method
#' @aliases dbUnquoteIdentifier,SqlClientNETConnection,Id-method
#' @aliases dbUnquoteIdentifier,SqlClientNETConnection,SQL-method
#' @aliases dbUnquoteIdentifier,SqlClientNETConnection,character-method
#' @aliases dbReadTable
#' @aliases dbListTables
#' @aliases dbListFields
#' @aliases dbExistsTable
#' @aliases dbDataType
#' @description Read full tables and inspect SQL Server table names, fields and inferred column types through DBI.
#' @usage dbReadTable(conn, name, ...)
#'   dbListTables(conn, ...)
#'   dbListFields(conn, name, ...)
#'   dbExistsTable(conn, name, ...)
#'   dbDataType(dbObj, obj, ...)
#' @param conn An open \code{SqlClientNETConnection}.
#' @param name An SQL Server table name as a character scalar, \code{DBI::Id(schema = "dbo", table = "name")}, or an already quoted \code{DBI::SQL()} identifier. \code{#temp} names are local to the connection.
#' @param ... For \code{dbReadTable()}, named read options described in the Per-query read options section. Table metadata methods do not support these conversion options.
#' @param dbObj A RSqlClientNET DBI connection or driver for \code{dbDataType()}.
#' @param obj R vector, data.frame, or compatible R object whose SQL column type is to be inferred.
#' @section Table-read arguments passed through \code{...}:
#' These named arguments are supplied through \code{...} in the documented
#' DBI generic signatures; they are not additional formal parameters:
#' \describe{
#'   \item{\code{row.names}}{For \code{dbReadTable()}, \code{FALSE} (default) keeps all columns; \code{TRUE} consumes a \code{row_names} column as R row names, \code{NA} uses it if present, and a character scalar names an explicit row-name column.}
#'   \item{\code{check.names}}{\code{FALSE} (default) preserves SQL Server column names; \code{TRUE} normalizes them with \code{make.names()}.}
#' }
#' @details \code{dbReadTable()} forwards per-call conversion settings to \code{dbGetQuery()} without changing connection defaults. It reads the entire table into memory; for large tables use \code{dbSendQuery()} and repeated \code{dbFetch()} or \code{sql_query_stream()}. \code{dbListTables()} includes SQL Server views as well as ordinary tables. SQL Server local temporary tables survive only during the current connection and can be addressed as \code{"#name"}. The physical table name corresponding to \code{temporary=TRUE} is managed by the driver.
#' @section Per-query read options:
#' The following named arguments are accepted through \code{...} by
#' \code{dbGetQuery()}, \code{dbSendQuery()}, and \code{dbReadTable()}.
#' Unspecified options inherit the connection defaults from \code{dbConnect()}:
#' \describe{
#'   \item{\code{precision}}{\code{"native"} (R numeric/POSIXct where possible) or \code{"full"} (exact decimal and datetime text).}
#'   \item{\code{bigint}}{\code{"integer64"}, \code{"character"}, \code{"numeric"}, or \code{"integer"}. \code{"character"} preserves the SQL BIGINT minimum exactly; with \code{"integer64"} that sentinel becomes NA with a warning.}
#'   \item{\code{datetime}}{\code{NULL} to choose from precision mode, or \code{"POSIXct"} / \code{"character"} explicitly. An explicit connection-level setting is not reset by a query's precision override.}
#'   \item{\code{binary}}{\code{"raw"} for list-columns of raw vectors, or \code{"base64"} for encoded text values. This is not the transport protocol.}
#'   \item{\code{unsupported}}{\code{"error"} or \code{"character"} for unsupported SQL Server types.}
#'   \item{\code{tz}}{\code{""} for the system time zone, \code{"UTC"}, or an IANA zone such as \code{"Europe/Copenhagen"}. Applies to zone-less SQL local timestamps.}
#'   \item{\code{datetime_parser}}{\code{"lubridate"} or \code{"base"}.}
#'   \item{\code{dst_fallback}}{\code{"na"} (default, ambiguous repeated-hour times become NA with a warning) or \code{"ordered"} (infer summer time before the clock rollback and winter time after a backwards jump, with a warning). Only use the latter when SQL rows have a reliable independent chronological order.}
#'   \item{\code{data_table}}{\code{FALSE} for a data.frame or \code{TRUE} for a data.table.}
#'   \item{\code{batch_rows}}{Integer 1--32768, default 32768; this is a row-count ceiling. Frames automatically contain fewer rows if needed to stay within the 128 MiB byte limit; it does not control SqlBulkCopy writing.}
#'   \item{\code{timeout}}{Positive finite number of seconds, default 600.}
#' }
#' The option snapshot remains fixed for the result and for every subsequent
#' \code{dbFetch()} call, without changing the underlying connection defaults.
#' @return \code{dbReadTable()} returns a data.frame (or data.table if enabled); \code{dbListTables()} and \code{dbListFields()} character vectors; \code{dbExistsTable()} a logical scalar; \code{dbDataType()} SQL type strings.
#' @seealso \code{\link{RSqlClientNET-db-write}}, \code{\link{RSqlClientNET-db-result}}
#' @examples
#' \dontrun{
#' con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
#'   connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
#' tables <- DBI::dbListTables(con)
#' if (any(tables == "sys_dummy")) {
#'   columns <- DBI::dbListFields(con, "sys_dummy")
#'   data <- DBI::dbReadTable(con, "sys_dummy", precision="full")
#' }
#' DBI::dbDisconnect(con)
#' }
NULL

#' Transactions on SQL Server connections
#'
#' @name RSqlClientNET-db-transaction
#' @aliases dbBegin,SqlClientNETConnection-method
#' @aliases dbCommit,SqlClientNETConnection-method
#' @aliases dbRollback,SqlClientNETConnection-method
#' @aliases dbBegin
#' @aliases dbCommit
#' @aliases dbRollback
#' @aliases dbWithTransaction
#' @description Begin, commit, or roll back SQL Server transactions with standard DBI methods.
#' @usage dbBegin(conn, ...)
#'   dbCommit(conn, ...)
#'   dbRollback(conn, ...)
#'   dbWithTransaction(conn, code, ...)
#' @param conn An open \code{SqlClientNETConnection}.
#' @param ... No driver-specific options for ordinary DBI transactions. For explicit isolation levels, use \code{sql_begin()}.
#' @param code R expression evaluated within the transaction by \code{dbWithTransaction()}.
#' @details \code{dbBegin()} uses the SQL Server default transaction isolation level. A transaction applies to subsequent queries and bulk-write batches on the same connection. \code{dbWithTransaction()} automatically rolls back on an R error; a broken connection or unknown commit state must be investigated before retrying. Do not automatically replay potentially committed writes. \code{sql_begin(con, isolation = "Serializable")} is available for explicit isolation.
#' @return For \code{dbBegin()}, \code{dbCommit()}, and \code{dbRollback()}, an invisible success indication; \code{dbWithTransaction()} returns the value of \code{code}.
#' @seealso \code{\link{RSqlClientNET-db-query}}, \code{\link{RSqlClientNET-db-write}}
#' @examples \dontrun{
#' con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
#'   connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
#' DBI::dbWithTransaction(con, {
#'   dbExecute(con, "CREATE TABLE #tx_example (id int)")
#'   dbExecute(con, "INSERT INTO #tx_example VALUES (1)")
#' })
#' DBI::dbDisconnect(con)
#' }
NULL

#' Hard-cancel work on a RSqlClientNET connection
#'
#' @name sql_cancel
#' @description Terminate the .NET helper and invalidate its SQL Server session. Accepts a DBI or a legacy RSqlClientNET connection.
#' @details This is not a cooperative per-command cancellation. It kills the helper process and invalidates the underlying SQL session and every open DBI cursor, even when a result is partially fetched. SQL Server local \code{#temp} tables cannot survive this operation. For a synchronous running R call, use Esc/Ctrl+C, handled by the native I/O layer. A server-side transaction or commit may have an unknown outcome after a disconnect; never replay writes automatically.
#' @param con An open \code{SqlClientNETConnection} or legacy \code{sqlclient_connection}.
#' @usage sql_cancel(con)
#' @return Invisibly \code{TRUE}.
NULL

#' Read Rcpp native pipe transport statistics
#'
#' @name sql_pipe_stats
#' @description Inspect cumulative native pipe I/O counters for an open DBI or legacy Rcpp connection.
#' @details Windows uses blocking \code{ReadFile()} in an interruptible I/O worker thread. Legacy poll counters may be zero. The values are diagnostics of the local pipe, not SQL Server query timings.
#' @param con An open DBI \code{SqlClientNETConnection} or legacy \code{sqlclient_connection} using \code{transport="rcpp"}.
#' @param reset Logical; reset cumulative counters after reading them.
#' @usage sql_pipe_stats(con, reset = FALSE)
#' @return A named list with fields including \code{read_calls}, \code{bytes_read}, \code{blocking_reads}, \code{timeout_count} and \code{cancel_count}.
NULL

#' Read current R process resident memory
#'
#' @name sql_process_memory
#' @description Reports OS-specific process RSS/working set in bytes; peak_rss_bytes
#' is a process-lifetime peak and must not be confused with query-specific memory.
#' @usage sql_process_memory()
#' @return List with rss_bytes, peak_rss_bytes, private_bytes, and source.
NULL

#' Stream SQL Server result batches to an R callback
#'
#' @name sql_query_stream
#' @description Read SCB2 binary batches into R data.tables without retaining the whole result. Accepts DBI and legacy connections. No result files are created.
#' @details A DBI connection cannot have an active result cursor during this call. Always clear results first with \code{DBI::dbClearResult()}. Calling any SQL method on the same connection within \code{callback} is prohibited; use a separate connection if SQL access is needed. A callback error, interrupt or timeout invalidates the underlying helper and SQL session. A reconnect does not restore \code{#temp} tables or transactions. Timestamps in ambiguous or nonexistent wall-clock hours become NA with a warning identifying the 1-based result row(s).
#' @param con An open \code{SqlClientNETConnection} from \code{DBI::dbConnect(SqlClientNET(), ...)} or a legacy Rcpp \code{sql_connect()} connection.
#' @param sql SQL SELECT statement.
#' @param callback A synchronous function called once per nonempty data.table batch.
#' @param params Named scalar parameter list, like \code{DBI::dbGetQuery()}.
#' @param timeout Positive overall timeout including callback time. \code{NULL} inherits DBI connection settings, otherwise 600 seconds.
#' @param batch_rows Maximum rows per SQL batch, from 1 to 32768. Large values automatically use fewer rows to keep each frame within 128 MiB. \code{NULL} inherits DBI connection setting, otherwise 32768.
#' @param ... Optional \code{precision}, \code{bigint}, \code{datetime}, \code{binary}, \code{unsupported}, \code{tz}, \code{datetime_parser}. Defaults inherit from the DBI connection, or legacy direct-query defaults.
#' @usage sql_query_stream(con, sql, callback, params = list(), timeout = NULL,
#'   batch_rows = NULL, ...)
#' @return A summary list with \code{rows_received}, \code{batches_received}, \code{bytes_received} and \code{callback_seconds}; attributes permit \code{sql_timings()} and \code{sql_schema()}.
#' @examples \dontrun{
#' con <- DBI::dbConnect(SqlClientNET(), connection_string=con_str, precision="native", tz="UTC")
#' n <- 0
#' out <- sql_query_stream(con, "SELECT * FROM dbo.LargeTable",
#'   callback = function(batch) n <<- n + nrow(batch))
#' stopifnot(n == out$rows_received)
#' DBI::dbDisconnect(con)
#' }
NULL

#' Write and append SQL Server tables using binary SqlBulkCopy
#'
#' @name RSqlClientNET-db-write
#' @aliases dbAppendTable,SqlClientNETConnection,Id-method
#' @aliases dbAppendTable,SqlClientNETConnection,SQL-method
#' @aliases dbAppendTable,SqlClientNETConnection,character-method
#' @aliases dbCreateTable,SqlClientNETConnection,Id-method
#' @aliases dbCreateTable,SqlClientNETConnection,SQL-method
#' @aliases dbCreateTable,SqlClientNETConnection,character-method
#' @aliases dbRemoveTable,SqlClientNETConnection,Id-method
#' @aliases dbRemoveTable,SqlClientNETConnection,SQL-method
#' @aliases dbRemoveTable,SqlClientNETConnection,character-method
#' @aliases dbWriteTable,SqlClientNETConnection,Id-method
#' @aliases dbWriteTable,SqlClientNETConnection,SQL-method
#' @aliases dbWriteTable,SqlClientNETConnection,character-method
#' @aliases dbWriteTable
#' @aliases dbAppendTable
#' @aliases dbCreateTable
#' @aliases dbRemoveTable
#' @description Create, replace, append, and remove SQL Server tables using the DBI interface. Uploads use Rcpp SBIN binary column batches, a .NET IDataReader and SqlBulkCopy.
#' @usage dbWriteTable(conn, name, value, ...)
#'   dbAppendTable(conn, name, value, ..., row.names = NULL)
#'   dbCreateTable(conn, name, fields, ..., row.names = NULL,
#'       temporary = FALSE)
#'   dbRemoveTable(conn, name, ...)
#' @param conn An open \code{SqlClientNETConnection}.
#' @param name Target table name as a character scalar, \code{DBI::Id()} or \code{DBI::SQL()}. Use \code{temporary=TRUE} or names beginning \code{#} for SQL Server session-local tables.
#' @param value Data.frame or data.table to write. Column names form the target table fields; missing values become SQL NULL.
#' @param ... For \code{dbAppendTable()}, accepts only named \code{batch_rows}, \code{timeout}, and \code{table_lock}. Other methods reject unsupported extra options.
#' @param row.names For \code{dbWriteTable()}, \code{FALSE} (default) omits row names; \code{TRUE} stores them as \code{row_names}; \code{NA} retains nonautomatic names; a character scalar supplies the row-name field. For \code{dbAppendTable()} and \code{dbCreateTable()}, must be \code{NULL}.
#' @param temporary Logical. If \code{TRUE}, use a SQL Server local \code{#temp} table (session scope); for \code{dbRemoveTable()}, targets only a local temporary table.
#' @param fields A data.frame whose columns determine the SQL table types, or a named list/character vector of SQL Server type declarations.
#' @section Table-write arguments passed through \code{...}:
#' These named arguments are supplied through \code{...} in the documented
#' DBI generic signatures; they are not additional formal parameters:
#' \describe{
#'   \item{\code{overwrite}}{If \code{TRUE}, drop/recreate an existing table before writing; mutually exclusive with \code{append}.}
#'   \item{\code{append}}{If \code{TRUE}, append to an existing table rather than creating a new one; mutually exclusive with \code{overwrite}.}
#'   \item{\code{field.types}}{Optional named character vector of SQL Server column type overrides (e.g. \code{c(amount="DECIMAL(38,10)")}); unavailable with \code{append=TRUE}.}
#'   \item{\code{batch_rows}}{Number of rows per binary write batch (\code{NULL} uses 65536; range 1 to 131072). Also accepted inside \code{...} for \code{dbAppendTable()}. Unlike \code{dbConnect(batch_rows=)}, this controls upload.}
#'   \item{\code{timeout}}{Positive upload-operation timeout in seconds (default 600); can be passed inside \code{...} to \code{dbAppendTable()}.}
#'   \item{\code{table_lock}}{Logical, default \code{TRUE}. Requests \code{SqlBulkCopyOptions.TableLock} for this write. Set \code{FALSE} to avoid explicitly requesting a bulk table lock when concurrent writes are important. Table locking can block other operations and may allow minimal logging on eligible SQL Server tables under SIMPLE/BULK_LOGGED recovery. For \code{dbAppendTable()} pass an override as a named argument in \code{...}.}
#'   \item{\code{fail_if_missing}}{For \code{dbRemoveTable()}, error if table does not exist when \code{TRUE} (default); otherwise return \code{FALSE} invisibly.}
#' }
#' @details The binary upload transport is the only bulk-write implementation. \code{bulk_transport} is not supported. Encoded batches are capped at 128 MiB, and batches that exceed this limit are automatically retried with fewer rows before transmission. Upload is transactional unless the connection already has an explicit transaction; incomplete exchanges invalidate the connection. SQL types include NVARCHAR(MAX), DATE, DATETIME2, BIGINT, and VARBINARY(MAX). Factor columns are stored as character; appending factor columns produces a warning. For exact decimals, use a suitable \code{field.types} override and character input. For per-batch timings, set \code{options(RSqlClientNET.bulk_profile = TRUE)}. \code{table_lock} defaults to TRUE for each write and can be disabled per operation; it is not a persistent connection option; it does not guarantee minimal logging (recovery model, indexes, table state and other SQL Server conditions matter). Local temporary tables require the same connection on subsequent reads.
#' @return \code{dbWriteTable()} and \code{dbCreateTable()} return invisibly TRUE, \code{dbAppendTable()} returns the number of written rows, and \code{dbRemoveTable()} returns invisibly TRUE or FALSE if a missing table is ignored.
#' @seealso \code{\link{RSqlClientNET-db-read}}, \code{\link{RSqlClientNET-db-connect}}
#' @examples \dontrun{
#' con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
#'   connection_string = Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
#' DBI::dbWriteTable(con, "#demo", data.frame(id = 1:3, label = letters[1:3]))
#' DBI::dbAppendTable(con, "#demo", data.frame(id = 4L, label = "d"),
#'   batch_rows = 65536L, table_lock = TRUE)
#' DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM #demo")
#' DBI::dbRemoveTable(con, "#demo")
#' DBI::dbDisconnect(con)
#' }
NULL

#' Optional dbplyr SQL Server translation
#'
#' @name RSqlClientNET-dbplyr
#' @aliases sql_dialect.SqlClientNETConnection
#' @aliases dbplyr_edition.SqlClientNETConnection
#' @description Use dbplyr's SQL Server dialect with RSqlClientNET DBI connections.
#' @details Requires optional \code{dbplyr >= 2.6.0}. Use \code{dbplyr::in_schema()} for schema-qualified tables; lazy \code{filter()}, \code{mutate()}, \code{summarise()}, \code{collect()} translate to T-SQL. \code{dbWriteTable()} and \code{dbAppendTable()} use SQL Server \code{SqlBulkCopy} through the existing DBI session. \code{dbplyr::copy_to()} relies on these DBI write methods. For local temporary tables, both the dbplyr logical table name and the physical \code{#name} can be used by DBI operations on the same connection; raw SQL must use \code{#name}. One active result cursor per connection.
#' @param con A \code{SqlClientNETConnection} from \code{DBI::dbConnect()}.
#' @usage \method{sql_dialect}{SqlClientNETConnection}(con)
#' \method{dbplyr_edition}{SqlClientNETConnection}(con)
#' @examples \dontrun{
#' con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=con_str)
#' remote <- dplyr::tbl(con, dbplyr::in_schema("dbo", "MyTable"))
#' x <- dplyr::collect(dplyr::filter(remote, id > 10))
#' DBI::dbDisconnect(con)
#' }
NULL

#' Internal developer fixtures and stress tests
#'
#' @name RSqlClientNET-internal-tests
#' @aliases sql_memory_test
#' @aliases sql_types_test
#' @aliases sql_edge_types_test
#' @aliases sql_edge_types_report
#' @aliases sql_timeout_test
#' @aliases sql_stream_timeout_test
#' @aliases sql_robustness_test
#' @aliases sql_stress_test
#' @aliases sql_stream_stress_test
#' @aliases sql_stream_integrity_test
#' @aliases sql_batch_benchmark
#' @aliases sql_batch_summary
#' @description Internal diagnostics used by the RSqlClientNET regression suite. Not part of the supported public API. They remain inside the namespace and may be accessed via \code{RSqlClientNET:::} for package development.
#' @details Run tests through \code{devtools::test()} from the package source. \code{test-00-} exercises local unit tests; \code{test-10-} and \code{test-11-} exercise the .NET helper without SQL Server; \code{test-20-} requires \code{RSQLCLIENTNET_TEST_CONNECTION_STRING} and should use a disposable SQL Server database. Developer helpers are not guaranteed to retain compatible argument signatures. Ordinary applications should use DBI, \code{sql_query_stream()}, and supported operational diagnostics instead.
#' @keywords internal
NULL

#' Inspect original SQL Server column types and define typed parameters
#'
#' @name RSqlClientNET-types
#' @aliases sql_schema
#' @aliases sql_param
#' @description Extract original SQL Server type metadata attached to a direct query/stream summary, or construct a typed SQL parameter descriptor.
#' @details \code{sql_schema()} reports SQL Server source types, not only R output classes. Ordinary \code{DBI::dbFetch()} results do not currently carry this RSqlClientNET-specific schema attribute; use \code{DBI::dbColumnInfo()} on a DBI result to inspect column metadata. Named scalar parameters can be bound through \code{DBI::dbGetQuery(..., params = list(...))}.
#' @param x A \code{sql_query()} result or \code{sql_query_stream()} summary with \code{sqlclient_schema} attribute.
#' @param value One SQL parameter value.
#' @param type SQL Server type name.
#' @param precision Optional SQL numeric precision.
#' @param scale Optional decimal scale.
#' @usage sql_schema(x)
#' sql_param(value, type, precision = NULL, scale = NULL)
#' @return \code{sql_schema} returns a data.frame; \code{sql_param} returns a typed parameter descriptor.
NULL

#' Connection state, reconnect and SQL Server isolation
#'
#' @name RSqlClientNET-validation
#' @aliases sql_connection_status
#' @aliases sql_reconnect
#' @aliases sql_begin
#' @aliases sql_commit
#' @aliases sql_rollback
#' @description Operational extensions usable on DBI and legacy RSqlClientNET connections.
#' @details \code{sql_connection_status()} reports local helper validity and invalidation reason without testing SQL Server health.
#' \code{sql_reconnect()} requires an already invalid connection; it creates a NEW SQL Server session and returns the SAME connection style (DBI S4 or legacy environment). DBI precision, timezone and batch defaults are preserved. The original connection string and credentials are not retained, and must be supplied again. An active SQL transaction, query or local \code{#temp} table is NOT recreated. Never replay writes or a failed COMMIT automatically.
#' \code{sql_begin()} allows explicit SQL Server transaction isolation; standard DBI \code{dbBegin()} uses the default. A DBI result must be cleared before SQL commands can be issued on the same session.
#' @param con A DBI SqlClientNETConnection or legacy sqlclient_connection.
#' @param ... New named authentication and connection arguments forwarded to \code{DBI::dbConnect()} or \code{sql_connect()} as appropriate. Credentials must be supplied again.
#' @param isolation ReadCommitted, ReadUncommitted, RepeatableRead, Serializable, or Snapshot (if enabled on SQL Server).
#' @param timeout Positive seconds for the operation.
#' @usage sql_connection_status(con)
#' sql_reconnect(con, ...)
#' sql_begin(con, isolation = "ReadCommitted", timeout = 30)
#' sql_commit(con, timeout = 30)
#' sql_rollback(con, timeout = 30)
#' @return Status returns a named list; reconnect a new connection; transaction helpers return server status invisibly.
NULL

#' Build the helper, query SQL Server and inspect performance
#'
#' @name RSqlClientNET
#' @aliases sql_connect
#' @aliases sqlclient_build
#' @aliases sql_ping
#' @aliases sql_query
#' @aliases sql_execute
#' @aliases sql_disconnect
#' @aliases sql_is_valid
#' @aliases sql_timings
#' @aliases sql_type_timings
#' @aliases sql_default_format
#' @description Legacy RSqlClientNET direct API and operational helpers. Prefer \code{DBI::dbConnect(SqlClientNET(), ...)} and other DBI methods for new programs. These direct functions are retained for compatibility.
#' @details The .NET helper is compiled automatically when installing RSqlClientNET from source via R CMD INSTALL / devtools::install(). Precompiled binary R packages include the published helper, so a .NET runtime version 8 or newer is needed to run it. The helper targets net8.0 and uses RollForward=LatestMajor to select the newest installed major runtime, such as .NET 10. \code{sqlclient_build(force = TRUE)} is for developer rebuilds.
#'
#' The recommended DBI cursor always uses Rcpp SCB2 column batches; \code{sql_default_format()} affects only direct \code{sql_query()} (default: \code{"columnar"}; \code{"csv"} is opt-in). Direct query functions accept a DBI handle for compatibility, but use their explicitly supplied conversion settings and legacy defaults rather than implicitly using all the settings from \code{dbConnect()}. New code should prefer \code{DBI::dbGetQuery()}.
#' \code{sql_timings()} and \code{sql_type_timings()} read attributes on direct query results or streaming summaries, not ordinary \code{DBI::dbFetch()} results. \code{sql_disconnect()} delegates to \code{DBI::dbDisconnect()} on DBI handles. \code{sql_is_valid()} checks local helper liveness, not server connectivity. \code{sql_ping()} and direct commands must not run while a DBI cursor is active or during a streaming callback. Data remain in memory without temporary SQL result files.
#' @param force Rebuild the helper in a private user cache, even if an installed helper exists. Normal installations do not need this.
#' @param con A legacy RSqlClientNET connection or an existing DBI RSqlClientNET connection where supported; \code{sql_ping(NULL)} starts a disposable helper.
#' @param server SQL Server host or instance.
#' @param database Initial database.
#' @param integrated_security Use Windows authentication where supported.
#' @param username SQL authentication user.
#' @param password SQL authentication password.
#' @param encrypt Enable TLS.
#' @param trust_server_certificate Skip server certificate verification; for controlled development only.
#' @param connection_string A Microsoft.Data.SqlClient connection string.
#' @param transport Only \code{"rcpp"} (native framed transport).
#' @param sql One SQL statement.
#' @param params Named scalar parameters, bound safely by SqlClient.
#' @param timeout Timeout in seconds.
#' @param parser CSV parser: \code{"fread"} or \code{"read.csv"}.
#' @param quote_correction Apply doubled quote correction to fread CSV results.
#' @param chunk_kib CSV streaming payload block size.
#' @param bigint \code{"integer64"} warns and returns NA for SQL -2^63; \code{"character"} preserves this value exactly.
#' @param datetime Override datetime conversion: \code{"character"} or \code{"POSIXct"}.
#' @param binary Binary SQL data output: \code{"raw"} list columns or \code{"base64"} strings.
#' @param unsupported How to handle unsupported SQL types.
#' @param precision \code{"full"} (exact character decimal and datetime) or \code{"native"} (numeric and POSIXct).
#' @param tz Time zone for timezone-naive SQL datetime values interpreted as POSIXct.
#' @param datetime_parser \code{"lubridate"} or \code{"base"}. DST gaps and overlaps are NA with warnings in native mode.
#' @param dst_fallback \code{"na"} returns NA for repeated autumn hours; \code{"ordered"} infers offsets from chronologically ordered rows, with a warning.
#' @param result_format \code{"csv"} or \code{"columnar"}. NULL uses the direct-API \code{sql_default_format()} preference (initially \code{"columnar"}).
#' @param batch_rows Maximum rows per binary column batch, from 1 to 32768; actual batches may be shorter to respect the 128 MiB frame limit.
#' @param format For \code{sql_timings}, list or single-row data.frame; for \code{sql_default_format}, CSV or columnar.
#' @param x A direct \code{sql_query()} result or \code{sql_query_stream()} summary with timing attributes.
#' @usage sqlclient_build(force = FALSE)
#' sql_connect(server = NULL, database = NULL, integrated_security = TRUE,
#'   username = NULL, password = NULL, encrypt = TRUE,
#'   trust_server_certificate = FALSE, connection_string = NULL,
#'   transport = "rcpp")
#' sql_ping(con = NULL, timeout = 10L, transport = "rcpp")
#' sql_query(con, sql, params = list(), timeout = 600L,
#'   parser = c("fread", "read.csv"), quote_correction = TRUE,
#'   chunk_kib = 256L, bigint = NULL, datetime = NULL,
#'   binary = "raw", unsupported = "error", precision = c("full", "native"),
#'   tz = "", datetime_parser = c("lubridate", "base"),
#'   dst_fallback = "na", result_format = NULL, batch_rows = 32768L)
#' sql_execute(con, sql, params = list(), timeout = 600L)
#' sql_disconnect(con)
#' sql_is_valid(con)
#' sql_timings(x, format = c("list", "data.frame"))
#' sql_type_timings(x)
#' sql_default_format(format = NULL)
#' @return Build returns a helper DLL path. Connect returns a legacy environment-based connection. Query returns a data.table, execute returns affected row count, disconnect returns invisibly TRUE, validity returns logical, ping returns helper details including the selected .NET runtime (`runtime_version`), timings returns a list or one-row data.frame, type timings returns a data.frame, and format setter returns the previous preference invisibly.
NULL

