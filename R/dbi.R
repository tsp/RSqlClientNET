#' DBI driver for SQL Server using Microsoft.Data.SqlClient
#'
#' The Rcpp/SCB2 backend keeps a SqlDataReader open in the .NET helper for
#' paged dbFetch(). One active result per connection is supported. Sending a
#' new DBI query invalidates an outstanding result with a warning. Explicitly
#' clear results with DBI::dbClearResult() before non-DBI extension commands.
#'
#' @return S4 SqlClientNETDriver object.
#' @export
#' @noRd
SqlClientNET <- function() methods::new("SqlClientNETDriver")

methods::setClass("SqlClientNETDriver", contains = "DBIDriver")
# sql_connect() deliberately adds an S3 class to an R environment.
# The S4 slot type "environment" rejects that classed environment, even
# though is.environment() is TRUE. Store it in an ANY slot instead, then
# enforce the actual backend contract in S4 validity checks.
methods::setClass("SqlClientNETConnection", contains = "DBIConnection",
                  slots = c(handle = "ANY"))
methods::setValidity("SqlClientNETConnection", function(object) {
  h <- object@handle
  if (!is.environment(h))
    return("slot 'handle' must contain an R environment")
  if (!inherits(h, "sqlclient_connection"))
    return("slot 'handle' must contain a sqlclient_connection")
  TRUE
})
methods::setClass("SqlClientNETResult", contains = "DBIResult",
                  slots = c(state = "environment"))

.dbi_handle <- function(conn) conn@handle
.dbi_state <- function(res) res@state
.dbi_connection <- function(con) {
  if (!inherits(con, "SqlClientNETConnection")) stop("Not a RSqlClientNET DBI connection.", call.=FALSE)
  h <- .dbi_handle(con)
  if (!sql_is_valid(h)) stop("SQL Server DBI connection is closed or invalid.", call.=FALSE)
  h
}
.dbi_active <- function(h) {
  !is.null(h$dbi_result) && !isTRUE(h$dbi_result$cleared)
}
.dbi_lock_check <- function(h) {
  if (isTRUE(h$stream_active))
    stop("Cannot issue DBI commands during sql_query_stream() callback.", call.=FALSE)
  if (.dbi_active(h)) stop("Clear the current DBI result with dbClearResult() first.", call.=FALSE)
}
.dbi_rss_begin <- function() proc.time()[["elapsed"]]
.dbi_max_batch <- function(n) {
  if (length(n) != 1L || !is.numeric(n) || is.na(n) ||
      !is.finite(n) || n != floor(n) || n < 1 || n > 32768)
    stop("batch_rows must be an integer between 1 and 32768.", call.=FALSE)
  as.integer(n)
}
.dbi_validate_timeout <- function(timeout) {
  if (!is.numeric(timeout) || length(timeout) != 1L || is.na(timeout) ||
      !is.finite(timeout) || timeout <= 0)
    stop("timeout must be a positive finite number of seconds.", call.=FALSE)
  timeout
}
.dbi_n <- function(n) {
  # DBItest specifies dbFetch(n = NA), where NA is a *logical* scalar.
  # Other logical values are not valid row limits.
  if (is.logical(n)) {
    if (length(n) == 1L && is.na(n)) return(-1L)
    stop("n must be -1, Inf, NA, or a nonnegative whole number.", call.=FALSE)
  }
  if (length(n) != 1L || !is.numeric(n) ||
      !((is.na(n) && !is.nan(n)) || (is.infinite(n) && n > 0) ||
        (is.finite(n) && n == floor(n) && n >= -1)))
    stop("n must be -1, Inf, or a nonnegative whole number.", call.=FALSE)
  if (is.na(n) || is.infinite(n) || n == -1) return(-1L)
  if (n > .Machine$integer.max) stop("n exceeds DBI cursor maximum for a single dbFetch call.", call.=FALSE)
  as.integer(n)
}

# A query's decoding configuration is a snapshot, not a mutable property of
# the connection.  dbSendQuery() accepts these overrides through DBI's ...;
# dbGetQuery() forwards them to dbSendQuery(), and dbReadTable() does likewise.
# The same snapshot is used at cursor_open, each dbFetch, and dbColumnInfo.
.dbi_query_options <- function(base, overrides = list()) {
  if (!length(overrides)) return(base)
  names_given <- names(overrides)
  if (is.null(names_given) || anyNA(names_given) ||
      any(!nzchar(names_given)) || anyDuplicated(names_given))
    stop("Query conversion overrides must be uniquely named arguments.", call.=FALSE)
  allowed <- c("precision", "bigint", "datetime", "binary", "unsupported",
               "tz", "datetime_parser", "dst_fallback", "data_table",
               "batch_rows", "timeout")
  unknown <- setdiff(names_given, allowed)
  if (length(unknown))
    stop("Unsupported RSqlClientNET query option(s): ",
         paste(unknown, collapse=", "), call.=FALSE)

  target <- base
  # Passing precision alone selects the corresponding datetime default unless
  # datetime was explicitly configured at dbConnect().  An explicit per-query
  # datetime argument always wins, including datetime=NULL (automatic).
  if ("precision" %in% names_given && !"datetime" %in% names_given &&
      isTRUE(base$datetime_automatic)) target$datetime <- NULL
  for (key in names_given) target[key] <- list(overrides[[key]])
  if ("datetime" %in% names_given)
    target$datetime_automatic <- is.null(overrides[["datetime"]])

  parsed <- .sql_precision_options(target$precision, target$bigint,
                                   target$datetime)
  target$precision <- parsed$precision
  target$bigint <- parsed$bigint
  target$datetime <- parsed$datetime
  target$tz <- .sql_validate_tz(target$tz)
  target$dst_fallback <- .sql_dst_fallback(target$dst_fallback)
  target$binary <- match.arg(target$binary, c("raw", "base64"))
  target$unsupported <- match.arg(target$unsupported, c("error", "character"))
  target$datetime_parser <- match.arg(target$datetime_parser, c("lubridate", "base"))
  target$batch_rows <- .dbi_max_batch(target$batch_rows)
  target$timeout <- .dbi_validate_timeout(target$timeout)
  if (!is.logical(target$data_table) || length(target$data_table) != 1L ||
      is.na(target$data_table))
    stop("data_table must be TRUE or FALSE.", call.=FALSE)
  target
}

# All DBI cursors use the native Rcpp/SCB2 framed transport.
methods::setMethod("dbConnect", "SqlClientNETDriver",
  function(drv, ..., transport = "rcpp", result_format = "columnar",
           batch_rows = 32768L, precision = "native", bigint = "integer64",
           datetime = NULL, binary = "raw", unsupported = "error",
           tz = "", datetime_parser = "lubridate",
           dst_fallback = "na", timeout = 600,
           data_table = FALSE) {
    if (!identical(transport, "rcpp"))
      stop("DBI cursor paging requires transport='rcpp'.", call.=FALSE)
    if (!identical(result_format, "columnar"))
      stop("DBI cursor paging currently requires result_format='columnar'.", call.=FALSE)
    batch_rows <- .dbi_max_batch(batch_rows)
    timeout <- .dbi_validate_timeout(timeout)
    if (!is.logical(data_table) || length(data_table) != 1L || is.na(data_table))
      stop("data_table must be TRUE or FALSE.", call.=FALSE)
    opts <- .sql_precision_options(precision, bigint, datetime)
    tz <- .sql_validate_tz(tz)
    datetime_parser <- match.arg(datetime_parser, c("lubridate", "base"))
    dst_fallback <- .sql_dst_fallback(dst_fallback)
    binary <- match.arg(binary, c("raw", "base64"))
    unsupported <- match.arg(unsupported, c("error", "character"))
    h <- sql_connect(..., transport="rcpp")
    # If DBI S4 construction or option assignment fails after opening the
    # native helper, close it immediately rather than leaking the process.
    object_created <- FALSE
    on.exit(if (!object_created) try(sql_disconnect(h), silent=TRUE), add=TRUE)
    h$dbi_result <- NULL
    h$dbi_temp_aliases <- new.env(parent=emptyenv())
    h$dbi_temp_names <- character()
    h$dbi_cursor_active <- FALSE
    h$dbi_transaction_active <- FALSE
    h$dbi_info <- list(db.version=NA_character_, dbname=NA_character_,
                       username=NA_character_, host=NA_character_,
                       port=NA_integer_)
    h$dbi_options <- list(batch_rows=batch_rows, timeout=timeout,
       precision=opts$precision, bigint=opts$bigint, datetime=opts$datetime,
       binary=binary, unsupported=unsupported, tz=tz,
       datetime_parser=datetime_parser, dst_fallback=dst_fallback,
       data_table=data_table, datetime_automatic=is.null(datetime))
    dbi_connection <- methods::new("SqlClientNETConnection", handle=h)
    # Cache metadata once, before any cursor is active. dbGetInfo() must not
    # issue a query while an SqlDataReader is open (MARS is not required).
    meta <- DBI::dbGetQuery(dbi_connection,
      paste0("SELECT CONVERT(nvarchar(128), SERVERPROPERTY('ProductVersion')) AS version, ",
             "DB_NAME() AS dbname, ORIGINAL_LOGIN() AS username"))
    if (nrow(meta) != 1L) stop("SQL Server metadata query returned no row.", call.=FALSE)
    val <- function(x) if (length(x)==1L && !is.na(x)) as.character(x) else NA_character_
    h$dbi_info$db.version <- val(meta$version)
    h$dbi_info$dbname <- val(meta$dbname)
    h$dbi_info$username <- val(meta$username)
    object_created <- TRUE
    dbi_connection
  })

methods::setMethod("dbIsValid", "SqlClientNETDriver", function(dbObj, ...) TRUE)
methods::setMethod("dbIsValid", "SqlClientNETConnection", function(dbObj, ...) {
  sql_is_valid(dbObj@handle)
})
methods::setMethod("dbIsValid", "SqlClientNETResult", function(dbObj, ...) {
  s <- .dbi_state(dbObj)
  !isTRUE(s$cleared) && sql_is_valid(s$con) &&
    identical(s$con$dbi_result, s)
})

methods::setMethod("dbDisconnect", "SqlClientNETConnection", function(conn, ...) {
  h <- .dbi_handle(conn)
  if (!sql_is_valid(h)) {
    warning("DBI connection is already closed or invalid.",call.=FALSE)
    return(invisible(TRUE))
  }
  if (.dbi_active(h)) {
    warning("DBI connection is being closed with an active result; the result has been invalidated.",
            call.=FALSE)
    # On a broken connection dbClearResult only changes R-side state.
    try(DBI::dbClearResult(methods::new("SqlClientNETResult", state=h$dbi_result)), silent=TRUE)
  }
  if (sql_is_valid(h)) sql_disconnect(h)
  else .sql_invalidate(h, "DBI disconnected")
  invisible(TRUE)
})

# DBI permits both named atomic vectors (each element is a distinct bind
# parameter) and named lists/data.frames (each column can contain vectorized
# sets).  Preserve typed scalar classes, including Date, POSIXct, integer64,
# difftime, raw/blob and POSIXlt. Values are never interpolated into SQL text.
.dbi_param_sets <- function(params) {
  if (is.data.frame(params)) params <- as.list(params)
  else if (inherits(params, "POSIXlt") || !is.list(params)) {
    if (!is.atomic(params) && !inherits(params, "POSIXlt"))
      stop("DBI params must be a named vector, list or data.frame.",call.=FALSE)
    # Use `[` rather than `[[` to preserve Date, integer64 and difftime
    # classes. POSIXlt is list-backed but semantically an atomic time vector.
    nm <- names(params)
    params <- lapply(seq_along(params), function(i) params[i])
    names(params) <- nm
  }
  if (length(params) &&
      (is.null(names(params)) || anyNA(names(params)) ||
       any(!nzchar(names(params))) || anyDuplicated(names(params))))
    stop("DBI params must have unique nonempty names.",call.=FALSE)
  if (!length(params)) return(list(list()))
  # DBI requires an explicit warning for factor bindings; string labels,
  # not integer factor codes, must reach SQL Server.
  if (any(vapply(params, is.factor, logical(1)))) {
    warning("Factor DBI parameters converted to character.",call.=FALSE)
    params <- lapply(params, function(p) if (is.factor(p)) as.character(p) else p)
  }
  lengths <- vapply(params,function(p) {
    if (inherits(p,"sqlclient_parameter") || is.raw(p) || is.null(p)) return(1L)
    length(p)
  },integer(1))
  if (length(unique(lengths))!=1L)
    stop("All DBI parameter columns must have equal lengths.",call.=FALSE)
  count <- lengths[[1L]]
  if (count==0L) return(list())
  if (count > 1000000L) stop("At most one million DBI parameter sets per bind.",call.=FALSE)
  names_p <- names(params)
  lapply(seq_len(count),function(i) {
    row <- vector("list",length(params)); names(row) <- names_p
    for (j in seq_along(params)) {
      p <- params[[j]]
      scalar <- if (inherits(p,"sqlclient_parameter") || is.raw(p) || is.null(p)) p
                else if (inherits(p,"POSIXlt")) as.POSIXct(p[i])
                else if (is.list(p)) p[[i]]
                else p[i]
      # Some callers supply blob columns as a list containing a single raw
      # vector; flatten only this specific scalar binary/NULL representation.
      scalar <- .sql_unwrap_binary_param(scalar)
      # Named atomic parameter vectors identify parameters by their names,
      # but a *scalar parameter value* must not retain that name.  Subsetting
      # with `[` preserves important S3 classes (Date, integer64, POSIXct),
      # so drop only the name attribute rather than unclassing the value.
      if (!is.null(names(scalar))) names(scalar) <- NULL
      row[j] <- list(scalar) # preserves SQL NULL (R NULL) elements
    }
    row
  })
}

.dbi_close_cursor <- function(s) {
  if (is.null(s$id)) return(invisible(NULL))
  h <- s$con
  if (sql_is_valid(h))
    .sql_rpc(h,list(op="cursor_close",cursorId=s$id),
             timeout=min(10,s$options$timeout))
  s$id <- NULL
  h$dbi_cursor_active <- FALSE
  invisible(NULL)
}


# DBItest passes a named @a statement to dbSendQuery()/dbSendStatement()
# WITHOUT params, then binds later.  SQL Server cannot execute that text
# immediately (SQL error 137), but its terminal SqlException is recoverable:
# no cursor has been opened, and the helper has consumed the entire reply.
# DBI's default immediate=NULL permits falling back from direct execution to
# deferred binding.  Only this specific error is eligible; real SQL syntax,
# transport, timeout and explicit immediate=TRUE errors must propagate.
.dbi_defer_on_unbound_parameter <- function(e, sql, params, immediate) {
  is.null(params) && is.null(immediate) &&
    inherits(e, "sqlclient_recoverable_sql_error") &&
    length(e$sql_number) == 1L &&
    !is.na(suppressWarnings(as.integer(e$sql_number))) &&
    identical(suppressWarnings(as.integer(e$sql_number)), 137L) &&
    grepl("(?<!@)@[A-Za-z_][A-Za-z_0-9]*", sql, perl=TRUE)
}

# SQL Server's SqlCommand accepts named parameters; explicitly reject
# surplus R bindings in the DBI API, even if the server would silently
# ignore them.  Ignore quoted strings, comments, [] and @@system variables.
# Do not inspect EXEC parameter assignments (e.g. EXEC proc @arg=@value),
# whose left-hand names are procedure arguments, not bound SQL variables.
.dbi_sql_param_names <- function(sql) {
  code <- gsub(
    "(?s)/\\*.*?\\*/|'(?:''|[^'])*'|\"(?:\"\"|[^\"])*\"|\\[(?:\\]\\]|[^\\]])*\\]|--[^\\r\\n]*",
    " ", sql, perl=TRUE
  )
  hits <- regmatches(code, gregexpr("(?<!@)@[A-Za-z_][A-Za-z_0-9]*", code, perl=TRUE))[[1L]]
  unique(substring(hits, 2L))
}
.dbi_check_bound_names <- function(sql, params) {
  if (!length(params)) return(invisible(NULL))
  # Let the DBI scalar-vector routine validate missing or duplicate names.
  if (is.null(names(params)) || anyNA(names(params)) ||
      any(!nzchar(names(params))) || anyDuplicated(names(params))) return(invisible(NULL))
  if (grepl("^\\s*(?:EXEC(?:UTE)?|DECLARE)\\b", sql, ignore.case=TRUE, perl=TRUE))
    return(invisible(NULL))
  referenced <- .dbi_sql_param_names(sql)
  if (!length(referenced)) return(invisible(NULL))
  excess <- setdiff(names(params), referenced)
  if (length(excess))
    stop("Unexpected DBI binding name(s): ", paste(excess, collapse=", "),
         "; SQL uses: ", paste(referenced, collapse=", "), ".", call.=FALSE)
  invisible(NULL)
}

.dbi_bind_query <- function(s,params) {
  h <- s$con
  if (isTRUE(s$cleared) || !identical(h$dbi_result,s))
    stop("DBI result is no longer active.",call.=FALSE)
  .dbi_check_bound_names(s$sql, params)
  sets <- .dbi_param_sets(params)
  if (!is.null(s$id)) .dbi_close_cursor(s)
  s$bind_sets <- sets
  s$bind_index <- 0L
  s$row_count <- 0
  s$complete <- FALSE
  s$pending <- FALSE
  s$zero_bind <- !length(sets)
  s$schema_names <- NULL
  s$schema_types <- NULL
  s$dst_states <- new.env(parent=emptyenv())
  if (length(sets)) {
    s$pending <- TRUE
    .dbi_open(s,sets[[1L]])
    s$bind_index <- 1L
  } else {
    s$complete <- TRUE
    s$columns <- character(); s$types <- character(); s$kinds <- character()
  }
  invisible(NULL)
}

.dbi_next_bound_cursor <- function(s) {
  if (s$bind_index >= length(s$bind_sets)) return(FALSE)
  .dbi_close_cursor(s)
  s$pending <- TRUE
  s$complete <- FALSE
  i <- s$bind_index+1L
  .dbi_open(s,s$bind_sets[[i]])
  s$bind_index <- i
  TRUE
}

.dbi_bind_statement <- function(s,params) {
  if (isTRUE(s$cleared) || !identical(s$con$dbi_result,s))
    stop("DBI result is no longer active.",call.=FALSE)
  .dbi_check_bound_names(s$sql, params)
  sets <- .dbi_param_sets(params)
  # No implicit transaction: SQL Server's default statement-level semantics
  # apply.  On a mid-vector error earlier statements may have committed;
  # callers requiring atomicity must use dbBegin()/dbRollback().
  counts <- numeric(length(sets))
  for (i in seq_along(sets)) {
    values <- sets[[i]]
    # Execute while this DBIResult is registered. sql_execute() intentionally
    # rejects an outstanding DBI result, so use the same validated RPC directly.
    timeout <- s$con$dbi_options$timeout
    reply <- .sql_rpc(s$con,list(op="execute",sql=s$sql,
                 params=.sql_encode_params(values),timeout=as.integer(timeout)),
                 timeout=timeout)
    one <- as.integer(reply$affected)
    counts[[i]] <- if (length(one)!=1L || is.na(one)) NA_real_ else as.numeric(one)
  }
  affected <- if (!length(counts)) 0 else if (length(counts)==1L) counts[[1L]]
              else if (anyNA(counts) || any(counts < 0)) NA_real_
              else sum(counts)
  s$affected <- if (!is.na(affected) && abs(affected) <= .Machine$integer.max)
                  as.integer(affected) else NA_integer_
  s$pending <- FALSE
  s$complete <- TRUE
  invisible(NULL)
}

.dbi_open <- function(s, params = list()) {
  h <- s$con
  if (!sql_is_valid(h)) stop("SQL Server connection is invalid.", call.=FALSE)
  if (!identical(h$dbi_result, s)) stop("Result no longer belongs to this connection.", call.=FALSE)
  if (!isTRUE(s$pending)) stop("Result was already submitted.", call.=FALSE)
  o <- s$options
  request <- list(op="cursor_open", sql=s$sql, params=.sql_encode_params(params),
                  timeout=as.integer(o$timeout), batchRows=o$batch_rows,
                  columnarPrecision=o$precision, columnarDatetime=o$datetime,
                  columnarTz=o$tz)
  start <- .sql_rpc(h, request, timeout=o$timeout)
  cursor_id <- suppressWarnings(as.numeric(start$cursorId))
  if (!identical(start$kind,"cursor_open") ||
      !identical(start$valueEncoding,"columnar-v2") ||
      length(cursor_id)!=1L || !is.finite(cursor_id) || cursor_id < 1 ||
      cursor_id != floor(cursor_id)) {
    .sql_invalidate(h, "Invalid DBI cursor open response")
    stop("Invalid DBI cursor response.",call.=FALSE)
  }
  # Validate the exact C# schema before any data frame arrives. Keep the
  # diagnostics in the error: a mismatch can otherwise be impossible to
  # identify when a helper starts with a different binary build.
  types <- unname(as.character(unlist(start$types, use.names=FALSE)))
  column_names <- unname(as.character(unlist(start$columns, use.names=FALSE)))
  kinds <- unname(as.character(unlist(start$wireKinds, use.names=FALSE)))
  expected <- .sql_wire_kind(types,o$precision,o$datetime,o$tz)
  expected <- unname(as.character(expected))
  schema_ok <- length(types) == length(column_names) &&
    length(kinds) == length(types) && length(types) <= 4096L &&
    identical(kinds, expected)
  if (!schema_ok) {
    report <- paste0(
      "DBI columnar schema mismatch for SQL: ", substr(s$sql, 1L, 250L),
      "\nC# column types: ", paste(types, collapse=", "),
      "\nC# wire kinds: ", paste(kinds, collapse=", "),
      "\nR expected kinds: ", paste(expected, collapse=", "),
      "\nColumns: ", paste(column_names, collapse=", "),
      "\nOptions: precision=", o$precision,
      ", datetime=", o$datetime, ", tz=", shQuote(o$tz),
      ". Rebuild the .NET helper after installing this package."
    )
    .sql_invalidate(h, "DBI columnar schema mismatch")
    stop(report, call.=FALSE)
  }
  if (is.null(s$schema_names)) {
    s$schema_names <- column_names
    s$schema_types <- types
  } else if (!identical(s$schema_names,column_names) ||
             !identical(s$schema_types,types)) {
    .sql_invalidate(h,"Column schema changed between DBI parameter sets")
    stop("DBI parameter sets returned different column schemas.",call.=FALSE)
  }
  s$id <- as.numeric(start$cursorId)
  s$dst_states <- new.env(parent=emptyenv())
  s$columns <- column_names
  s$types <- types
  s$kinds <- kinds
  s$pending <- FALSE
  s$complete <- !isTRUE(start$hasMore)
  h$dbi_cursor_active <- TRUE
  invisible(NULL)
}

.dbi_send <- function(h, sql, params=NULL, immediate=NULL, statement=FALSE,
                      query_options=list()) {
  # Validate before replacing an existing live cursor, so invalid options
  # never invalidate an unrelated result.
  overrides <- if (statement) h$dbi_options else
    .dbi_query_options(h$dbi_options, query_options)
  if (!sql_is_valid(h)) stop("Connection is not valid.",call.=FALSE)
  if (isTRUE(h$stream_active))
    stop("Cannot issue DBI commands during sql_query_stream() callback.",call.=FALSE)
  # DBI allows only one active result for backends without MARS, but the
  # NEXT dbSendQuery/dbSendStatement must replace the stale result, warning
  # about invalidation. Extension calls still use .dbi_lock_check() to avoid
  # accidentally consuming a partially streamed result.
  if (.dbi_active(h)) {
    old <- h$dbi_result
    if (!isTRUE(old$is_statement) && !is.null(old$id)) .dbi_close_cursor(old)
    old$cleared <- TRUE
    h$dbi_result <- NULL
    h$dbi_cursor_active <- FALSE
    warning("Previous DBI result was not cleared; it has been invalidated.",call.=FALSE)
  }
  if (!is.character(sql) || length(sql)!=1L || is.na(sql))
    stop("statement must be a single non-missing character string.",call.=FALSE)
  if (!is.null(immediate) && (!is.logical(immediate) || length(immediate)!=1L || is.na(immediate)))
    stop("immediate must be TRUE, FALSE or NULL.",call.=FALSE)
  if (statement) {
    s <- new.env(parent=emptyenv())
    s$con<-h; s$sql<-sql; s$pending<-TRUE; s$is_statement<-TRUE
    s$cleared<-FALSE; s$complete<-FALSE; s$row_count<-0; s$affected<-NA_integer_
    h$dbi_result<-s
    tryCatch({
      if (!identical(immediate,FALSE) || !is.null(params))
        .dbi_bind_statement(s,params %||% list())
    },error=function(e) {
      if (.dbi_defer_on_unbound_parameter(e, sql, params, immediate) &&
          sql_is_valid(h)) {
        s$pending <- TRUE
        s$complete <- FALSE
        return(invisible(NULL))
      }
      s$cleared<-TRUE; h$dbi_result<-NULL; stop(e)
    })
    return(methods::new("SqlClientNETResult",state=s))
  }
  s <- new.env(parent=emptyenv())
  s$con<-h; s$sql<-sql; s$pending<-TRUE; s$is_statement<-FALSE
  s$options <- overrides
  s$cleared<-FALSE; s$complete<-FALSE; s$row_count<-0; s$affected<-0L
  h$dbi_result<-s
  tryCatch({
    if (!identical(immediate,FALSE) || !is.null(params)) .dbi_bind_query(s,params %||% list())
  },error=function(e) {
    if (.dbi_defer_on_unbound_parameter(e, sql, params, immediate) &&
        sql_is_valid(h)) {
      s$pending <- TRUE
      s$complete <- FALSE
      s$bind_sets <- NULL
      s$bind_index <- 0L
      return(invisible(NULL))
    }
    s$cleared<-TRUE; h$dbi_result<-NULL; h$dbi_cursor_active<-FALSE; stop(e)
  })
  methods::new("SqlClientNETResult",state=s)
}
# Both dbSendQuery() and dbSendStatement() dispatch on (conn, statement).
# Restrict BOTH slots: DBI's (DBIConnection, character) methods otherwise
# compete with a RSqlClientNET (SqlClientNETConnection, ANY) method and can win.
# SQL is a subclass of character. Register it explicitly as well to
# avoid ambiguity against DBIConnection#SQL inherited methods.
for (.statement_type in c("character", "SQL")) {
  methods::setMethod("dbSendQuery",
    signature(conn="SqlClientNETConnection",statement=.statement_type),
    function(conn,statement,...,params=NULL,immediate=NULL) {
      .dbi_send(.dbi_connection(conn),statement,params=params,immediate=immediate,
                query_options=list(...))
    })
  methods::setMethod("dbSendStatement",
    signature(conn="SqlClientNETConnection",statement=.statement_type),
    function(conn,statement,...,params=NULL,immediate=NULL) {
      .dbi_send(.dbi_connection(conn),statement,params=params,immediate=immediate,statement=TRUE)
    })
}
rm(.statement_type)

methods::setMethod("dbBind", "SqlClientNETResult", function(res, params, ...) {
  s <- .dbi_state(res)
  if (isTRUE(s$cleared) || !sql_is_valid(s$con))
    stop("dbBind() requires a valid, uncleared result.",call.=FALSE)
  if (!length(params)) stop("dbBind() requires a nonempty parameter set.",call.=FALSE)
  if (isTRUE(s$is_statement)) .dbi_bind_statement(s,params)
  else .dbi_bind_query(s,params)
  invisible(res)
})

.dbi_fetch_frame <- function(s,n) {
  h <- s$con
  o <- s$options
  profile_read <- isTRUE(getOption("RSqlClientNET.dbi_read_profile", FALSE))
  profile_alloc <- isTRUE(getOption("RSqlClientNET.dbi_alloc_profile", FALSE))
  profile_start <- if (profile_read) proc.time()[["elapsed"]] else NULL
  profile_recv <- 0
  profile_decode <- 0
  profile_cast <- 0
  profile_table_wrap <- 0
  profile_type_cast <- 0
  profile_direct_batches <- 0L
  profile_helper <- NULL
  deadline <- Sys.time()+o$timeout
  request <- list(op="cursor_fetch",cursorId=s$id,n=n,
                  allocationProfile=profile_alloc)
  completed <- FALSE
  on.exit(if(!completed) .sql_invalidate(h,"Incomplete DBI cursor fetch"),add=TRUE)
  .sql_send(h,jsonlite::toJSON(request,auto_unbox=TRUE,null="null",digits=NA),deadline)
  head <- .sql_read_reply(h,deadline)
  if (!identical(head$kind,"start") || !identical(head$valueEncoding,"columnar-v2") ||
      !identical(as.numeric(head$cursorId),s$id))
    stop("Invalid DBI fetch start frame.",call.=FALSE)
  batches<-list(); count<-0; bytes<-0; total<-0
  # These SCB2 kinds already decode into their final DBI R types. Avoid
  # making a data.table and running the conversion dispatcher for every
  # single-column chunk. rbindlist() accepts named lists of column vectors.
  # Multi-column, Base64 and other SQL types retain the established path.
  direct_single_column <- length(s$columns) == 1L && (
    (identical(s$kinds[[1L]], "Y") && identical(o$binary, "raw")) ||
    (identical(s$kinds[[1L]], "S") &&
     tolower(s$types[[1L]]) %in% c("xml", "varchar", "nvarchar",
                                  "char", "nchar", "text", "ntext"))
  )

  repeat {
    if (profile_read) profile_t0 <- proc.time()[["elapsed"]]
    frame <- .sql_read_reply(h,deadline,allow_data=TRUE)
    if (profile_read) profile_recv <- profile_recv + proc.time()[["elapsed"]] - profile_t0
    if (identical(frame$kind,"end")) break
    if (!identical(frame$kind,"chunk")) stop("Unexpected DBI cursor frame.",call.=FALSE)
    raw <- frame$data
    bytes<-bytes+length(raw)
    if (profile_read) profile_t0 <- proc.time()[["elapsed"]]
    decoded <- .Call("sqlcpp_decode_batch",raw, o$bigint=="integer64",o$tz,PACKAGE="RSqlClientNET")
    if (profile_read) profile_decode <- profile_decode + proc.time()[["elapsed"]] - profile_t0
    if (!identical(as.character(decoded$kinds),s$kinds) ||
        length(decoded$columns)!=length(s$columns) || decoded$rows>o$batch_rows)
      stop("DBI batch schema mismatch.",call.=FALSE)
    .sql_warn_decoded_bigint_min(decoded, s$columns, row_offset=s$row_count+total)
    if (profile_read) profile_t0 <- proc.time()[["elapsed"]]
    if (direct_single_column) {
      # No data.table wrapper or type coercion is needed: SCB2 kind Y is
      # already a list of raw vectors/NULLs, and SQL XML/plain-text kind S
      # is UTF-8 character with R's usual NA_character_ for SQL NULL.
      batch <- decoded$columns
      names(batch) <- s$columns
      profile_direct_batches <- profile_direct_batches + 1L
    } else {
      batch <- data.table::as.data.table(decoded$columns)
      data.table::setnames(batch,s$columns)
      if (profile_read) {
        profile_table_wrap <- profile_table_wrap + proc.time()[["elapsed"]] - profile_t0
        profile_type_t0 <- proc.time()[["elapsed"]]
      }
      # Offsets cover rows already fetched in previous dbFetch() calls AND
      # earlier batches of this same fetch; warnings use 1-based global rows.
      cast <- .sql_columnar_cast(batch,s$types,s$kinds,o$bigint,o$datetime,
                o$binary,o$unsupported,o$precision,o$tz,o$datetime_parser,
                row_offset=s$row_count+total,
                dst_fallback=o$dst_fallback, dst_states=s$dst_states)
      if (profile_read) profile_type_cast <- profile_type_cast + proc.time()[["elapsed"]] - profile_type_t0
      batch <- cast$data
    }
    count <- count+1L
    total<-total+decoded$rows
    batches[[count]]<-batch
    if (profile_read) profile_cast <- profile_cast + proc.time()[["elapsed"]] - profile_t0
  }
  if (!identical(as.numeric(frame$cursorId),s$id) || total!=as.numeric(frame$rows) ||
      count!=as.numeric(frame$chunksSent) || bytes!=as.numeric(frame$bytesSent) ||
      (n >= 0L && total > n))
    stop("DBI cursor count/byte mismatch.",call.=FALSE)
  if (profile_read) profile_t0 <- proc.time()[["elapsed"]]
  dt <- data.table::rbindlist(batches,use.names=TRUE,fill=FALSE)
  profile_assemble <- if (profile_read) proc.time()[["elapsed"]] - profile_t0 else NULL
  # A columnar frame contains typed columns but not their SQL column names.
  # An expression such as SELECT 10000000000 can have a blank column name.
  # rbindlist() may repair empty names; restore the exact reader schema here.
  if (ncol(dt) != length(s$columns))
    stop(sprintf("DBI cursor returned %d columns, expected %d.",
                 ncol(dt),length(s$columns)),call.=FALSE)
  if (length(s$columns)) data.table::setnames(dt,s$columns)
  s$complete <- !isTRUE(frame$hasMore)
  s$row_count <- s$row_count+total
  completed<-TRUE
  if (profile_read) {
    message(sprintf(paste0(
      "RSqlClientNET DBI read: rows=%d chunks=%d bytes=%.1f MB, ",
      "total=%.3fs, receive_wait=%.3fs, decode=%.3fs, ",
      "R_cast=%.3fs (table_wrap=%.3fs, type_cast=%.3fs, direct_chunks=%d), ",
      "rbind=%.3fs; helper total=%.3fs ",
      "(SQL_read=%.3fs, encode_pipe=%.3fs, single_column_fast=%s)"),
      total, count, bytes / 1048576,
      proc.time()[["elapsed"]] - profile_start,
      profile_recv, profile_decode, profile_cast,
      profile_table_wrap, profile_type_cast, profile_direct_batches,
      profile_assemble,
      as.numeric(frame$fetchAndStreamSeconds %||% NA_real_),
      as.numeric(frame$readAndBufferSeconds %||% NA_real_),
      as.numeric(frame$encodeAndPipeSeconds %||% NA_real_),
      as.character(frame$singleColumnFastPath %||% NA)))
  }
  if (profile_alloc && !is.null(frame$allocations)) {
    a <- frame$allocations
    message(sprintf(paste0(
      "RSqlClientNET allocation read: rows=%d, nonnull=%d, ",
      "thread=%.3f MB, process=%.3f MB, ",
      "GetValue=%.3f MB / %.1fms, UTF8_length=%.3f MB / %.1fms, ",
      "SCB2_encode=%.3f MB / %.1fms, GC=(%d,%d,%d), xml_mode=%s"),
      total, a$materialized,
      a$threadBytes / 1048576, a$processBytes / 1048576,
      a$valueReadBytes / 1048576, a$getValueMilliseconds,
      a$lengthBytes / 1048576, a$lengthMilliseconds,
      a$encodeBytes / 1048576, a$encodeMilliseconds,
      a$gen0, a$gen1, a$gen2,
      as.character(a$xmlReadMode %||% "getvalue")))
  }
  dt
}
# Assemble exactly n rows over one or more bound parameter executions.
.dbi_fetch_bound <- function(s,n) {
  if (isTRUE(s$zero_bind)) return(data.table::as.data.table(
    stats::setNames(vector("list",length(s$columns)),s$columns)))
  if (is.null(s$bind_sets)) return(.dbi_fetch_frame(s,n))
  remaining <- n
  fragments <- list()
  repeat {
    if (remaining==0L) {
      # Do not open a new SQL execution just to fetch zero rows.
      # Request a typed empty frame from the existing cursor if needed.
      if (!length(fragments)) fragments[[1L]] <- .dbi_fetch_frame(s,0L)
      break
    }
    if (isTRUE(s$complete) && s$bind_index < length(s$bind_sets))
      .dbi_next_bound_cursor(s)
    piece <- .dbi_fetch_frame(s,remaining)
    fragments[[length(fragments)+1L]] <- piece
    got <- nrow(piece)
    if (remaining >= 0L) remaining <- remaining-got
    if (remaining==0L || (isTRUE(s$complete) &&
        s$bind_index >= length(s$bind_sets))) break
    if (!isTRUE(s$complete) && got==0L)
      stop("DBI cursor returned no rows before completion.",call.=FALSE)
  }
  data.table::rbindlist(fragments,use.names=TRUE,fill=FALSE)
}
# SQL Server permits unnamed SELECT expressions.  DBI requires the names in
# dbColumnInfo() and dbFetch() to match; expose blank names as V1, V2, ...
# independently of whether the caller requests data.frame or data.table.
# Avoid clashing with an existing real column called V1, V2, etc.
.dbi_result_names <- function(nms) {
  if (!length(nms)) return(nms)
  empty <- which(is.na(nms) | !nzchar(nms))
  if (!length(empty)) return(nms)
  used <- nms[-empty]
  next_no <- 1L
  for (i in empty) {
    candidate <- paste0("V", next_no)
    while (candidate %in% used) {
      next_no <- next_no + 1L
      candidate <- paste0("V", next_no)
    }
    nms[[i]] <- candidate
    used <- c(used, candidate)
    next_no <- next_no + 1L
  }
  nms
}
# DBI requires a plain data.frame by default.  The data.table fast path is
# available explicitly per connection and retains the same columnar transport.
.dbi_format_frame <- function(s, rows) {
  wanted <- .dbi_result_names(s$columns)
  if (length(wanted) == ncol(rows)) data.table::setnames(rows, wanted)
  if (isTRUE(s$options$data_table)) return(rows)
  as.data.frame(rows, optional=TRUE, stringsAsFactors=FALSE)
}
methods::setMethod("dbFetch", "SqlClientNETResult",function(res,n=-1,...) {
  s <- .dbi_state(res)
  n <- .dbi_n(n)
  if (isTRUE(s$cleared) || !sql_is_valid(s$con))
    stop("DBI result is cleared or its connection is invalid.",call.=FALSE)
  if (isTRUE(s$pending)) stop("Result has not been bound. Call dbBind().",call.=FALSE)
  if (isTRUE(s$is_statement)) {
    warning("dbFetch() on a statement result returns no rows.",call.=FALSE)
    return(data.frame())
  }
  if (!identical(s$con$dbi_result,s)) stop("DBI result is not active.",call.=FALSE)
  .dbi_format_frame(s, .dbi_fetch_bound(s,n))
})
methods::setMethod("dbHasCompleted","SqlClientNETResult",function(res,...) {
  s <- .dbi_state(res)
  if (isTRUE(s$cleared)) stop("Result has been cleared.",call.=FALSE)
  if (!sql_is_valid(s$con)) stop("Result connection is invalid.",call.=FALSE)
  isTRUE(s$complete) &&
    (is.null(s$bind_sets) || s$bind_index >= length(s$bind_sets))
})
methods::setMethod("dbClearResult","SqlClientNETResult",function(res,...) {
  s <- .dbi_state(res)
  if (isTRUE(s$cleared)) {
    warning("DBI result has already been cleared or invalidated.",call.=FALSE)
    return(invisible(TRUE))
  }
  h<-s$con
  on.exit({s$cleared<-TRUE; if (identical(h$dbi_result,s)) h$dbi_result<-NULL;
           h$dbi_cursor_active<-FALSE},add=TRUE)
  if (!isTRUE(s$is_statement) && !is.null(s$id) && sql_is_valid(h))
    .dbi_close_cursor(s)
  invisible(TRUE)
})
# Introspection on cleared/invalidated results must fail instead of returning
# stale values from an R environment whose native cursor is no longer alive.
.dbi_live_state <- function(res) {
  if (!DBI::dbIsValid(res)) stop("DBI result is no longer valid.",call.=FALSE)
  .dbi_state(res)
}
methods::setMethod("dbGetRowCount","SqlClientNETResult",function(res,...) .dbi_live_state(res)$row_count)
methods::setMethod("dbGetRowsAffected","SqlClientNETResult",function(res,...) .dbi_live_state(res)$affected)
methods::setMethod("dbGetStatement","SqlClientNETResult",function(res,...) .dbi_live_state(res)$sql)
methods::setMethod("dbColumnInfo","SqlClientNETResult",function(res,...) {
  s<-.dbi_state(res)
  if (isTRUE(s$cleared)) stop("Result has been cleared.",call.=FALSE)
  if (isTRUE(s$pending)) stop("Result has not been bound.",call.=FALSE)
  if (isTRUE(s$is_statement)) return(data.frame(name=character(),type=character()))
  o<-s$options
  kinds<-s$kinds
  types<-vapply(seq_along(kinds),function(i){
    k<-kinds[[i]]
    if (k=="I") return("integer")
    if (k=="L") return(switch(o$bigint,integer64="integer64",
                              character="character",numeric="numeric",
                              integer="integer"))
    if (k %in% c("N","D")) return("numeric")
    if (k=="B") return("logical")
    if (k=="A") return("Date")
    if (k %in% c("T","O")) return("POSIXct")
    if (k=="H") return("difftime")
    if (k=="Y") return(if(o$binary=="raw") "list" else "character")
    if (tolower(s$types[[i]]) %in% c("datetime","datetime2","smalldatetime","datetimeoffset") && o$datetime=="POSIXct") return("POSIXct")
    "character"
  },character(1))
  data.frame(name=.dbi_result_names(s$columns),type=types,stringsAsFactors=FALSE)
})
methods::setMethod("dbBegin","SqlClientNETConnection",function(conn,...) {
  h <- .dbi_connection(conn)
  if (isTRUE(h$dbi_transaction_active))
    stop("A transaction is already active; nested DBI transactions are not supported.",call.=FALSE)
  sql_begin(h)
  h$dbi_transaction_active <- TRUE
  invisible(TRUE)
})
methods::setMethod("dbCommit","SqlClientNETConnection",function(conn,...) {
  h <- .dbi_connection(conn)
  if (!isTRUE(h$dbi_transaction_active))
    stop("Cannot commit: no active DBI transaction.",call.=FALSE)
  sql_commit(h)
  h$dbi_transaction_active <- FALSE
  invisible(TRUE)
})
methods::setMethod("dbRollback","SqlClientNETConnection",function(conn,...) {
  h <- .dbi_connection(conn)
  if (!isTRUE(h$dbi_transaction_active))
    stop("Cannot roll back: no active DBI transaction.",call.=FALSE)
  sql_rollback(h)
  h$dbi_transaction_active <- FALSE
  invisible(TRUE)
})
methods::setMethod("dbGetInfo", "SqlClientNETConnection", function(dbObj, ...) {
  h <- .dbi_handle(dbObj)
  info <- h$dbi_info
  if (is.null(info)) info <- list(db.version=NA_character_, dbname=NA_character_,
                                  username=NA_character_, host=NA_character_,
                                  port=NA_integer_)
  c(info, list(dbms.name="Microsoft SQL Server",
               driver.version=as.character(utils::packageVersion("RSqlClientNET")),
               transport=h$transport, connection.valid=sql_is_valid(h)))
})
methods::setMethod("dbGetInfo", "SqlClientNETDriver", function(dbObj, ...) {
  list(driver.version=as.character(utils::packageVersion("RSqlClientNET")),
       name="RSqlClientNET", client.version="Microsoft.Data.SqlClient (managed helper)")
})
# DBI::dbQuoteIdentifier()/dbQuoteString() are TWO-argument S4 generics.
# Specialise both dispatch arguments so the DBIConnection#Id and
# DBIConnection#character defaults never override SQL Server quoting.
.dbi_quote_identifier <- function(conn, x) {
  if (inherits(x, "SQL")) return(x)
  if (is.character(x) && !inherits(x, "Id")) {
    xnames <- names(x)
    x <- vapply(x, function(one) as.character(.dbi_temp_resolve(conn, one)), character(1))
    names(x) <- xnames
  } else {
    x <- .dbi_temp_resolve(conn,x)
  }
  if (inherits(x, "Id")) {
    parts <- methods::slot(x, "name")
    if (!length(parts) || anyNA(parts))
      stop("Id must contain one or more non-missing components.",call.=FALSE)
    part_names <- names(parts)
    # Named components may be specified in arbitrary order.
    # SQL Server multipart syntax is [catalog].[schema].[table].
    if (!is.null(part_names) &&
        all(part_names %in% c("catalog", "schema", "table"))) {
      parts <- parts[order(match(part_names,c("catalog","schema","table")))]
    }
    parts <- unname(parts)
    return(DBI::SQL(paste0("[",gsub("]","]]",parts,fixed=TRUE),"]", collapse=".")))
  }
  if (!is.character(x) || anyNA(x))
    stop("Identifiers must be non-missing character strings or DBI::Id.",call.=FALSE)
  if (!length(x)) return(DBI::SQL(character()))
  DBI::SQL(paste0("[",gsub("]","]]",x,fixed=TRUE),"]"), names=names(x))
}
for (.quote_type in c("Id","character","SQL")) {
  methods::setMethod("dbQuoteIdentifier",
    signature(conn="SqlClientNETConnection",x=.quote_type),
    function(conn,x,...) .dbi_quote_identifier(conn,x))
}
rm(.quote_type)

.dbi_quote_string <- function(conn,x) {
  if (inherits(x,"SQL")) return(x)
  if (!is.character(x))
    stop("dbQuoteString() requires a character vector or DBI::SQL().",call.=FALSE)
  if (!length(x)) return(DBI::SQL(character()))
  DBI::SQL(ifelse(is.na(x),"NULL",paste0("N'",gsub("'","''",x,fixed=TRUE),"'")),
           names=names(x))
}
for (.quote_type in c("character","SQL")) {
  methods::setMethod("dbQuoteString",
    signature(conn="SqlClientNETConnection",x=.quote_type),
    function(conn,x,...) .dbi_quote_string(conn,x))
}
rm(.quote_type)

# In SQL Server, brackets are not regex-safe as used by DBI's generic ANSI
# unquoter. Accept both SQL-quoted and plain dotted qualified names. Quoted
# dots belong to a single component and ]] represents a literal ].
.dbi_unquote_one <- function(text) {
  if (!is.character(text) || length(text)!=1L || is.na(text))
    stop("Identifier must be one non-missing string.",call.=FALSE)
  if (!nzchar(text)) return(DBI::Id(""))
  chars <- strsplit(text, "", fixed=TRUE)[[1L]]
  n <- length(chars); pos <- 1L; parts <- character()
  repeat {
    if (chars[[pos]] == "[") {
      pos <- pos + 1L; buf <- character(); closed <- FALSE
      while (pos <= n) {
        ch <- chars[[pos]]
        if (ch == "]") {
          if (pos < n && chars[[pos+1L]] == "]") {
            buf <- c(buf, "]"); pos <- pos + 2L
          } else {pos <- pos + 1L; closed <- TRUE; break}
        } else {buf <- c(buf,ch); pos <- pos + 1L}
      }
      if (!closed) stop("Unterminated bracket-quoted SQL identifier.",call.=FALSE)
      part <- paste0(buf,collapse="")
    } else {
      begin <- pos
      while (pos <= n && chars[[pos]] != ".") pos <- pos + 1L
      part <- paste0(chars[seq.int(begin,pos-1L)],collapse="")
      if (!nzchar(part)) stop("Invalid SQL identifier component.",call.=FALSE)
    }
    parts <- c(parts,part)
    if (pos > n) break
    if (chars[[pos]] != ".") stop("Unexpected character after quoted SQL identifier.",call.=FALSE)
    pos <- pos+1L
    if (pos > n) stop("Trailing dot in SQL identifier.",call.=FALSE)
  }
  do.call(DBI::Id,unname(as.list(parts)))
}
for (.utype in c("Id","SQL","character")) {
  methods::setMethod("dbUnquoteIdentifier",
    signature(conn="SqlClientNETConnection",x=.utype),
    function(conn,x,...) {
      if (inherits(x,"Id")) return(list(x))
      if (!is.character(x) || anyNA(x)) stop("Invalid SQL identifier.",call.=FALSE)
      ans <- lapply(as.character(x),.dbi_unquote_one)
      names(ans) <- names(x)
      ans
    })
}
rm(.utype)

# Reject unsupported input rather than silently converting numeric/logical objects.
methods::setMethod("dbQuoteString",signature(conn="SqlClientNETConnection",x="ANY"),
  function(conn,x,...) .dbi_quote_string(conn,x))

# ANSI X'HH' binary literal syntax is not accepted by T-SQL; emit 0xHH.
.dbi_raw_literal <- function(x) {
  if (is.null(x)) return("NULL")
  if (!is.raw(x)) stop("Binary SQL literals require raw vectors or NULL.",call.=FALSE)
  paste0("0x", paste(sprintf("%02X", as.integer(x)),collapse=""))
}
# The generic DBI POSIXct/POSIXlt literal may include a time-zone suffix
# (e.g. " UTC") that SQL Server's datetime2 does not accept (SQL error 241).
# Match our bound datetime2 parameters exactly: ISO 8601 local wall-clock
# text with a T separator, explicit CAST, and SQL NULL for missing values.
# Note: SQL datetime2 is timezone-less; the caller chooses the intended zone
# before binding or quoting. These values must not be silently shifted to UTC.
.dbi_quote_timestamp <- function(x) {
  if (inherits(x, "POSIXlt")) x <- as.POSIXct(x)
  if (!inherits(x, "POSIXct"))
    stop("Expected a POSIXct or POSIXlt timestamp.", call. = FALSE)
  text <- vapply(seq_along(x), function(i) {
    stamp <- x[i]
    if (is.na(stamp)) return("NULL")
    paste0("CAST('", format(stamp, "%Y-%m-%dT%H:%M:%OS6"),
           "' AS datetime2(7))")
  }, character(1))
  DBI::SQL(text, names = names(x))
}
for (.timestamp_type in c("POSIXct", "POSIXlt")) {
  methods::setMethod("dbQuoteLiteral",
    signature(conn="SqlClientNETConnection", x=.timestamp_type),
    function(conn, x, ...) .dbi_quote_timestamp(x))
}
rm(.timestamp_type)

methods::setMethod("dbQuoteLiteral",
  signature(conn="SqlClientNETConnection",x="raw"),
  function(conn,x,...) DBI::SQL(.dbi_raw_literal(x)))

# blob::blob() is an S3 vctrs list-of.  Registering a standalone S4
# `blob` old class does NOT reliably dispatch on the concrete blob/vctrs S3
# class hierarchy (and can yield DBI's ANSI X'HH' fallback).  Instead place
# the blob guard in this connection-specific ANY method.  All our existing
# specialised raw/list/POSIXct methods remain preferred, and all remaining
# types delegate to the DBI default with methods::callNextMethod().
.dbi_blob_literals <- function(x) {
  if (!inherits(x, "blob"))
    stop("Expected a blob vector.", call.=FALSE)
  values <- vapply(seq_along(x), function(i) .dbi_raw_literal(x[[i]]),
                   character(1))
  DBI::SQL(values, names=names(x))
}
methods::setMethod("dbQuoteLiteral",
  signature(conn="SqlClientNETConnection",x="ANY"),
  function(conn,x,...) {
    if (inherits(x, "blob")) return(.dbi_blob_literals(x))
    methods::callNextMethod()
  })

methods::setMethod("dbQuoteLiteral",
  signature(conn="SqlClientNETConnection",x="list"),
  function(conn,x,...) {
    if (inherits(x, "blob")) return(.dbi_blob_literals(x))
    if (!all(vapply(x,function(a) is.null(a) || is.raw(a),logical(1))))
      stop("List SQL literals must contain raw vectors or NULL.",call.=FALSE)
    DBI::SQL(vapply(x,.dbi_raw_literal,character(1)),names=names(x))
  })

methods::setMethod("dbListTables", "SqlClientNETConnection", function(conn, ...) {
  # Views are valid remote tables in dbplyr as well as in SQL Server.
  x <- DBI::dbGetQuery(conn, paste0(
    "SELECT TABLE_SCHEMA, TABLE_NAME FROM INFORMATION_SCHEMA.TABLES ",
    "WHERE TABLE_TYPE IN ('BASE TABLE', 'VIEW') ",
    "ORDER BY TABLE_SCHEMA, TABLE_NAME"))
  ordinary <- if (nrow(x)) ifelse(x$TABLE_SCHEMA=="dbo", x$TABLE_NAME,
         paste0(x$TABLE_SCHEMA,".",x$TABLE_NAME)) else character()
  unique(c(ordinary, .dbi_handle(conn)$dbi_temp_names %||% character()))
})
 .dbi_exists_table <- function(conn,name) {
  name <- .dbi_temp_resolve(conn,name)
  # OBJECT_ID accepts multipart identifiers when supplied as a parameter.
  # Handle #local-temp specially: SQL Server stores them in tempdb.
  if (is.character(name) && length(name)==1L && grepl("^#",name)) {
    name<-paste0("tempdb..",name)
  } else if (inherits(name, "Id") && length(methods::slot(name,"name"))==1L &&
             startsWith(unname(methods::slot(name,"name")[[1L]]),"#")) {
    name<-paste0("tempdb..",unname(methods::slot(name,"name")[[1L]]))
  } else if (inherits(name, "SQL") && length(name)==1L &&
             grepl("^(\\[#[A-Za-z0-9_]+\\]|#[A-Za-z0-9_]+)$",as.character(name))) {
    name<-as.character(name)
    if (startsWith(name,"[")) name<-substr(name,2L,nchar(name)-1L)
    name<-paste0("tempdb..",name)
  } else if (inherits(name, "Id") || inherits(name, "SQL")) {
    name <- as.character(DBI::dbQuoteIdentifier(conn, name))
  } else if (!is.character(name) || length(name)!=1L || is.na(name)) {
    stop("name must identify one table.", call.=FALSE)
  }
  ans <- DBI::dbGetQuery(conn, paste0(
    "SELECT CASE WHEN OBJECT_ID(@name, 'U') IS NOT NULL ",
    "OR OBJECT_ID(@name, 'V') IS NOT NULL THEN 1 ELSE 0 END AS exists_flag"),
    params=list(name=as.character(name)))
  isTRUE(as.integer(ans$exists_flag[[1L]]) == 1L)
}
for (.name_type in c("Id","character","SQL")) {
  methods::setMethod("dbExistsTable", signature(conn="SqlClientNETConnection",name=.name_type),
    function(conn,name,...) .dbi_exists_table(conn,name))
}
rm(.name_type)
# DBI data-type inference accepts both vectors and data frames.  This only
# infers SQL types; actual supported writes are checked by dbWriteTable().
.dbi_sql_type <- function(obj) {
  if (is.data.frame(obj))
    return(vapply(obj,.dbi_sql_type,character(1)))
  if (inherits(obj,"Date")) return("DATE")
  if (inherits(obj,"POSIXt")) return("DATETIME2(7)")
  if (inherits(obj,"difftime")) return("TIME(7)")
  if (inherits(obj,"integer64")) return("BIGINT")
  if (is.factor(obj)) return("NVARCHAR(MAX)")
  if (is.integer(obj)) return("INT")
  if (is.logical(obj)) return("BIT")
  if (is.numeric(obj)) return("FLOAT")
  if (is.character(obj)) return("NVARCHAR(MAX)")
  if (is.raw(obj) || inherits(obj,"blob")) return("VARBINARY(MAX)")
  if (is.list(obj)) {
    if (all(vapply(obj,function(x) is.null(x) || is.raw(x),logical(1))))
      return("VARBINARY(MAX)")
    # DBI expects a type name for list objects.  Non-binary lists require
    # explicit conversion by the caller before a real table upload.
    return("NVARCHAR(MAX)")
  }
  stop("Unsupported R column type for SQL Server.",call.=FALSE)
}
methods::setMethod("dbDataType","SqlClientNETConnection",
  function(dbObj,obj,...) .dbi_sql_type(obj))
methods::setMethod("dbDataType","SqlClientNETDriver",
  function(dbObj,obj,...) .dbi_sql_type(obj))

.dbi_list_fields <- function(conn,name) {
  quoted<-as.character(DBI::dbQuoteIdentifier(conn,.dbi_temp_resolve(conn,name)))
  res<-DBI::dbSendQuery(conn,paste0("SELECT TOP (0) * FROM ",quoted))
  on.exit(DBI::dbClearResult(res),add=TRUE)
  DBI::dbColumnInfo(res)$name
}
.dbi_read_table <- function(conn,name,row.names=FALSE,check.names=FALSE,
                            query_options=list()) {
  if (!(is.null(row.names) || (is.logical(row.names) && length(row.names)==1L) ||
        (is.character(row.names) && length(row.names)==1L && !is.na(row.names))))
    stop("row.names must be NULL, TRUE, FALSE, NA or a column name.",call.=FALSE)
  if (!is.logical(check.names) || length(check.names)!=1L || is.na(check.names))
    stop("check.names must be TRUE or FALSE.",call.=FALSE)
  quoted <- as.character(DBI::dbQuoteIdentifier(conn,.dbi_temp_resolve(conn,name)))
  if (length(quoted)!=1L || is.na(quoted)) stop("Invalid table name.",call.=FALSE)
  out <- do.call(DBI::dbGetQuery,
    c(list(conn=conn, statement=paste0("SELECT * FROM ",quoted)), query_options))
  use_col <- NULL
  if (isTRUE(row.names)) use_col <- "row_names"
  else if (is.logical(row.names) && length(row.names)==1L && is.na(row.names)) {
    if ("row_names" %in% names(out)) use_col <- "row_names"
  } else if (is.character(row.names)) use_col <- row.names
  if (!is.null(use_col)) {
    if (!use_col %in% names(out)) stop("Specified row-name column does not exist.",call.=FALSE)
    rn <- out[[use_col]]
    if (anyNA(rn) || anyDuplicated(rn)) stop("Row names must be unique and non-missing.",call.=FALSE)
    out[[use_col]] <- NULL
    rownames(out) <- as.character(rn)
  }
  if (isTRUE(check.names)) names(out) <- make.names(names(out),unique=TRUE)
  out
}
for (.name_type in c("Id","character","SQL")) {
  methods::setMethod("dbListFields",signature(conn="SqlClientNETConnection",name=.name_type),
    function(conn,name,...) .dbi_list_fields(conn,name))
  methods::setMethod("dbReadTable",signature(conn="SqlClientNETConnection",name=.name_type),
    function(conn,name,...,row.names=FALSE,check.names=FALSE)
      .dbi_read_table(conn,name,row.names=row.names,check.names=check.names,
                      query_options=list(...)))
}
rm(.name_type)

# DBI introspection useful to clients such as dbplyr and DBItest.
methods::setMethod("dbListResults", "SqlClientNETConnection", function(conn, ...) {
  h <- .dbi_connection(conn)
  if (!.dbi_active(h)) return(list())
  list(methods::new("SqlClientNETResult", state=h$dbi_result))
})
methods::setMethod("dbGetInfo", "SqlClientNETResult", function(dbObj, ...) {
  s <- .dbi_state(dbObj)
  list(statement=s$sql, row.count=s$row_count, rows.affected=s$affected,
       completed=isTRUE(s$complete), has.completed=isTRUE(s$complete),
       is.valid=DBI::dbIsValid(dbObj))
})
