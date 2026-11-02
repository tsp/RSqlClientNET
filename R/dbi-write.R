# DBI writing for SQL Server. Keep uploads on the same SqlConnection as the
# query cursor: #local temporary tables are not visible on a second session.
# Upload batches use Rcpp SBIN binary columns over the existing native pipe,
# decoded by an IDataReader and written via SqlBulkCopy without data files.

.dbi_bulk_conn <- function(conn) {
  h <- .dbi_connection(conn)
  .dbi_lock_check(h)
  if (isTRUE(h$stream_active)) stop("Cannot write during active streaming.", call.=FALSE)
  h
}
.dbi_name_key <- function(name) {
  if (inherits(name, "Id")) {
    parts <- methods::slot(name,"name")
    if (length(parts)==1L) return(unname(parts[[1L]]))
    return(NULL)
  }
  if (inherits(name,"SQL")) return(NULL)
  if (is.character(name) && length(name)==1L && !is.na(name)) return(name)
  NULL
}
.dbi_temp_resolve <- function(conn,name) {
  key <- .dbi_name_key(name)
  h <- .dbi_handle(conn)
  # DBI's inherited Arrow helpers first quote the logical table name, then
  # forward SQL("[name]") through dbCreateTableArrow() -> dbAppendTableArrow().
  # When temporary=TRUE created a local #name, lookup must also work for that
  # *same quoted logical identifier*.  Never rewrite arbitrary SQL, quoted
  # multi-part names, or an identifier without a registered temp alias.
  if (is.null(key) && inherits(name, "SQL") && length(name)==1L && !is.na(name)) {
    literal <- as.character(name)
    if (grepl("^\\[[A-Za-z_][A-Za-z0-9_]*\\]$", literal))
      key <- substr(literal, 2L, nchar(literal)-1L)
  }
  # Empty identifiers can be quoted (as []) but cannot be used as env keys.
  if (!is.null(key) && length(key)==1L && nzchar(key) &&
      !is.null(h$dbi_temp_aliases) &&
      exists(key,envir=h$dbi_temp_aliases,inherits=FALSE))
    return(get(key,envir=h$dbi_temp_aliases,inherits=FALSE))
  name
}
.dbi_temp_target <- function(conn,name,temporary=FALSE) {
  if (!isTRUE(temporary)) return(.dbi_temp_resolve(conn,name))
  # dbplyr's MSSQL dialect may have already prefixed #.  DBI's generic
  # Arrow table methods, by contrast, forward a safely quoted SQL identifier
  # such as SQL("[dbit08]") while keeping temporary=TRUE.  In that case,
  # convert ONLY a single simple quoted identifier to a local #table. Never
  # reinterpret arbitrary SQL as an identifier.
  if (inherits(name,"SQL")) {
    literal <- as.character(name)
    if (length(literal)!=1L || is.na(literal))
      stop("temporary=TRUE requires one SQL identifier.",call.=FALSE)
    if (grepl("^(\\[#[A-Za-z0-9_]+\\]|#[A-Za-z0-9_]+)$", literal))
      return(name)
    if (grepl("^\\[[A-Za-z_][A-Za-z0-9_]*\\]$", literal)) {
      key <- substr(literal, 2L, nchar(literal)-1L)
      if (nchar(key, type="chars") > 115L)
        stop("SQL Server local temporary table names may not exceed 116 characters including #.",call.=FALSE)
      return(paste0("#", key))
    }
    stop("temporary=TRUE with DBI::SQL requires one safely quoted table identifier.",call.=FALSE)
  }
  key <- .dbi_name_key(name)
  if (is.null(key) || length(key)!=1L || !nzchar(key) || startsWith(key,"##"))
    stop("temporary=TRUE requires an unqualified, nonempty table name (not ##global).",call.=FALSE)
  if (nchar(key,type="chars")>115L)
    stop("SQL Server local temporary table names may not exceed 116 characters including #.",call.=FALSE)
  if (startsWith(key,"#")) return(key)
  paste0("#",key)
}
# dbplyr's MSSQL dialect rewrites temporary names to #name *before* calling
# DBI::dbWriteTable(), then passes DBI::SQL("[#name]") with temporary=FALSE.
# Recognise such already-quoted local names so the original unprefixed name
# remains addressable through our per-connection DBI alias table.
# Only accept simple local temp names here; never reinterpret arbitrary SQL.
.dbi_local_temp_name <- function(name) {
  if (!is.character(name) || length(name) != 1L || is.na(name)) return(NULL)
  literal <- as.character(name)
  if (grepl("^\\[#[A-Za-z0-9_]+\\]$", literal))
    return(substr(literal, 2L, nchar(literal) - 1L))
  if (grepl("^#[A-Za-z0-9_]+$", literal)) return(literal)
  NULL
}
.dbi_register_temp <- function(conn,name,target,temporary) {
  key <- .dbi_name_key(name)
  h <- .dbi_handle(conn)
  physical <- .dbi_local_temp_name(target)
  if (!is.null(physical))
    h$dbi_temp_names <- unique(c(h$dbi_temp_names,physical))
  if (isTRUE(temporary) && !is.null(key) && !startsWith(key,"#")) {
    assign(key,target,envir=h$dbi_temp_aliases)
    h$dbi_temp_names <- unique(c(h$dbi_temp_names,key))
  } else if (is.null(key) && inherits(name,"SQL") && !is.null(physical)) {
    # dbplyr created #name and passed it as already-quoted SQL.
    # Register the unprefixed alias, just as dbWriteTable(temporary=TRUE)
    # would have done if dbplyr had not rewritten the name itself.
    logical <- substring(physical, 2L)
    if (nzchar(logical)) {
      assign(logical,target,envir=h$dbi_temp_aliases)
      h$dbi_temp_names <- unique(c(h$dbi_temp_names,logical))
    }
  }
  invisible(NULL)
}
.dbi_forget_temp <- function(conn,name) {
  h <- .dbi_handle(conn)
  key <- .dbi_name_key(name)
  physical <- .dbi_local_temp_name(.dbi_temp_resolve(conn,name))
  forget <- if (is.null(key)) character() else key
  if (!is.null(physical)) {
    forget <- unique(c(forget, physical))
    # Removing through either #name or logical name must clear all aliases
    # that reference the deleted physical temporary table.
    for (alias in ls(h$dbi_temp_aliases, all.names=TRUE)) {
      dest <- get(alias,envir=h$dbi_temp_aliases,inherits=FALSE)
      if (identical(.dbi_local_temp_name(dest),physical))
        forget <- c(forget,alias)
    }
  }
  for (alias in intersect(unique(forget),ls(h$dbi_temp_aliases,all.names=TRUE)))
    rm(list=alias,envir=h$dbi_temp_aliases)
  h$dbi_temp_names <- setdiff(h$dbi_temp_names,unique(forget))
  invisible(NULL)
}
.dbi_bulk_validate_frame <- function(value) {
  if (!is.data.frame(value)) stop("value must be a data.frame.",call.=FALSE)
  if (!ncol(value) || is.null(names(value)) || anyNA(names(value)) ||
      any(!nzchar(names(value))) || anyDuplicated(tolower(names(value))))
    stop("The data.frame must have unique nonempty column names.",call.=FALSE)
  invisible(TRUE)
}
.dbi_bulk_field_types <- function(conn,value,field.types=NULL) {
  nm<-names(value)
  types<-vapply(value,function(x) {
    if (is.factor(x)) return("NVARCHAR(MAX)")
    if (inherits(x,"difftime")) return("TIME(7)")
    if (is.list(x) && all(vapply(x,function(a) is.null(a) || is.raw(a),logical(1))))
      return("VARBINARY(MAX)")
    as.character(DBI::dbDataType(conn,x))
  },character(1))
  if (!is.null(field.types)) {
    if (!(is.character(field.types) || is.list(field.types)) ||
        is.null(names(field.types)) || anyNA(names(field.types)) ||
        any(!nzchar(names(field.types))) || anyDuplicated(names(field.types)) ||
        !all(names(field.types) %in% nm))
      stop("field.types must be a named subset of the columns.",call.=FALSE)
    for (k in names(field.types)) types[[k]]<-as.character(field.types[[k]])
  }
  names(types)<-nm
  # No semicolon, quotes, or SQL fragments in type declarations. SQL type names
  # remain user-selected, but this blocks multi-statement DDL injection.
  if (any(!grepl("^[A-Za-z][A-Za-z0-9_]*(\\s*\\(\\s*(MAX|[0-9]+)(\\s*,\\s*[0-9]+)?\\s*\\))?$",types,ignore.case=TRUE)))
    stop("Invalid SQL field type declaration (expected e.g. DECIMAL(38,10)).",call.=FALSE)
  types
}
.dbi_bulk_kind <- function(x,sql_type) {
  st<-toupper(trimws(sql_type))
  if (inherits(x,"integer64")) return("integer64")
  if (inherits(x,"POSIXct")) return(if (grepl("^DATETIMEOFFSET",st)) "datetimeoffset" else "datetime")
  if (inherits(x,"Date")) return("date")
  if (inherits(x,"difftime")) return("time")
  if (is.factor(x)) return(if (grepl("^(DECIMAL|NUMERIC|MONEY|SMALLMONEY)",st)) "decimal" else "text")
  if (is.list(x) && all(vapply(x,function(a) is.null(a) || is.raw(a),logical(1)))) return("binary")
  if (is.integer(x)) return("integer")
  if (is.logical(x)) return("logical")
  if (is.numeric(x)) return(if (grepl("^(DECIMAL|NUMERIC|MONEY|SMALLMONEY)",st)) "decimal" else "double")
  if (is.character(x)) return(if (grepl("^(DECIMAL|NUMERIC|MONEY|SMALLMONEY)",st)) "decimal" else "text")
  stop("Unsupported bulk column type: ",paste(class(x),collapse="/"),call.=FALSE)
}
# Temporal and decimal types retain canonical text formatting when needed by
# the SBIN encoder; numeric, logical, integer64 and POSIXct use native buffers.
.dbi_bulk_atomic_values <- function(x, kind) {
  if (kind == "integer64") return(as.character(x))
  if (kind == "date") return(format(x,"%Y-%m-%d"))
  if (kind %in% c("datetime","datetimeoffset")) {
    y <- format(x,format="%Y-%m-%dT%H:%M:%OS6",tz="UTC",usetz=FALSE)
    if (kind == "datetimeoffset") y[!is.na(y)] <- paste0(y[!is.na(y)],"+00:00")
    return(y)
  }
  if (kind == "time") {
    sec <- as.numeric(x,units="secs")
    if (any(!is.na(sec) & (!is.finite(sec) | sec < 0 | sec >= 86400)))
      stop("SQL TIME requires values from 00:00:00 to less than 24 hours.",call.=FALSE)
    y <- rep(NA_character_,length(sec))
    good <- which(!is.na(sec))
    if (length(good)) {
      hh <- floor(sec[good]/3600)
      mm <- floor((sec[good] %% 3600)/60)
      ss <- sec[good] %% 60
      y[good] <- sprintf("%02d:%02d:%010.7f",hh,mm,ss)
    }
    return(y)
  }
  if (kind == "decimal") {
    if (is.numeric(x)) {
      # format.numeric() turns NA_real_ into the literal string "NA".
      # The SBIN encoder distinguishes SQL NULL by an actual NA_character_
      # in its bitmap, not by parsing sentinel text in the .NET helper.
      # NA_real_ is SQL NULL; NaN is not. is.na() is TRUE for both,
      # so reject NaN (and +/-Inf) explicitly *before* formatting/encoding.
      # Only ordinary NA values are represented by the SBIN NULL bitmap.
      if (any(is.nan(x) | is.infinite(x)))
        stop("NaN/Inf cannot be sent to SQL Server DECIMAL or MONEY; convert to NA first.",
             call. = FALSE)
      missing <- is.na(x)
      y <- format(x,scientific=FALSE,trim=TRUE,digits=17)
      y[missing] <- NA_character_
      return(y)
    }
    return(as.character(x))
  }
  if (kind == "double") {
    if (any(is.nan(x) | is.infinite(x)))
      stop("NaN/Inf cannot be sent to SQL Server FLOAT; convert to NA first.",call.=FALSE)
    return(as.numeric(x))
  }
  if (kind == "text") return(enc2utf8(as.character(x)))
  if (kind == "integer") return(as.integer(x))
  if (kind == "logical") return(as.logical(x))
  stop("Unknown bulk kind.",call.=FALSE)
}
# SBIN native encoder: DATE (days since 1970), TIME (seconds since midnight),
# and numeric DECIMAL/MONEY (IEEE doubles) now use fixed-width native columns.
# Exact character/factor DECIMAL remains UTF-8 to preserve all 38 digits.
.dbi_bulk_binary_payload <- function(value, kinds, first, last) {
  stopifnot(length(kinds)==ncol(value),first>=1L,last>=first,last<=nrow(value))
  profile_stages <- isTRUE(getOption("RSqlClientNET.bulk_profile_stages", FALSE))
  tic_pre <- if (profile_stages) proc.time()[["elapsed"]] else NULL
  # SBIN v1 extension: POSIXct is a packed IEEE-754 Unix-seconds vector.
  # Only the native encoder sees these names; bulk_begin keeps the original
  # SQL schema kinds (datetime / datetimeoffset) for destination metadata.
  encode_kinds <- vapply(seq_along(value), function(i) {
    kind <- kinds[[i]]
    if (kind == "datetime") return("datetime_native")
    if (kind == "datetimeoffset") return("datetimeoffset_native")
    if (kind == "date") return("date_native")
    if (kind == "time") return("time_native")
    # Numeric double has at most 17 significant decimal digits. Retain the
    # lossless UTF-8 route for character/factor input (DECIMAL(38,s)).
    if (kind == "decimal" && is.numeric(value[[i]]) &&
        !is.factor(value[[i]])) return("decimal_native")
    kind
  }, character(1))
  columns <- lapply(seq_along(value),function(i) {
    kind <- kinds[[i]]
    x <- value[[i]][first:last]
    if (kind %in% c("datetime", "datetimeoffset")) {
      if (!inherits(x, "POSIXct")) stop("Expected POSIXct column for native datetime.",call.=FALSE)
      return(as.numeric(x)) # POSIXct seconds; never format timestamps as strings.
    }
    if (kind == "date") {
      days <- as.numeric(x) # Date's native representation is Unix epoch days
      if (any(!is.na(days) & (!is.finite(days) | days != trunc(days))))
        stop("SQL DATE requires finite whole calendar days.", call.=FALSE)
      return(days)
    }
    if (kind == "time") {
      sec <- as.numeric(x, units="secs")
      if (any(!is.na(sec) & (!is.finite(sec) | sec < 0 | sec >= 86400)))
        stop("SQL TIME requires values from 00:00:00 to less than 24 hours.",call.=FALSE)
      return(sec)
    }
    if (kind == "decimal" && encode_kinds[[i]] == "decimal_native") {
      if (any(is.nan(x) | is.infinite(x)))
        stop("NaN/Inf cannot be sent to SQL Server DECIMAL or MONEY; convert to NA first.",call.=FALSE)
      return(as.numeric(x))
    }
    if (kind == "integer64") return(x) # preserve bit64's exact 64-bit bits
    if (kind %in% c("integer","double","logical","binary")) {
      if (kind == "double" && any(is.nan(x) | is.infinite(x)))
        stop("NaN/Inf cannot be sent to SQL Server FLOAT; convert to NA first.",call.=FALSE)
      return(x)
    }
    .dbi_bulk_atomic_values(x,kind)
  })
  if (profile_stages) pre_seconds <- proc.time()[["elapsed"]] - tic_pre
  tic_pack <- if (profile_stages) proc.time()[["elapsed"]] else NULL
  payload <- .sql_native_bulk_encode(columns,as.character(encode_kinds))
  if (profile_stages) message(sprintf(
    "RSqlClientNET binary encode detail: R column prep %.3fs, Rcpp packing %.3fs",
    pre_seconds, proc.time()[["elapsed"]] - tic_pack))
  payload
}

.dbi_bulk_ddl <- function(conn,target,types) {
  qtable<-as.character(DBI::dbQuoteIdentifier(conn,target))
  qcol<-as.character(DBI::dbQuoteIdentifier(conn,names(types)))
  paste0("CREATE TABLE ",qtable," (",
         paste(paste(qcol,unname(types),"NULL"),collapse=", "),")")
}
# Keep SBIN payloads within the native 128 MiB frame limit.  Row-count
# limits alone are insufficient for long NVARCHAR, Unicode and VARBINARY
# columns.  Only the encoder's explicit size error is recoverable: no bytes
# have been sent to the helper, so a smaller slice is safe to retry.  Reuse
# the reduced row limit for subsequent batches to avoid repeated failures.
.dbi_bulk_encode_fitting <- function(value, kinds, first, batch_rows,
                                     encode = .dbi_bulk_binary_payload) {
  rows <- as.integer(min(as.double(batch_rows),
                         as.double(nrow(value)) - as.double(first) + 1))
  if (length(rows) != 1L || is.na(rows) || rows < 1L)
    stop("Invalid binary bulk batch bounds.", call. = FALSE)
  repeat {
    last <- as.integer(as.double(first) + rows - 1)
    payload <- tryCatch(encode(value, kinds, first, last), error = function(e) {
      if (!identical(conditionMessage(e),
                     "Binary bulk batch exceeds 128 MiB; decrease batch_rows."))
        stop(e)
      e
    })
    if (!inherits(payload, "error"))
      return(list(payload = payload, last = last, rows = rows))
    if (rows == 1L)
      stop("One binary bulk row exceeds the 128 MiB frame limit; reduce the size of the individual value.",
           call. = FALSE)
    rows <- max(1L, rows %/% 2L)
  }
}

# SqlBulkCopyOptions.TableLock may allow minimally logged writes into eligible
# heaps under SIMPLE/BULK_LOGGED recovery, but can block concurrent writers.
# Enabled by default for throughput; callers can opt out on each write.
.dbi_validate_table_lock <- function(x) {
  if (!is.logical(x) || length(x) != 1L || is.na(x))
    stop("table_lock must be TRUE or FALSE.", call. = FALSE)
  x
}

# Independent bulk limit: SQL result downloads still max out at 32768 rows.
.dbi_max_bulk_batch <- function(n) {
  if (length(n) != 1L || !is.numeric(n) || is.na(n) ||
      !is.finite(n) || n != floor(n) || n < 1 || n > 131072L)
    stop("batch_rows for binary bulk upload must be a whole number between 1 and 131072.",call.=FALSE)
  as.integer(n)
}

# Benchmarked upload default; 131072 rows remain available on request.
.dbi_default_bulk_batch <- function() 65536L

.dbi_bulk_upload <- function(conn,target,value,types,create=FALSE,drop=FALSE,
                             batch_rows=NULL,timeout=600,table_lock=TRUE) {
  table_lock <- .dbi_validate_table_lock(table_lock)
  .dbi_bulk_validate_frame(value)
  h<-.dbi_bulk_conn(conn)
  if (!identical(h$transport,"rcpp"))
    stop("Binary bulk upload requires the native Rcpp connection.",call.=FALSE)
  if (is.null(batch_rows)) batch_rows <- .dbi_default_bulk_batch()
  batch_rows <- .dbi_max_bulk_batch(batch_rows)
  timeout<-.dbi_validate_timeout(timeout)
  if (length(types)!=ncol(value) || !identical(names(types),names(value)))
    stop("Upload types do not match value columns.",call.=FALSE)
  kinds<-unname(mapply(.dbi_bulk_kind,value,types,USE.NAMES=FALSE))
  qtable<-as.character(DBI::dbQuoteIdentifier(conn,target))
  .sql_rpc(h,list(op="bulk_begin",destination=qtable,
      # jsonlite::toJSON(auto_unbox=TRUE) otherwise turns a one-column
      # schema into three JSON strings; .NET BulkBegin expects JSON arrays.
      columns=unname(as.list(names(value))),
      kinds=unname(as.list(kinds)),sqlTypes=unname(as.list(types)),
      batchRows=batch_rows,timeout=as.integer(ceiling(timeout)),
      tableLock=table_lock,
      dropSql=if(drop) paste0("DROP TABLE ",qtable) else "",
      createSql=if(create) .dbi_bulk_ddl(conn,target,types) else ""), timeout=timeout)
  finished<-FALSE
  on.exit(if(!finished && sql_is_valid(h)) {
      try(.sql_rpc(h,list(op="bulk_abort"),timeout=min(timeout,30)),silent=TRUE)
    },add=TRUE)
  n<-nrow(value)
  profile <- isTRUE(getOption("RSqlClientNET.bulk_profile", FALSE))
  alloc_profile <- isTRUE(getOption("RSqlClientNET.bulk_alloc_profile", FALSE))
  first <- 1
  while (first <= n) {
    tic <- proc.time()[["elapsed"]]
    encoded <- .dbi_bulk_encode_fitting(value, kinds, first, batch_rows)
    payload <- encoded$payload
    last <- encoded$last
    if (encoded$rows < min(as.double(batch_rows),
                           as.double(n) - as.double(first) + 1)) {
      if (profile) message(sprintf(
        "RSqlClientNET binary bulk: reducing upload batch_rows from %d to %d to fit 128 MiB frame",
        batch_rows, encoded$rows))
      batch_rows <- encoded$rows
    }
    serial_seconds <- proc.time()[["elapsed"]] - tic
    reply <- .sql_rpc_binary(h,payload,last-first+1L,timeout=timeout,
                             allocation_profile=alloc_profile)
    payload_bytes <- length(payload)
    if (!identical(as.numeric(reply$rows),as.numeric(last-first+1L)))
      stop("Bulk upload row count mismatch.",call.=FALSE)
    if (profile) message(sprintf(
      paste0("RSqlClientNET binary bulk batch %d-%d: R encode %.3fs, ",
             "RPC + SQL %.3fs (helper prepare %.1fms, SqlBulkCopy %.1fms, %.1f MB); ",
             "transport: native packet %.1fms, native pipe send %.1fms, ",
             "helper receive %.1fms"),
      first,last,serial_seconds,proc.time()[["elapsed"]]-tic-serial_seconds,
      as.numeric(reply$prepareMs %||% NA_real_),
      as.numeric(reply$copyMs %||% NA_real_),
      payload_bytes/1e6,
      as.numeric(reply$nativePacketMs %||% NA_real_),
      as.numeric(reply$nativePipeSendMs %||% NA_real_),
      as.numeric(reply$receiveMs %||% NA_real_)))
    if (alloc_profile && !is.null(reply$allocations)) {
      a <- reply$allocations
      c <- a$readerCounters
      message(sprintf(paste0(
        "RSqlClientNET allocation bulk: rows=%d, receive=%.3f MB, ",
        "reader_init=%.3f MB, bulkcopy_thread=%.3f MB, ",
        "process_total=%.3f MB, GC=(%d,%d,%d), ",
        "GetValue=%d, GetChars=%d, GetBytes=%d, ",
        "text_objects=%d, text_UTF16=%.3f MB, ",
        "blob_objects=%d, blob_copy=%.3f MB, binary_stream=%.3f MB"),
        encoded$rows, a$receiveThreadBytes / 1048576,
        a$prepareThreadBytes / 1048576, a$copyThreadBytes / 1048576,
        a$processBytes / 1048576,
        a$gen0, a$gen1, a$gen2,
        c$getValueCalls, c$getCharsCalls, c$getBytesCalls,
        c$textObjects, c$textUtf16Bytes / 1048576,
        c$blobObjects, c$blobBytes / 1048576,
        c$streamBinaryBytes / 1048576))
    }
    first <- as.double(last) + 1
  }
  reply<-.sql_rpc(h,list(op="bulk_finish"),timeout=timeout)
  finished<-TRUE
  if (!identical(as.numeric(reply$rows),as.numeric(n)))
    stop("Bulk upload final row count mismatch.",call.=FALSE)
  as.numeric(reply$rows)
}
.dbi_write_table <- function(conn,name,value,overwrite=FALSE,append=FALSE,
                             row.names=FALSE,field.types=NULL,temporary=FALSE,
                             batch_rows=NULL,timeout=600,table_lock=TRUE) {
  table_lock <- .dbi_validate_table_lock(table_lock)
  h<-.dbi_bulk_conn(conn)
  if (!(is.null(row.names) ||
        (is.logical(row.names) && length(row.names)==1L) ||
        (is.character(row.names) && length(row.names)==1L && !is.na(row.names))))
    stop("row.names must be NULL, TRUE, FALSE, NA or a column name.",call.=FALSE)
  for (flag in list(overwrite,append,temporary))
    if (!is.logical(flag) || length(flag)!=1L || is.na(flag)) stop("Invalid logical flag.",call.=FALSE)
  if (overwrite && append) stop("overwrite and append cannot both be TRUE.",call.=FALSE)
  if (append && !is.null(field.types))
    stop("field.types cannot be supplied when append=TRUE.",call.=FALSE)
  .dbi_bulk_validate_frame(value)
  # DBI row.names=TRUE stores an explicit 'row_names' field, while NA
  # preserves only non-automatic names; character supplies the field name.
  row_column <- NULL
  if (isTRUE(row.names)) row_column <- "row_names"
  else if (is.logical(row.names) && length(row.names)==1L && is.na(row.names)) {
    if (!identical(rownames(value),as.character(seq_len(nrow(value)))))
      row_column <- "row_names"
  } else if (is.character(row.names)) row_column <- row.names
  if (!is.null(row_column)) {
    if (!nzchar(row_column) || row_column %in% names(value))
      stop("Invalid or duplicate row-name column.",call.=FALSE)
    value <- c(stats::setNames(list(as.character(rownames(value))),row_column),as.list(value))
    value <- as.data.frame(value,optional=TRUE,stringsAsFactors=FALSE)
  }
  target<-.dbi_temp_target(conn,name,temporary)
  exists_already<-DBI::dbExistsTable(conn,target)
  if (exists_already && !(append || overwrite)) stop("Table already exists.",call.=FALSE)
  types<-.dbi_bulk_field_types(conn,value,field.types)
  if (exists_already && append) {
    known<-DBI::dbListFields(conn,target)
    if (!all(names(value) %in% known)) stop("Append columns are not in destination table.",call.=FALSE)
  }
  # dbWriteTable(factor) is a normal character roundtrip in DBI. In contrast,
  # dbAppendTable() explicitly warns about the loss of factor metadata.
  .dbi_bulk_upload(conn,target,value,types,create=(!exists_already || overwrite),
                   drop=(exists_already && overwrite),batch_rows=batch_rows,
                   timeout=timeout,table_lock=table_lock)
  .dbi_register_temp(conn,name,target,temporary)
  invisible(TRUE)
}
.dbi_append_table <- function(conn,name,value,row.names=NULL,batch_rows=NULL,
                              timeout=600,table_lock=TRUE) {
  table_lock <- .dbi_validate_table_lock(table_lock)
  if (!is.null(row.names)) stop("row.names must be NULL for dbAppendTable().",call.=FALSE)
  .dbi_bulk_validate_frame(value)
  target<-.dbi_temp_resolve(conn,name)
  if (!DBI::dbExistsTable(conn,target)) stop("Table does not exist.",call.=FALSE)
  fields<-DBI::dbListFields(conn,target)
  if (!all(names(value) %in% fields)) stop("Append columns do not match existing table.",call.=FALSE)
  types<-.dbi_bulk_field_types(conn,value)
  if (any(vapply(value,is.factor,logical(1))))
    warning("Factor columns are converted to character during SQL upload.",call.=FALSE)
  .dbi_bulk_upload(conn,target,value,types,batch_rows=batch_rows,
                   timeout=timeout,table_lock=table_lock)
}
.dbi_create_table <- function(conn,name,fields,temporary=FALSE,row.names=NULL) {
  if (!is.null(row.names)) stop("row.names must be NULL.",call.=FALSE)
  if (!is.logical(temporary) || length(temporary)!=1L || is.na(temporary))
    stop("temporary must be TRUE or FALSE.",call.=FALSE)
  target<-.dbi_temp_target(conn,name,temporary)
  if (DBI::dbExistsTable(conn,target)) stop("Table already exists.",call.=FALSE)
  if (is.data.frame(fields)) {
    .dbi_bulk_validate_frame(fields)
    types<-.dbi_bulk_field_types(conn,fields)
  } else {
    if (!(is.list(fields) || is.character(fields)) || !length(fields) ||
        is.null(names(fields)) || any(!nzchar(names(fields))))
      stop("fields must be a data.frame or a named list of SQL types.",call.=FALSE)
    types<-unlist(fields,use.names=TRUE)
    types<-stats::setNames(as.character(types),names(types))
    dummy<-as.data.frame(stats::setNames(replicate(length(types),character(),simplify=FALSE), names(types)),
                         check.names=FALSE)
    types<-.dbi_bulk_field_types(conn,dummy,types)
  }
  DBI::dbExecute(conn,.dbi_bulk_ddl(conn,target,types))
  .dbi_register_temp(conn,name,target,temporary)
  invisible(TRUE)
}
.dbi_remove_table <- function(conn,name,fail_if_missing=TRUE,temporary=FALSE) {
  if (!is.logical(fail_if_missing) || length(fail_if_missing)!=1L || is.na(fail_if_missing))
    stop("fail_if_missing must be TRUE or FALSE.",call.=FALSE)
  if (!is.logical(temporary) || length(temporary)!=1L || is.na(temporary))
    stop("temporary must be TRUE or FALSE.",call.=FALSE)
  # If temporary=TRUE, address only SQL Server's session-local #table.
  # This must never fall back to deleting a permanent table of the same name.
  # The no-argument path retains RSqlClientNET's existing temp-alias behaviour.
  target <- if (temporary) .dbi_temp_target(conn,name,TRUE) else .dbi_temp_resolve(conn,name)
  exists_already<-DBI::dbExistsTable(conn,target)
  if (!exists_already) {
    if (isTRUE(fail_if_missing)) stop("Table does not exist.",call.=FALSE)
    return(invisible(FALSE))
  }
  DBI::dbExecute(conn,paste0("DROP TABLE ",as.character(DBI::dbQuoteIdentifier(conn,target))))
  # Forget using the physical name so all registered aliases are removed,
  # including when the caller named the logical table in temporary=TRUE mode.
  .dbi_forget_temp(conn,target)
  invisible(TRUE)
}
for (.nmtype in c("character","Id","SQL")) {
  methods::setMethod("dbWriteTable",signature(conn="SqlClientNETConnection",name=.nmtype),
    function(conn,name,value,...,overwrite=FALSE,append=FALSE,row.names=FALSE,
             field.types=NULL,temporary=FALSE,batch_rows=NULL,timeout=600,
             table_lock=TRUE) {
      if (length(list(...)))
        stop("Unsupported additional arguments to dbWriteTable().",call.=FALSE)
      .dbi_write_table(conn,name,value,overwrite,append,row.names,field.types,
                       temporary,batch_rows,timeout,table_lock)
    })
  # DBI::dbAppendTable(conn, name, value, ..., row.names=NULL) places
  # row.names AFTER ... and as the very last formal. S4 requires our method
  # to preserve that exact relative placement. Custom knobs are accepted via
  # ... and validated before forwarding them to the upload implementation.
  methods::setMethod("dbAppendTable",signature(conn="SqlClientNETConnection",name=.nmtype),
    function(conn,name,value,...,row.names=NULL) {
      opts <- list(...)
      allowed <- c("batch_rows", "timeout", "table_lock")
      if (length(opts) && (is.null(names(opts)) ||
          anyNA(names(opts)) || any(!nzchar(names(opts))) ||
          anyDuplicated(names(opts)) || !all(names(opts) %in% allowed))) {
        stop("dbAppendTable() accepts only named batch_rows, timeout and table_lock in ...",call.=FALSE)
      }
      .dbi_append_table(conn,name,value,row.names,
        batch_rows=opts$batch_rows,
        timeout=if (is.null(opts$timeout)) 600 else opts$timeout,
        table_lock=if ("table_lock" %in% names(opts)) opts$table_lock else TRUE)
    })
  # DBI::dbCreateTable(conn, name, fields, ..., row.names=NULL,
  #                    temporary=FALSE) requires this same trailing order.
  methods::setMethod("dbCreateTable",signature(conn="SqlClientNETConnection",name=.nmtype),
    function(conn,name,fields,...,row.names=NULL,temporary=FALSE) {
      .dbi_create_table(conn,name,fields,temporary,row.names)
    })
  methods::setMethod("dbRemoveTable",signature(conn="SqlClientNETConnection",name=.nmtype),
    function(conn,name,...,fail_if_missing=TRUE,temporary=FALSE) {
      if (length(list(...))) stop("Unsupported additional argument to dbRemoveTable().",call.=FALSE)
      .dbi_remove_table(conn,name,fail_if_missing,temporary)
    })
}
rm(.nmtype)
