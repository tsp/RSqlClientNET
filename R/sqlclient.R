.sql_bundled_helper <- function() {
  dll <- system.file("dotnet", "publish", "SqlClientBridge.dll",
                     package = "RSqlClientNET")
  if (nzchar(dll) && file.exists(dll)) dll else ""
}

#' Locate or build the .NET helper
#' @export
#' @noRd
#' @details Normal source-package installations compile the C# helper at
#'   installation time. The bundled published DLL is preferred and requires
#'   a .NET runtime version 8 or newer at use time (the helper uses
#'   RollForward=LatestMajor to prefer the latest installed major runtime).
#'   `force = TRUE` rebuilds a private copy in the user cache
#'   (requires the .NET SDK); development installations
#'   without a bundled helper also use that cache as a fallback.
sqlclient_build <- function(force = FALSE) {
  if (!is.logical(force) || length(force) != 1L || is.na(force))
    stop("force must be TRUE or FALSE.", call. = FALSE)

  # When a developer explicitly forces a rebuild, prefer that private
  # build for subsequent connections in this R session.
  override <- getOption("RSqlClientNET.helper_override", NULL)
  if (!force && is.character(override) && length(override) == 1L &&
      !is.na(override) && file.exists(override)) return(override)

  bundled <- .sql_bundled_helper()
  if (!force && nzchar(bundled)) return(bundled)

  dotnet <- unname(Sys.which("dotnet"))
  if (!nzchar(dotnet))
    stop(".NET SDK not found. A source installation normally builds the helper ",
         "automatically. For development, install .NET SDK 8 or later.",
         call. = FALSE)
  project <- system.file("dotnet", "SqlClientBridge.csproj",
                         package = "RSqlClientNET", mustWork = TRUE)
  cs_dir <- dirname(project)
  sources <- sort(list.files(cs_dir, pattern = "\\.cs$", full.names = TRUE))
  inputs <- c(project, sources)
  if (!length(sources)) stop("No .NET helper C# source files found.", call. = FALSE)
  hashes <- unname(tools::md5sum(inputs))
  if (anyNA(hashes)) stop("Cannot fingerprint .NET helper sources.", call. = FALSE)
  cache_key <- paste(substr(hashes, 1L, 12L), collapse = "-")
  root <- tools::R_user_dir("RSqlClientNET", which = "cache")
  dest <- file.path(root, paste0("net8-", cache_key))
  dll <- file.path(dest, "SqlClientBridge.dll")
  if (file.exists(dll) && !force) return(dll)

  # dotnet publish creates bin/obj beside its project file. Copy sources to a
  # writable cache location instead of trying to write into the R library.
  build_dir <- file.path(root, paste0("source-", cache_key))
  dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)
  copied <- file.copy(inputs, build_dir, overwrite = TRUE)
  if (!all(copied)) stop("Cannot stage .NET helper sources in the user cache.", call. = FALSE)
  dir.create(dest, recursive = TRUE, showWarnings = FALSE)
  # This fallback is used by pkgload/development installs. Keep .NET/MSBuild
  # scratch space out of R's session temp directory (R CMD check monitors it).
  # Passing "current" preserves PATH and all platform-specific environment
  # variables while overriding only the child process' build environment.
  scratch <- tempfile("dotnet-scratch-", tmpdir = root)
  dir.create(scratch, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(scratch, recursive = TRUE, force = TRUE), add = TRUE)
  dotnet_tmp <- file.path(scratch, "tmp")
  # Unlike MSBuild temp files, NuGet scratch must be shared between builds
  # using the same package cache to coordinate concurrent restores.
  nuget_tmp <- Sys.getenv("NUGET_SCRATCH", unset = "")
  if (!nzchar(nuget_tmp)) nuget_tmp <- file.path(root, "nuget-scratch")
  dir.create(dotnet_tmp, recursive = TRUE, showWarnings = FALSE)
  dir.create(nuget_tmp, recursive = TRUE, showWarnings = FALSE)
  message("Building .NET helper in the user cache. First build may restore NuGet packages...")
  processx::run(dotnet, c("publish", file.path(build_dir, basename(project)),
                          "-c", "Release", "-f", "net8.0", "--self-contained", "false",
                          "-o", dest),
                env = c("current", TMPDIR = dotnet_tmp, TMP = dotnet_tmp,
                        TEMP = dotnet_tmp, NUGET_SCRATCH = nuget_tmp,
                        DOTNET_CLI_TELEMETRY_OPTOUT = "1", DOTNET_NOLOGO = "1",
                        DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER = "1",
                        MSBUILDDISABLENODEREUSE = "1",
                        DOTNET_EnableDiagnostics_IPC = "0"),
                error_on_status = TRUE, echo = TRUE)
  if (!file.exists(dll)) stop("Build completed without creating the helper DLL.")
  if (force) options(RSqlClientNET.helper_override = dll)
  dll
}

# JSON control commands use newline-terminated stdin. Native Rcpp manages
# the .NET subprocess and reads framed stdout: J (JSON metadata) or D (raw
# result data), then 4-byte little-endian length and payload. No Base64
# transport exists; SQL VARBINARY parameters still use Base64 in JSON control
# requests (not the read-result transport).
`%||%` <- function(x, y) if (is.null(x)) y else x

.sql_transport <- function(transport) {
  if (!is.character(transport) || length(transport)!=1L || is.na(transport) ||
      !identical(transport,"rcpp"))
    stop("Only the native Rcpp binary transport is supported.", call.=FALSE)
  "rcpp"
}

#' Get or set the preferred result format
#' @param format NULL returns the current preference; otherwise "csv" or
#'   "columnar" (default). The value is stored in
#'   options(RSqlClientNET.result_format=...). Both formats use the native
#'   framed Rcpp pipe; CSV remains available for comparison/diagnostics.
#'   This preference affects direct sql_query(), not DBI queries.
#' @return Current format; when setting, the previous format invisibly.
#' @export
#' @noRd
sql_default_format <- function(format = NULL) {
  previous <- getOption("RSqlClientNET.result_format", "columnar")
  if (is.null(format)) return(.sql_check_format(previous))
  options(RSqlClientNET.result_format = .sql_check_format(format))
  invisible(previous)
}

.sql_check_format <- function(value) {
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !value %in% c("csv", "columnar"))
    stop("result_format must be 'csv' or 'columnar'.", call. = FALSE)
  value
}
.sql_result_format <- function(result_format, transport) {
  .sql_transport(transport)
  if (is.null(result_format)) return(sql_default_format())
  .sql_check_format(result_format)
}

.sql_native_start <- function(dotnet, dll) .Call("sqlcpp_start", dotnet, dll, PACKAGE="RSqlClientNET")
.sql_native_send <- function(ptr, command, timeout) .Call("sqlcpp_send", ptr, command, as.numeric(timeout), PACKAGE="RSqlClientNET")
# SBIN v1 writes exact binary bytes after one JSON control-line header.
.sql_native_send_binary <- function(ptr, header, payload, timeout)
  .Call("sqlcpp_send_binary", ptr, header, payload, as.numeric(timeout), PACKAGE="RSqlClientNET")
.sql_native_bulk_encode <- function(columns, kinds)
  .Call("sqlcpp_bulk_encode", columns, kinds, PACKAGE="RSqlClientNET")

.sql_native_receive <- function(ptr, timeout) .Call("sqlcpp_receive", ptr, as.numeric(timeout), PACKAGE="RSqlClientNET")
.sql_native_alive <- function(ptr) isTRUE(.Call("sqlcpp_alive", ptr, PACKAGE="RSqlClientNET"))
.sql_native_close <- function(ptr) invisible(.Call("sqlcpp_close", ptr, PACKAGE="RSqlClientNET"))

# Normalize the direct legacy environment and a DBI S4 connection to the
# SAME helper, without changing the persistent SqlConnection or SCB2 protocol.
# No credentials are retained in this handle; callers must supply them again
# when reconnecting after hard cancellation.
.sql_handle <- function(con, operation = "RSqlClientNET operation") {
  if (inherits(con, "SqlClientNETConnection")) {
    con <- con@handle
  }
  if (!inherits(con, "sqlclient_connection") || !is.environment(con))
    stop(operation, " requires a RSqlClientNET connection (DBI or legacy).",
         call. = FALSE)
  con
}

# An outstanding SqlDataReader is owned by the DBI cursor. Interleaving ANY
# helper command while it is open can steal the cursor's protocol frames.
.sql_check_not_busy <- function(h) {
  if (isTRUE(h$dbi_cursor_active) ||
      (!is.null(h$dbi_result) && !isTRUE(h$dbi_result$cleared)))
    stop("Clear the active DBI result with dbClearResult() first.", call.=FALSE)
  if (isTRUE(h$stream_active))
    stop("Cannot use this connection inside an active streaming callback.",
         call.=FALSE)
  invisible(TRUE)
}

#' Sample current R process memory directly from the operating system
#' @return List containing rss_bytes (resident working set), peak_rss_bytes
#'   (PROCESS LIFETIME peak, not a per-query peak), private_bytes when
#'   available, and the OS measurement source. Unsupported fields are NA.
#' @export
#' @noRd
sql_process_memory <- function() {
  .Call("sqlcpp_process_memory", PACKAGE="RSqlClientNET")
}

.sql_memory_rss <- function() {
  v <- tryCatch(sql_process_memory()$rss_bytes, error=function(e) NA_real_)
  as.numeric(v)
}


#' Read Rcpp native pipe counters for a connection
#' @param con RSqlClientNET connection opened with transport="rcpp".
#' @param reset Return counters and reset them, for per-query measurements.
#' @return Named list of stdout polling and transfer counters.
#' @export
#' @noRd
sql_pipe_stats <- function(con, reset = FALSE) {
  con <- .sql_handle(con, "sql_pipe_stats()")
  if (!inherits(con, "sqlclient_connection") || isTRUE(con$closed) ||
      !identical(con$transport, "rcpp"))
    stop("sql_pipe_stats() requires an open transport='rcpp' connection.", call.=FALSE)
  if (!is.logical(reset) || length(reset) != 1L || is.na(reset))
    stop("reset must be TRUE or FALSE.", call.=FALSE)
  .Call("sqlcpp_pipe_stats", con$native, reset, PACKAGE="RSqlClientNET")
}
.sql_alive <- function(con) .sql_native_alive(con$native)

# A failed or interrupted request can leave queued frames in stdout.  The .NET
# helper may still be running even when R's own deadline expires *between*
# native receive calls.  Reusing that process would corrupt the next reply.
# Terminate it on every incomplete exchange, regardless of transport.
.sql_invalidate <- function(con, reason = "Interrupted or incomplete helper exchange") {
  if (!inherits(con, "sqlclient_connection") || isTRUE(con$closed))
    return(invisible(FALSE))
  con$closed <- TRUE
  con$invalid_reason <- reason
  try(.sql_native_close(con$native), silent=TRUE)
  invisible(TRUE)
}

# Record the actual error and invalidate immediately; on.exit remains a
# second safety net for interrupts, for which R error handlers do not run.
.sql_exchange_error <- function(con, e) {
  .sql_invalidate(con, conditionMessage(e))
  stop(e)
}

.sql_send <- function(con, payload, deadline) {
  seconds <- as.numeric(difftime(deadline, Sys.time(), units="secs"))
  if (seconds <= 0) stop("Timed out writing command to .NET helper.", call.=FALSE)
  .sql_native_send(con$native, enc2utf8(payload), seconds)
  invisible(NULL)
}
# Structured error details from Microsoft.Data.SqlClient, when available.
.sql_helper_error <- function(reply) {
  kind <- reply$exception %||% "HelperError"
  number <- reply$sqlNumber
  suffix <- if (!is.null(number)) paste0(" (SQL error ", number, ")") else ""
  cond <- simpleError(paste0(".NET helper [", kind, "]", suffix, ": ",
                             reply$error %||% "Unknown error"))
  cond$sql_number <- number
  cond$sql_state <- reply$sqlState
  cond$sql_severity <- reply$severity
  # Only the .NET helper can attest to a complete, recoverable SQL error.
  # A failed stream, a damaged frame or a failed COMMIT is never recoverable.
  cond$recoverable <- identical(reply$exception, "SqlException") &&
    isTRUE(reply$recoverable)
  class(cond) <- c(if (cond$recoverable) "sqlclient_recoverable_sql_error",
                   "sqlclient_helper_error", class(cond))
  stop(cond)
}

# Handle a *terminal* SQL error received before a query start frame.
# The server has completed its error reply and neither side has begun sending
# result data. Keep the connection open, but leave on.exit() responsible for
# invalidating it for interrupts, timeouts and every other failure.
.sql_read_query_start <- function(con, deadline, mark_complete) {
  tryCatch(.sql_read_reply(con, deadline),
           sqlclient_recoverable_sql_error = function(e) {
             mark_complete()
             stop(e)
           })
}

.sql_read_reply <- function(con, deadline, allow_data = FALSE) {
  seconds <- as.numeric(difftime(deadline, Sys.time(), units="secs"))
  if (seconds <= 0) stop("Timed out waiting for .NET helper.", call.=FALSE)
  frame <- .sql_native_receive(con$native, seconds)
  if (identical(frame$kind, "D")) {
    if (!allow_data) stop("Unexpected data frame from .NET helper.", call.=FALSE)
    return(list(ok=TRUE, kind="chunk", data=frame$payload))
  }
  if (!identical(frame$kind, "J")) stop("Invalid frame type from .NET helper.", call.=FALSE)
  reply <- tryCatch(jsonlite::fromJSON(rawToChar(frame$payload), simplifyVector=FALSE),
      error=function(e) stop("Invalid framed JSON from .NET helper.", call.=FALSE))
  if (!isTRUE(reply$ok)) .sql_helper_error(reply)
  reply
}
.sql_rpc <- function(con, request, timeout = 600L) {
  if (isTRUE(con$stream_active))
    stop("Cannot issue RPC while a streaming callback is active on this connection.", call.=FALSE)
  if (isTRUE(con$dbi_cursor_active) && !request$op %in% c("cursor_close", "disconnect"))
    stop("Clear active DBI cursor before issuing another command.", call.=FALSE)
  if (!inherits(con, "sqlclient_connection") || isTRUE(con$closed) || !.sql_alive(con))
    stop(".NET helper process is not running.", call. = FALSE)
  if (length(timeout) != 1L || !is.numeric(timeout) ||
      !is.finite(timeout) || timeout <= 0)
    stop("timeout must be a positive finite number of seconds.", call. = FALSE)
  payload <- jsonlite::toJSON(request, auto_unbox = TRUE, null = "null", digits = NA)
  .sql_rpc_payload(con, as.character(payload), timeout=timeout, op=request$op)
}
.sql_rpc_payload <- function(con,payload,timeout=600L,op="request") {
  if (!is.character(payload) || length(payload)!=1L || is.na(payload))
    stop("RPC payload must be one valid JSON string.",call.=FALSE)
  if (isTRUE(con$stream_active))
    stop("Cannot issue RPC while a streaming callback is active on this connection.",call.=FALSE)
  if (isTRUE(con$dbi_cursor_active) && !op %in% c("cursor_close","disconnect"))
    stop("Clear active DBI cursor before issuing another command.",call.=FALSE)
  if (!inherits(con,"sqlclient_connection") || isTRUE(con$closed) || !.sql_alive(con))
    stop(".NET helper process is not running.",call.=FALSE)
  if (length(timeout)!=1L || !is.numeric(timeout) || !is.finite(timeout) || timeout<=0)
    stop("timeout must be a positive finite number of seconds.",call.=FALSE)
  deadline <- Sys.time() + as.numeric(timeout)
  completed <- FALSE
  # On interruption, timeout or an incomplete response, kill the helper.
  # Only a SQL error explicitly confirmed recoverable by C# may leave the
  # session alive. A COMMIT or bulk failure never qualifies.
  on.exit(if (!completed) .sql_invalidate(con), add = TRUE)
  .sql_send(con, payload, deadline)
  reply <- .sql_read_query_start(con, deadline, function() completed <<- TRUE)
  completed <- TRUE
  reply
}

# A complete binary upload request is an atomic native write: JSON header,
# newline, followed by exactly `bytes` raw bytes. Treat ANY incomplete reply
# as fatal so later queries cannot consume stale protocol frames.
.sql_rpc_binary <- function(con, payload, nrows, timeout=600L,
                            allocation_profile=FALSE) {
  if (!identical(con$transport, "rcpp"))
    stop("Binary bulk uploads require transport='rcpp'.", call.=FALSE)
  if (!is.raw(payload)) stop("Binary bulk payload must be raw.", call.=FALSE)
  if (isTRUE(con$stream_active) || isTRUE(con$dbi_cursor_active))
    stop("Cannot perform bulk upload with an active cursor/stream.",call.=FALSE)
  if (!inherits(con,"sqlclient_connection") || isTRUE(con$closed) || !.sql_alive(con))
    stop(".NET helper process is not running.",call.=FALSE)
  if (length(timeout)!=1L || !is.numeric(timeout) || !is.finite(timeout) || timeout<=0)
    stop("timeout must be positive finite seconds.",call.=FALSE)
  header <- as.character(jsonlite::toJSON(
    list(op="bulk_binary_batch", rows=as.integer(nrows), bytes=length(payload),
         allocationProfile=isTRUE(allocation_profile)),auto_unbox=TRUE))
  deadline <- Sys.time() + as.numeric(timeout)
  completed <- FALSE
  on.exit(if (!completed) .sql_invalidate(con),add=TRUE)
  remaining <- as.numeric(difftime(deadline, Sys.time(),units="secs"))
  if (remaining <= 0) stop("Binary bulk upload timed out.",call.=FALSE)
  native_timings <- .sql_native_send_binary(con$native, header, payload, remaining)
  # Timeout applies to the *whole* request, including the outgoing payload.
  reply <- .sql_read_query_start(con, deadline, function() completed <<- TRUE)
  completed <- TRUE
  # Internal diagnostics; native pipe-write and .NET receive time overlap
  # because the processes exchange bytes concurrently.
  if (is.list(native_timings)) {
    reply$nativePacketMs <- native_timings$packetMs
    reply$nativePipeSendMs <- native_timings$pipeSendMs
  }
  reply
}

# Create the child .NET process without connecting to a SQL Server.
.sql_start_helper <- function(transport = "rcpp") {
  transport <- .sql_transport(transport)
  dll <- sqlclient_build()
  dotnet <- unname(Sys.which("dotnet"))
  con <- new.env(parent = emptyenv())
  con$closed <- FALSE
  con$transport <- transport
  con$native <- .sql_native_start(dotnet, dll)
  class(con) <- "sqlclient_connection"
  reg.finalizer(con, function(e) {
    if (!isTRUE(e$closed))
      try(.sql_native_close(e$native), silent=TRUE)
  }, onexit = TRUE)
  con
}

#' Test communication with the .NET helper without contacting SQL Server
#' @param con Optional existing RSqlClientNET connection.
#' @param timeout Seconds to wait for the response.
#' @param transport Only "rcpp" is supported (native framed pipe).
#' @export
#' @noRd
sql_ping <- function(con = NULL, timeout = 10L,
                     transport = "rcpp") {
  if (length(timeout) != 1L || is.na(timeout) || !is.finite(timeout) || timeout <= 0)
    stop("timeout must be a positive number of seconds.", call. = FALSE)
  temporary <- is.null(con)
  if (!temporary) {
    con <- .sql_handle(con, "sql_ping()")
    .sql_check_not_busy(con)
  }
  if (temporary) {
    con <- .sql_start_helper(transport)
    on.exit({
      if (.sql_alive(con)) try(.sql_native_close(con$native), silent=TRUE)
      con$closed <- TRUE
    }, add = TRUE)
  }
  token <- paste0("ping-", Sys.getpid(), "-", format(Sys.time(), "%Y%m%d%H%M%OS6"))
  start <- proc.time()[["elapsed"]]
  answer <- .sql_rpc(con, list(op = "ping", echo = token), timeout = timeout)
  elapsed_ms <- 1000 * (proc.time()[["elapsed"]] - start)
  if (!identical(answer$reply, "pong") || !identical(answer$echo, token))
    stop("The .NET helper returned an invalid ping response.", call. = FALSE)
  list(ok = TRUE, reply = answer$reply, helper_version = answer$helperVersion,
       process_id = answer$processId, connection_state = answer$connectionState,
       runtime_version = answer$runtimeVersion,
       transport = con$transport,
       elapsed_ms = round(elapsed_ms, 1))
}

#' Open a persistent SQL Server connection through Microsoft.Data.SqlClient
#' @param transport Only "rcpp" is supported. Columnar and optional CSV
#'   results both use the native framed binary pipe.
#' @export
#' @noRd
sql_connect <- function(server = NULL, database = NULL,
                        integrated_security = TRUE,
                        username = NULL, password = NULL,
                        encrypt = TRUE, trust_server_certificate = FALSE,
                        connection_string = NULL,
                        transport = "rcpp") {
  con <- .sql_start_helper(transport)
  tryCatch({
    sql_ping(con, timeout = 10L)
    .sql_rpc(con, list(op = "connect", server = server,
                       database = database,
                       integratedSecurity = integrated_security,
                       username = username, password = password,
                       encrypt = encrypt,
                       trustServerCertificate = trust_server_certificate,
                       connectionString = connection_string), timeout = 60L)
  }, error = function(e) {
    try(.sql_native_close(con$native), silent=TRUE)
    con$closed <- TRUE
    stop(conditionMessage(e), call. = FALSE)
  })
  con
}

# In full mode, retain the v0.0.8 mappings: exact SQL decimals and timestamps
# as text, bigint as lossless integer64 except bit64's reserved NA sentinel.
# In native mode, convert exact decimals to double and timestamps to POSIXct.
# Legacy bigint= and datetime= arguments remain explicit overrides.
.sql_precision_options <- function(precision, bigint = NULL, datetime = NULL) {
  precision <- match.arg(precision, c("full", "native"))
  if (is.null(bigint)) bigint <- "integer64"
  else bigint <- match.arg(bigint, c("integer64", "character", "numeric", "integer"))
  if (is.null(datetime)) datetime <- if (precision == "native") "POSIXct" else "character"
  else datetime <- match.arg(datetime, c("character", "POSIXct"))
  list(precision = precision, bigint = bigint, datetime = datetime)
}

.sql_native_numeric <- function(x, sql_type) {
  ans <- suppressWarnings(as.numeric(x))
  if (any(!is.na(x) & !is.finite(ans)))
    stop("Cannot convert SQL ", sql_type, " to finite R numeric without overflow/invalid value.",
         call. = FALSE)
  ans
}

# SQL datetime/datetime2/smalldatetime do not carry timezone information.
# Interpret their wall-clock values in the requested tz (default "" = local).
# Use canonical IANA names, e.g. "Europe/Copenhagen", for portable results.
.sql_validate_tz <- function(tz) {
  if (!is.character(tz) || length(tz) != 1L || is.na(tz))
    stop('tz must be a single non-missing timezone string ("" or e.g. "Europe/Copenhagen").',
         call. = FALSE)
  if (nzchar(tz) && !tz %in% c("UTC", "GMT", OlsonNames()))
    stop('Unknown timezone: ', tz, '. Use an IANA name from OlsonNames().',
         call. = FALSE)
  tz
}

# Identify genuinely ambiguous SQL wall times without depending on which
# side of a DST fold the platform-specific POSIXct parser chooses.
#
# For each distinct calendar date, compare the timezone's UTC offsets at
# midday on the preceding and following dates. Only falling offsets can
# introduce repeated local clock times. For those dates, reconstruct both
# possible UTC instants and round-trip them through the IANA timezone. This
# detects unusual DST shifts (e.g. 30-minute folds) as well as 1-hour folds.
# On ordinary dates, this is O(number of distinct dates), not O(number of
# timestamps) expensive timezone conversions.
.sql_utc_offset_seconds <- function(x, tz) {
  z <- format(x, format = "%z", tz = tz)
  valid <- grepl("^[+-][0-9]{4}$", z)
  ans <- rep(NA_real_, length(z))
  if (any(valid)) {
    zv <- z[valid]
    ans[valid] <- ifelse(substr(zv, 1L, 1L) == "-", -1, 1) *
      (as.numeric(substr(zv, 2L, 3L)) * 3600 +
       as.numeric(substr(zv, 4L, 5L)) * 60)
  }
  ans
}

.sql_ambiguous_local_wall <- function(wall, tz) {
  ans <- rep(FALSE, length(wall))
  if (!length(wall) || tz %in% c("UTC", "GMT")) return(ans)
  dates <- substr(wall, 1L, 10L)
  unique_dates <- unique(dates)
  day <- as.Date(unique_dates, format = "%Y-%m-%d")
  # Avoid TZ-dependent date arithmetic: these are calendar days, not instants.
  noon_before <- as.POSIXct(paste(as.character(day - 1L), "12:00:00"),
                            format = "%Y-%m-%d %H:%M:%S", tz = tz)
  noon_after <- as.POSIXct(paste(as.character(day + 1L), "12:00:00"),
                           format = "%Y-%m-%d %H:%M:%S", tz = tz)
  before <- .sql_utc_offset_seconds(noon_before, tz)
  after <- .sql_utc_offset_seconds(noon_after, tz)
  folded <- which(is.finite(before) & is.finite(after) & before > after)
  if (!length(folded)) return(ans)

  # Build two candidate instants only for dates near an offset decrease.
  idx <- which(dates %in% unique_dates[folded])
  if (!length(idx)) return(ans)
  d <- match(dates[idx], unique_dates)
  utc_wall <- as.POSIXct(substr(wall[idx], 1L, 19L),
                         format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
  stamp <- as.numeric(utc_wall)
  earlier <- as.POSIXct(stamp - before[d], origin = "1970-01-01", tz = "UTC")
  later <- as.POSIXct(stamp - after[d], origin = "1970-01-01", tz = "UTC")
  target <- substr(wall[idx], 1L, 19L)
  match_earlier <- format(earlier, "%Y-%m-%dT%H:%M:%S", tz = tz) == target
  match_later <- format(later, "%Y-%m-%dT%H:%M:%S", tz = tz) == target
  both <- !is.na(match_earlier) & match_earlier &
    !is.na(match_later) & match_later &
    is.finite(stamp) & before[d] != after[d]
  ans[idx] <- both
  ans
}

.sql_warn_wall_rows <- function(kind, indices, tz, column_name = NULL,
                                max_rows = 20L, resolution = "na") {
  if (!length(indices)) return(invisible(NULL))
  shown <- utils::head(indices, max_rows)
  extra <- length(indices) - length(shown)
  where <- if (is.null(column_name)) "" else
    sprintf(" in column '%s'", column_name)
  warning(sprintf(
    if (identical(resolution, "ordered"))
      "%d %s SQL local timestamp value(s)%s in timezone '%s'; resolved using row order (first occurrence uses daylight time, after clock rollback standard time). Row(s): %s%s. Assumes chronologically ordered rows; use precision='full' to preserve exact wall times."
    else
      "%d %s SQL local timestamp value(s)%s in timezone '%s'; converted to NA. Row(s): %s%s. Use precision='full' (or datetime='character') to preserve the source wall times.",
    length(indices), kind, where,
    if (nzchar(tz)) tz else "system local",
    paste(format(shown, scientific = FALSE, trim = TRUE), collapse = ", "),
    if (extra > 0L) sprintf(" (and %s more)", format(extra, scientific = FALSE)) else ""
  ), call. = FALSE)
  invisible(NULL)
}

# SQL BIGINT minimum (-2^63) shares the NA bit pattern in bit64::integer64.
# Warn only for non-NULL SQL values. Indices are 1-based result row numbers;
# the C++ decoder supplies a bounded sample and an exact count per batch.
.sql_warn_bigint_min_rows <- function(indices, column_name = NULL,
                                     count = length(indices), max_rows = 20L) {
  if (!length(indices)) return(invisible(NULL))
  shown <- utils::head(indices, max_rows)
  remaining <- count - length(shown)
  where <- if (is.null(column_name)) "" else sprintf(" in column '%s'", column_name)
  warning(sprintf(
    "%d SQL BIGINT value(s) equal to -9223372036854775808%s cannot be represented by bit64::integer64; converted to NA. Row(s): %s%s. Use bigint='character' to preserve the exact value.",
    count, where,
    paste(format(shown, scientific = FALSE, trim = TRUE), collapse = ", "),
    if (remaining > 0L) sprintf(" (and %s more)", format(remaining, scientific = FALSE)) else ""
  ), call. = FALSE)
  invisible(NULL)
}

.sql_warn_decoded_bigint_min <- function(decoded, column_names, row_offset = 0) {
  counts <- decoded$bigint_min_counts
  if (is.null(counts)) return(invisible(NULL))
  for (j in which(counts > 0L)) {
    indices <- decoded$bigint_min_rows[[j]] + row_offset
    .sql_warn_bigint_min_rows(indices, column_names[[j]], count = counts[[j]])
  }
  invisible(NULL)
}

# Optional ordered DST-fold resolution. Each result column owns its own state
# so a fall-back transition is remembered across DBI fetches and callback
# batches, not only within a single vector of timestamps.
.sql_dst_fallback <- function(value) match.arg(value, c("na", "ordered"))

.sql_dst_ordered <- function(wall, ambiguous, tz, state) {
  idx <- which(ambiguous)
  if (!length(idx)) return(numeric(0))
  if (is.null(state)) state <- new.env(parent = emptyenv())
  # Previous non-NULL input in the full stream, including ordinary clock hours.
  valid_idx <- which(!is.na(wall))
  last_before_batch <- state$last_wall %||% NA_character_
  chosen_winter_date <- state$winter_date %||% NA_character_
  winter <- isTRUE(state$winter)
  winter_choice <- logical(length(idx))
  preceding_positions <- findInterval(idx - 1L, valid_idx)
  for (k in seq_along(idx)) {
    i <- idx[[k]]
    current <- wall[[i]]
    date <- substr(current, 1L, 10L)
    if (is.na(chosen_winter_date) || !identical(chosen_winter_date, date)) {
      chosen_winter_date <- date
      winter <- FALSE
    }
    preceding <- preceding_positions[[k]]
    previous <- if (preceding > 0L) wall[[valid_idx[[preceding]]]] else last_before_batch
    # Local timestamps must be ordered chronologically in result-row order.
    # Detect the wall-clock rollback, even when it falls between fetch calls.
    if (!is.na(previous) && substr(previous, 1L, 10L) == date &&
        current < previous) winter <- TRUE
    winter_choice[[k]] <- winter
  }
  state$winter_date <- chosen_winter_date
  state$winter <- winter
  if (length(valid_idx)) state$last_wall <- wall[[utils::tail(valid_idx, 1L)]]

  dates <- substr(wall[idx], 1L, 10L)
  day <- as.Date(unique(dates), format = "%Y-%m-%d")
  before <- .sql_utc_offset_seconds(as.POSIXct(
    paste(as.character(day - 1L), "12:00:00"),
    format = "%Y-%m-%d %H:%M:%S", tz = tz), tz)
  after <- .sql_utc_offset_seconds(as.POSIXct(
    paste(as.character(day + 1L), "12:00:00"),
    format = "%Y-%m-%d %H:%M:%S", tz = tz), tz)
  d <- match(dates, as.character(day))
  if (anyNA(before[d]) || anyNA(after[d]))
    stop("Unable to determine UTC offsets for repeated local hour.", call. = FALSE)
  offset <- ifelse(winter_choice, after[d], before[d])
  # Parse as UTC wall time, then apply the chosen offset. This avoids the
  # platform-dependent ambiguity of parsing the wall clock directly in tz.
  stamp <- as.numeric(as.POSIXct(wall[idx],
      format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))
  if (any(!is.finite(stamp)))
    stop("Cannot parse repeated local timestamp as UTC wall time.", call. = FALSE)
  stamp - offset
}

# SQL datetimeoffset has an explicit offset: calculate the actual instant
# using UTC, retaining the tzone = "UTC" of that result.
.sql_native_timestamp <- function(x, with_offset = FALSE, tz = "",
                                  datetime_parser = c("lubridate", "base"),
                                  row_offset = 0, column_name = NULL,
                                  dst_fallback = "na", dst_state = NULL) {
  dst_fallback <- .sql_dst_fallback(dst_fallback)
  datetime_parser <- match.arg(datetime_parser)
  tz <- .sql_validate_tz(tz)
  result_tz <- if (with_offset) "UTC" else tz
  out <- as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = result_tz)
  at <- which(!is.na(x))
  if (!length(at)) return(out)
  y <- x[at]
  date_time_re <- "[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{1,7})?"
  if (with_offset) {
    valid <- grepl(paste0("^", date_time_re, "([+-][0-9]{2}:[0-9]{2}|Z)$"), y)
    if (!all(valid)) stop("Invalid SQL datetimeoffset format.", call. = FALSE)
    zulu <- endsWith(y, "Z")
    wall <- ifelse(zulu, substr(y, 1L, nchar(y) - 1L),
                   substr(y, 1L, nchar(y) - 6L))
    tzpart <- substring(y, nchar(y) - 5L)
    offsets <- numeric(length(y))
    if (any(!zulu)) {
      o <- tzpart[!zulu]
      hours <- as.integer(substr(o, 2L, 3L))
      mins <- as.integer(substr(o, 5L, 6L))
      if (any(hours > 14L | mins > 59L | (hours == 14L & mins != 0L)))
        stop("Invalid SQL datetimeoffset UTC offset.", call. = FALSE)
      offsets[!zulu] <- (hours * 3600 + mins * 60) *
        ifelse(substr(o, 1L, 1L) == "+", 1, -1)
    }
  } else {
    if (!all(grepl(paste0("^", date_time_re, "$"), y)))
      stop("Invalid SQL datetime/datetime2 format.", call. = FALSE)
    wall <- y
    offsets <- numeric(length(y))
  }
  # A SQL datetime2 is a valid wall-clock value, but that local wall time
  # may not exist in the requested IANA timezone (e.g., Copenhagen's DST gap).
  # The fast parser can SILENTLY normalize such times to 03:30. Never change
  # the intended wall time: verify the conversion by a local round trip.
  # SQL protocol/format violations above remain hard errors.
  parsed <- suppressWarnings(if (identical(datetime_parser, "lubridate")) {
    lubridate::parse_date_time2(wall, orders = "YmdHMOS",
                                tz = result_tz, lt = FALSE)
  } else {
    as.POSIXct(wall, format = "%Y-%m-%dT%H:%M:%OS", tz = result_tz)
  })
  bad <- is.na(parsed) | !is.finite(as.numeric(parsed))
  good <- which(!bad)
  if (length(good)) {
    roundtrip <- format(parsed[good], format = "%Y-%m-%dT%H:%M:%S",
                        tz = result_tz, usetz = FALSE)
    bad[good] <- is.na(roundtrip) | roundtrip != substr(wall[good], 1L, 19L)
  }
  # datetimeoffset already denotes an absolute instant; only timezone-less
  # SQL datetime/datetime2/smalldatetime can be ambiguous in a DST fold.
  ambiguous <- if (!with_offset && !result_tz %in% c("UTC", "GMT"))
    .sql_ambiguous_local_wall(wall, result_tz) else rep(FALSE, length(wall))
  gap_or_unrepresentable <- bad & !ambiguous
  if (any(gap_or_unrepresentable))
    .sql_warn_wall_rows("nonexistent/unrepresentable",
      row_offset + at[gap_or_unrepresentable], result_tz, column_name)
  if (any(ambiguous)) {
    if (identical(dst_fallback, "ordered")) {
      # State must advance through non-ambiguous rows too; keep the full wall
      # vector in source order (including SQL NULL) for boundary detection.
      instants <- .sql_dst_ordered(wall, ambiguous, result_tz, dst_state)
      parsed[ambiguous] <- as.POSIXct(instants, origin="1970-01-01", tz=result_tz)
      bad[ambiguous] <- FALSE
      .sql_warn_wall_rows("ambiguous (repeated clock-hour)",
        row_offset + at[ambiguous], result_tz, column_name,
        resolution = "ordered")
    } else {
      .sql_warn_wall_rows("ambiguous (repeated clock-hour)",
        row_offset + at[ambiguous], result_tz, column_name)
    }
  }
  if (identical(dst_fallback, "ordered") && !with_offset &&
      !is.null(dst_state) && !any(ambiguous)) {
    last_valid <- utils::tail(which(!is.na(wall)), 1L)
    if (length(last_valid)) dst_state$last_wall <- wall[[last_valid]]
  }
  # The default remains lossless with respect to uncertainty: no instant is
  # fabricated if a timezone-less SQL timestamp is ambiguous.
  parsed[bad | (ambiguous & dst_fallback == "na")] <-
    as.POSIXct(NA_real_, origin="1970-01-01", tz=result_tz)
  out[at] <- parsed - offsets
  out
}

# SQL time => seconds elapsed since midnight, with a difftime class. This
# carries no artificial calendar date and can be used in arithmetic.
.sql_native_time <- function(x) {
  seconds <- rep(NA_real_, length(x))
  at <- which(!is.na(x))
  if (length(at)) {
    y <- x[at]
    valid <- grepl("^[0-9]{2}:[0-9]{2}:[0-9]{2}(\\.[0-9]{1,7})?$", y)
    if (!all(valid)) stop("Invalid SQL time format.", call. = FALSE)
    h <- as.integer(substr(y, 1L, 2L))
    m <- as.integer(substr(y, 4L, 5L))
    sec <- as.integer(substr(y, 7L, 8L))
    if (any(h > 23L | m > 59L | sec > 59L))
      stop("Invalid SQL time value.", call. = FALSE)
    fractions <- numeric(length(y))
    fractional <- nchar(y) > 8L
    if (any(fractional))
      fractions[fractional] <- as.numeric(substring(y[fractional], 9L))
    seconds[at] <- h * 3600 + m * 60 + sec + fractions
  }
  as.difftime(seconds, units = "secs")
}

# Numeric and binary types are transported as tagged, UTF-8 CSV fields:
#    N       => SQL NULL
#    V<text> => non-NULL value (including V alone for empty string).
# This prevents collisions with NA, empty strings, or any random SQL text.
.sql_convert_column <- function(x, sql_type, bigint, datetime, binary,
                                unsupported, precision, tz = "",
                                datetime_parser = c("lubridate", "base"),
                                row_offset = 0, column_name = NULL,
                                dst_fallback = "na", dst_state = NULL) {
  datetime_parser <- match.arg(datetime_parser)
  type <- tolower(sql_type)
  if (type %in% c("char", "varchar", "nchar", "nvarchar", "text", "ntext",
                  "xml", "uniqueidentifier", "sysname", "json"))
    return(x)
  if (type %in% c("decimal", "numeric", "money", "smallmoney"))
    return(if (precision == "native") .sql_native_numeric(x, type) else x)
  if (type == "bigint") {
    if (bigint == "character") return(x)
    if (bigint == "numeric") return(suppressWarnings(as.numeric(x)))
    if (bigint == "integer") return(suppressWarnings(as.integer(x)))
    if (!requireNamespace("bit64", quietly = TRUE))
      stop("Install 'bit64' to read bigint as integer64.", call. = FALSE)
    # Distinguish a real SQL -2^63 from NULL before converting. Both become
    # bit64's NA_integer64_ sentinel, but only the former deserves a warning.
    lost <- which(!is.na(x) & x == "-9223372036854775808")
    if (length(lost))
      .sql_warn_bigint_min_rows(row_offset + lost, column_name)
    return(suppressWarnings(bit64::as.integer64(x)))
  }
  if (type %in% c("tinyint", "smallint", "int")) {
    # R reserves INT32_MIN for NA_integer_. A SQL int of -2147483648 is a
    # legitimate value: promote to numeric (all int32 values are exact doubles).
    if (type == "int" && any(!is.na(x) & x == "-2147483648")) {
      out <- suppressWarnings(as.numeric(x))
      if (any(!is.na(x) & is.na(out))) stop("Cannot convert SQL int to R numeric.", call.=FALSE)
      return(out)
    }
    out <- suppressWarnings(as.integer(x))
    if (any(!is.na(x) & is.na(out))) stop("Cannot convert SQL ", type, " to R integer.", call. = FALSE)
    return(out)
  }
  if (type == "bit") {
    valid <- is.na(x) | x %in% c("TRUE", "FALSE")
    if (!all(valid)) stop("Invalid SQL bit value.", call. = FALSE)
    return(ifelse(is.na(x), NA, x == "TRUE"))
  }
  if (type %in% c("real", "float")) {
    out <- suppressWarnings(as.numeric(x))
    if (any(!is.na(x) & is.na(out))) stop("Cannot convert SQL ", type, " to R numeric.", call. = FALSE)
    return(out)
  }
  if (type == "date") {
    out <- as.Date(x, format = "%Y-%m-%d")
    if (any(!is.na(x) & is.na(out))) stop("Invalid SQL date.", call. = FALSE)
    return(out)
  }
  if (type %in% c("datetime", "datetime2", "smalldatetime")) {
    if (datetime == "character") return(x)
    # SQL timestamps without an offset are interpreted as wall-clock times in tz.
    return(.sql_native_timestamp(x, tz = tz, datetime_parser = datetime_parser,
                                 row_offset = row_offset, column_name = column_name,
                                 dst_fallback = dst_fallback, dst_state = dst_state))
  }
  if (type == "datetimeoffset")
    return(if (precision == "native")
      .sql_native_timestamp(x, with_offset = TRUE,
                            datetime_parser = datetime_parser,
                            row_offset = row_offset, column_name = column_name) else x)
  if (type == "time")
    return(if (precision == "native") .sql_native_time(x) else x)
  if (type %in% c("binary", "varbinary", "image", "timestamp", "rowversion")) {
    if (binary == "base64") return(x)
    return(lapply(x, function(v) if (is.na(v)) NULL else jsonlite::base64_dec(v)))
  }
  if (unsupported == "character") return(x)
  stop("Unsupported SQL type '", sql_type,
       "'. Re-run with unsupported='character' to opt into text representation.",
       call. = FALSE)
}

#' Decode tagged in-memory CSV with SQL schema-driven conversions.
#' `fread` currently leaves doubled interior quotes in some CSV fields; the
#' existing correction is available by default (only for the fread parser).
#' @noRd
.sql_parse_csv <- function(csv_text, start, parser = c("fread", "read.csv"),
                           quote_correction = TRUE, metrics = NULL,
                           bigint = NULL, datetime = NULL,
                           binary = c("raw", "base64"),
                           unsupported = c("error", "character"),
                           precision = c("full", "native"), tz = "",
                           datetime_parser = c("lubridate", "base"),
                           dst_fallback = "na") {
  tz <- .sql_validate_tz(tz)
  datetime_parser <- match.arg(datetime_parser)
  parser <- match.arg(parser)
  choices <- .sql_precision_options(precision, bigint, datetime)
  precision <- choices$precision
  bigint <- choices$bigint
  datetime <- choices$datetime
  binary <- match.arg(binary)
  unsupported <- match.arg(unsupported)
  if (!is.logical(quote_correction) || length(quote_correction) != 1L || is.na(quote_correction))
    stop("quote_correction must be TRUE or FALSE.", call. = FALSE)
  if (!identical(start$valueEncoding, "tagged-v1"))
    stop("The helper does not support tagged-v1 type-safe values. Rebuild RSqlClientNET 0.0.14.", call. = FALSE)
  types <- unlist(start$types, use.names = FALSE)
  names_from_server <- unlist(start$columns, use.names = FALSE)
  Encoding(csv_text) <- "UTF-8"
  t_parse <- proc.time()[["elapsed"]]
  if (identical(parser, "fread")) {
    dt <- data.table::fread(text = csv_text, sep = ",", quote = "\"",
               header = TRUE, na.strings = NULL,
               colClasses = list(character = seq_along(types)),
               check.names = FALSE, encoding = "UTF-8", showProgress = FALSE)
  } else {
    text_con <- textConnection(csv_text, open = "r", local = TRUE)
    dt <- tryCatch(
      data.table::as.data.table(utils::read.csv(
        file = text_con, header = TRUE, sep = ",", quote = "\"",
        check.names = FALSE, stringsAsFactors = FALSE,
        na.strings = character(0), colClasses = rep("character", length(types)),
        comment.char = "", dec = ".", strip.white = FALSE
      )), finally = close(text_con)
    )
  }
  parse_seconds <- proc.time()[["elapsed"]] - t_parse
  if (ncol(dt) != length(types))
    stop("SQL schema column count does not match the CSV result.", call. = FALSE)
  # Restore original SQL column names, including non-syntactic names.
  data.table::setnames(dt, names_from_server)
  correction_seconds <- 0
  if (identical(parser, "fread") && quote_correction) {
    t_fix <- proc.time()[["elapsed"]]
    for (j in seq_along(types)) {
      data.table::set(dt, j = j, value = gsub('""', '"', dt[[j]], fixed = TRUE))
    }
    correction_seconds <- proc.time()[["elapsed"]] - t_fix
  }
  t_convert <- proc.time()[["elapsed"]]
  for (j in seq_along(types)) {
    field <- dt[[j]]
    # Reject corruption: each non-NULL CSV field must start with V, NULL is N.
    valid <- !is.na(field) & (field == "N" | startsWith(field, "V"))
    if (!all(valid)) stop("Invalid tagged SQL value in column ", j, call. = FALSE)
    is_null <- field == "N"
    field <- substring(field, 2L)
    field[is_null] <- NA_character_
    converted <- .sql_convert_column(field, types[[j]], bigint, datetime,
                                     binary, unsupported, precision, tz = tz,
                                     datetime_parser = datetime_parser,
                                     column_name = names_from_server[[j]],
                                     dst_fallback = dst_fallback)
    data.table::set(dt, j = j, value = converted)
  }
  type_conversion_seconds <- proc.time()[["elapsed"]] - t_convert
  attr(dt, "sqlclient_schema") <- data.frame(index = seq_along(types),
         name = names_from_server, sql_type = types, stringsAsFactors = FALSE)
  if (!is.null(metrics)) {
    metrics$parse_seconds <- parse_seconds
    metrics$quote_correction_seconds <- correction_seconds
    metrics$type_conversion_seconds <- type_conversion_seconds
  }
  dt
}

# Validate stream block size; default is 256 KiB (previously 24 KiB).
.sql_chunk_kib <- function(chunk_kib) {
  if (!is.numeric(chunk_kib) || length(chunk_kib) != 1L ||
      is.na(chunk_kib) || !is.finite(chunk_kib) ||
      chunk_kib != floor(chunk_kib) || chunk_kib < 8 || chunk_kib > 1024)
    stop("chunk_kib must be a whole number between 8 and 1024.", call. = FALSE)
  as.integer(chunk_kib)
}

# Receive query results entirely in RAM. The timings for .NET and R are
# separate perspectives on overlapping work: they must NOT simply be added.
.sql_stream_query <- function(con, request, timeout = 600L,
                              parser = c("fread", "read.csv"),
                              quote_correction = TRUE, chunk_kib = 256L,
                              bigint = NULL, datetime = NULL,
                              binary = "raw", unsupported = "error",
                              precision = c("full", "native"), tz = "",
                              datetime_parser = c("lubridate", "base"),
                              dst_fallback = "na") {
  tz <- .sql_validate_tz(tz)
  datetime_parser <- match.arg(datetime_parser)
  t_total <- proc.time()[["elapsed"]]
  rss_before <- .sql_memory_rss()
  r_rss_peak <- rss_before
  sample_rss <- function() {
    now <- .sql_memory_rss()
    if (is.finite(now)) r_rss_peak <<- if (is.finite(r_rss_peak)) max(r_rss_peak,now) else now
  }
  parser <- match.arg(parser)
  choices <- .sql_precision_options(precision, bigint, datetime)
  precision <- choices$precision
  bigint <- choices$bigint
  datetime <- choices$datetime
  if (!is.logical(quote_correction) || length(quote_correction) != 1L ||
      is.na(quote_correction))
    stop("quote_correction must be TRUE or FALSE.", call. = FALSE)
  chunk_kib <- .sql_chunk_kib(chunk_kib)
  if (!inherits(con, "sqlclient_connection") || isTRUE(con$closed) ||
      !.sql_alive(con))
    stop(".NET helper process is not running.", call. = FALSE)
  if (length(timeout) != 1L || !is.numeric(timeout) ||
      !is.finite(timeout) || timeout <= 0)
    stop("timeout must be a positive finite number of seconds.", call. = FALSE)

  request$chunkSizeKiB <- chunk_kib
  deadline <- Sys.time() + as.numeric(timeout)
  t0 <- proc.time()[["elapsed"]]
  payload <- jsonlite::toJSON(request, auto_unbox = TRUE, null = "null", digits = NA)
  serialize_seconds <- proc.time()[["elapsed"]] - t0

  completed <- FALSE
  # A streaming timeout may happen BETWEEN native receives, before the C++
  # layer sees the deadline.  Invalidate here too so that the NEXT SQL query
  # can never consume stale D frames from a previous result.
  on.exit(if (!completed) .sql_invalidate(con), add = TRUE)
  t0 <- proc.time()[["elapsed"]]
  .sql_send(con, payload, deadline)
  send_seconds <- proc.time()[["elapsed"]] - t0

  t0 <- proc.time()[["elapsed"]]
  start <- .sql_read_query_start(con, deadline, function() completed <<- TRUE)
  wait_start_seconds <- proc.time()[["elapsed"]] - t0
  if (!identical(start$kind, "start"))
    stop("Expected a query start message from .NET helper.", call. = FALSE)
  if (is.null(start$chunkSizeBytes))
    stop("An older .NET helper is running. Disconnect, restart R and rebuild the helper.",
         call. = FALSE)
  if (as.integer(start$chunkSizeBytes) != chunk_kib * 1024L)
    stop(".NET helper did not accept the requested chunk size.", call. = FALSE)

  raw_chunks <- list()
  n_chunks <- 0L
  n_bytes <- 0
  read_stream_seconds <- 0
  base64_decode_seconds <- 0
  repeat {
    t0 <- proc.time()[["elapsed"]]
    reply <- .sql_read_reply(con, deadline, allow_data = TRUE)
    read_stream_seconds <- read_stream_seconds + proc.time()[["elapsed"]] - t0
    if (identical(reply$kind, "chunk")) {
      bytes <- reply$data  # raw D frame; no Base64 decode
      n_chunks <- n_chunks + 1L
      n_bytes <- n_bytes + length(bytes)
      raw_chunks[[n_chunks]] <- bytes
      if (n_chunks %% 32L == 0L) sample_rss()
    } else if (identical(reply$kind, "end")) {
      break
    } else {
      stop("Unexpected message in .NET query stream.", call. = FALSE)
    }
  }
  if (n_chunks == 0L) stop("Query returned no CSV header.", call. = FALSE)
  # Cross-check transport integrity before parsing. The complete CSV is held
  # in memory; no data-bearing temporary file is created.
  if (!is.null(reply$chunksSent) && n_chunks != as.integer(reply$chunksSent))
    stop("Received chunk count differs from the C# sender.", call. = FALSE)
  if (!is.null(reply$bytesSent) && n_bytes != as.numeric(reply$bytesSent))
    stop("Received byte count differs from the C# sender.", call. = FALSE)

  t0 <- proc.time()[["elapsed"]]
  all_bytes <- do.call(c, raw_chunks)
  sample_rss()
  assemble_raw_seconds <- proc.time()[["elapsed"]] - t0
  rm(raw_chunks)

  t0 <- proc.time()[["elapsed"]]
  csv_text <- rawToChar(all_bytes) # decode UTF-8 only after concatenating bytes
  utf8_conversion_seconds <- proc.time()[["elapsed"]] - t0
  rm(all_bytes)

  metrics <- new.env(parent = emptyenv())
  t0 <- proc.time()[["elapsed"]]
  dt <- .sql_parse_csv(csv_text, start, parser = parser,
                       quote_correction = quote_correction, metrics = metrics,
                       bigint = bigint, datetime = datetime, binary = binary,
                       unsupported = unsupported, precision = precision, tz = tz,
                       datetime_parser = datetime_parser,
                       dst_fallback = dst_fallback)
  import_seconds <- proc.time()[["elapsed"]] - t0
  rm(csv_text)
  if (nrow(dt) != as.numeric(reply$rows))
    stop("Received ", nrow(dt), " rows but .NET reported ", reply$rows,
         ". This result should not be trusted.", call. = FALSE)
  sample_rss()

  # Columnar and CSV expose the same metadata fields. Detailed per-type
  # timings are not instrumented for CSV, so use NA rather than inventing 0.
  tab <- table(tolower(sql_schema(dt)$sql_type))
  attr(dt, "sqlclient_type_timings") <- data.frame(
    sql_type=names(tab), columns=as.integer(tab),
    post_decode_seconds=rep(NA_real_, length(tab)),
    stringsAsFactors=FALSE)
  attr(dt, "sqlclient_timings") <- list(
    execute_seconds = reply$executeSeconds,
    fetch_and_stream_seconds = reply$fetchAndStreamSeconds,
    serialize_seconds = serialize_seconds,
    send_seconds = send_seconds,
    wait_start_seconds = wait_start_seconds,
    read_stream_seconds = read_stream_seconds,
    base64_decode_seconds = base64_decode_seconds,
    native_decode_seconds = 0,
    assemble_raw_seconds = assemble_raw_seconds,
    utf8_conversion_seconds = utf8_conversion_seconds,
    assemble_columns_seconds = 0,
    parse_seconds = metrics$parse_seconds,
    quote_correction_seconds = metrics$quote_correction_seconds,
    type_conversion_seconds = metrics$type_conversion_seconds,
    import_r_seconds = import_seconds,
    chunk_kib = chunk_kib,
    batch_rows = NA_integer_,
    transport = con$transport,
    result_format = "csv",
    chunks_received = n_chunks,
    bytes_received = n_bytes,
    helper_peak_working_set_bytes = reply$helperPeakWorkingSetBytes %||% NA_real_,
    helper_query_peak_rss_bytes = reply$helperQueryPeakWorkingSetBytes %||% NA_real_,
    helper_query_peak_private_bytes = reply$helperQueryPeakPrivateBytes %||% NA_real_,
    helper_rss_before_bytes = reply$helperStartWorkingSetBytes %||% NA_real_,
    r_rss_before_bytes = rss_before,
    r_query_sampled_peak_rss_bytes = r_rss_peak,
    r_rss_after_bytes = .sql_memory_rss(),
    # Keep CSV and columnar timing schemas identical. CSV never executes
    # an R streaming callback, so its callback time is exactly zero.
    stream_callback_seconds = 0,
    parser = parser,
    bigint = bigint, datetime = datetime, binary = binary,
    precision = precision,
    tz = tz,
    datetime_parser = datetime_parser,
    quote_correction = identical(parser, "fread") && quote_correction,
    total_seconds = proc.time()[["elapsed"]] - t_total
  )
  completed <- TRUE
  dt
}

#' Specify a precise SQL parameter type (recommended for bigint/decimal/binary)
#' @param value Scalar value, NULL/NA or raw vector for binary types.
#' @param type SQL Server type such as bigint, decimal, datetime2, varbinary.
#' @param precision Optional SQL numeric precision (1..38).
#' @param scale Optional SQL numeric scale (0..38).
#' @export
#' @noRd
sql_param <- function(value, type, precision = NULL, scale = NULL) {
  if (!is.character(type) || length(type) != 1L || !nzchar(type))
    stop("type must be a non-empty SQL type.", call. = FALSE)
  type <- tolower(type)
  supported <- c("bigint", "int", "smallint", "tinyint", "decimal", "numeric",
                 "money", "smallmoney", "float", "real", "bit", "date",
                 "datetime", "datetime2", "datetimeoffset", "smalldatetime",
                 "time", "uniqueidentifier", "varchar", "nvarchar", "char",
                 "nchar", "varbinary", "binary", "xml")
  if (!type %in% supported)
    stop("Unsupported SQL parameter type: ", type, call. = FALSE)
  if (!(is.null(value) || (length(value) == 1L && is.na(value)) ||
        (type %in% c("binary", "varbinary") && is.raw(value)) || length(value) == 1L))
    stop("Parameter value must be scalar, NULL, or a raw vector for binary values.", call. = FALSE)
  if (!is.null(precision) && (length(precision) != 1L || is.na(precision) ||
      precision != as.integer(precision) || precision < 1L || precision > 38L))
    stop("precision must be between 1 and 38.", call. = FALSE)
  if (!is.null(scale) && (length(scale) != 1L || is.na(scale) ||
      scale != as.integer(scale) || scale < 0L || scale > 38L))
    stop("scale must be between 0 and 38.", call. = FALSE)
  if (type %in% c("bigint", "decimal", "numeric") && is.numeric(value) &&
      !inherits(value, "integer64") && !is.na(value) &&
      (type != "bigint" || abs(value) > 2^53))
    stop("Provide bigint >2^53 and decimal/numeric values as character to avoid lost precision.",
         call. = FALSE)
  structure(list(type = type, value = value,
                 precision = precision, scale = scale), class = "sqlclient_parameter")
}

# A DBI blob may be represented as raw(0), a raw vector, a blob::blob()
# vctrs list-of wrapper, or one/more singleton list layers.  Unwrap only
# containers whose innermost value is actually raw or NULL.  Leave all
# non-binary lists unchanged so unexpected inputs still raise an error.
.sql_unwrap_binary_param <- function(value) {
  original <- value
  for (i in seq_len(32L)) {
    if (is.raw(value) || is.null(value)) return(value)
    if (inherits(value, "sqlclient_parameter") || !is.list(value) ||
        length(value) != 1L) return(original)
    value <- value[[1L]]
  }
  original
}

.sql_encode_params <- function(params) {
  if (!is.list(params)) stop("params must be a named list.", call. = FALSE)
  if (length(params) && (is.null(names(params)) || any(!nzchar(names(params)))))
    stop("params must be a named list.", call. = FALSE)
  lapply(params, function(p) {
    # DBI raw/blob elements may be wrapped in one list layer.  A raw vector
    # of any length (including length zero) is ONE binary SQL parameter.
    p <- .sql_unwrap_binary_param(p)
    # DBItest binds list(NULL) and blob/vctrs NULL elements for SQL NULL.
    # Unlike raw(0), which represents a valid empty VARBINARY, R NULL has
    # length zero and must bypass the untyped scalar-length check below.
    # lapply() preserves the named list element; jsonlite's null="null"
    # sends it as JSON null, which the .NET helper binds as DBNull.Value.
    if (is.null(p)) return(NULL)
    if (inherits(p, "POSIXlt")) p <- as.POSIXct(p)
    if (inherits(p, "integer64")) p <- sql_param(as.character(p), "bigint")
    if (inherits(p, "Date"))
      p <- sql_param(if (is.na(p)) NULL else format(p, "%Y-%m-%d"), "date")
    if (inherits(p, "POSIXct")) {
      # Preserve the local wall-clock used by dbQuoteLiteral() / T-SQL
      # datetime2 comparisons, rather than allowing jsonlite to infer a
      # lossy or differently-zoned textual representation.
      p <- sql_param(if (is.na(p)) NULL else
        format(p, "%Y-%m-%dT%H:%M:%OS6"), "datetime2")
    }
    if (inherits(p, "difftime")) {
      seconds <- as.numeric(p, units="secs")
      if (is.na(seconds)) p <- sql_param(NULL,"time")
      else {
        if (!is.finite(seconds) || seconds < 0 || seconds >= 86400)
          stop("SQL time parameters must fall within a 24-hour day.",call.=FALSE)
        hrs <- floor(seconds / 3600)
        mins <- floor((seconds %% 3600) / 60)
        sec <- seconds %% 60
        p <- sql_param(sprintf("%02d:%02d:%09.6f", hrs, mins, sec), "time")
      }
    }
    if (is.factor(p)) p <- as.character(p)
    if (is.raw(p)) p <- sql_param(p, "varbinary")
    if (inherits(p, "sqlclient_parameter")) {
      v <- p$value
      null_value <- is.null(v) || (length(v) == 1L && is.na(v))
      if (null_value) v <- NULL
      else if (is.raw(v)) v <- jsonlite::base64_enc(v)
      else if (inherits(v, "integer64")) v <- as.character(v)
      else if (inherits(v, "Date")) v <- format(v, "%Y-%m-%d")
      else if (inherits(v, "POSIXct")) v <- format(v, "%Y-%m-%dT%H:%M:%OS6", tz = "UTC")
      else v <- as.character(v)
      return(list(`$sqlclient_type` = p$type, value = v,
                  precision = p$precision, scale = p$scale))
    }
    if (length(p) != 1L)
      stop("Each untyped parameter must be scalar (class: ",
           paste(class(p), collapse="/"), "; length: ", length(p), ").",
           call. = FALSE)
    if (is.na(p)) return(NULL)
    if (is.numeric(p) && (is.infinite(p) || is.nan(p)))
      stop("Untyped numeric SQL parameters must be finite.", call. = FALSE)
    p
  })
}

# Columnar-v2 receives typed batches over the existing Rcpp J/D-framed pipe.
# Every D frame has SCB2 magic, row/column counts, validity bitmaps and
# typed payloads.  Rcpp checks bounds and builds R vectors directly.
.sql_wire_kind <- function(types, precision, datetime, tz) {
  vapply(tolower(types), function(x) {
    if (x == "bit") "B"
    else if (x %in% c("tinyint", "smallint", "int")) "I"
    else if (x == "bigint") "L"
    else if (x %in% c("real", "float")) "D"
    else if (x %in% c("decimal", "numeric", "money", "smallmoney") && precision == "native") "N"
    else if (x == "date") "A"
    else if (x == "time" && precision == "native") "H"
    else if (x %in% c("datetime", "datetime2", "smalldatetime") &&
             datetime == "POSIXct") if (tz %in% c("UTC", "GMT")) "T" else "W"
    else if (x == "datetimeoffset" && precision == "native") "O"
    else if (x %in% c("binary", "varbinary", "image", "timestamp", "rowversion")) "Y"
    else "S"
  }, character(1), USE.NAMES = FALSE)
}

# Convert any columns still requiring R-level conversion. Used for complete
# results AND streamed batches to preserve the same precision/NULL semantics.
.sql_columnar_cast <- function(dt, types, expected_kinds, bigint, datetime,
                               binary, unsupported, precision, tz,
                               datetime_parser, row_offset = 0,
                               dst_fallback = "na", dst_states = NULL) {
  type_time <- stats::setNames(numeric(0), character(0))
  for (j in seq_along(types)) {
    type <- tolower(types[[j]])
    kind <- expected_kinds[[j]]
    t_col <- proc.time()[["elapsed"]]
    # SCB2 decodes bigint to exact character unless integer64 was requested.
    # DBI's integer/numeric bigint modes are deliberate lossy conversions.
    if (kind == "L" && bigint %in% c("integer", "numeric")) {
      converted <- if (bigint == "integer") suppressWarnings(as.integer(dt[[j]]))
                   else suppressWarnings(as.numeric(dt[[j]]))
      data.table::set(dt,j=j,value=converted)
      next
    }
    if (kind == "W") {
      dst_state <- NULL
      if (identical(dst_fallback, "ordered")) {
        if (is.null(dst_states)) dst_states <- new.env(parent=emptyenv())
        key <- as.character(j)
        if (!exists(key, envir=dst_states, inherits=FALSE))
          assign(key, new.env(parent=emptyenv()), envir=dst_states)
        dst_state <- get(key, envir=dst_states, inherits=FALSE)
      }
      data.table::set(dt, j=j, value=.sql_binary_wall_to_posixct(
        dt[[j]], tz=tz, row_offset=row_offset,
        column_name=names(dt)[[j]], dst_fallback=dst_fallback,
        dst_state=dst_state))
      duration <- proc.time()[["elapsed"]] - t_col
      if (!type %in% names(type_time)) type_time[type] <- 0
      type_time[type] <- type_time[type] + duration
      next
    }
    if (kind %in% c("I", "D", "B", "N", "T", "O", "H", "A", "L")) next
    if (kind == "Y") {
      if (identical(binary, "base64")) {
        column <- lapply(dt[[j]], function(v) if (is.null(v)) NA_character_
                         else jsonlite::base64_enc(v))
        data.table::set(dt, j=j, value=unlist(column, use.names=FALSE))
        duration <- proc.time()[["elapsed"]] - t_col
        if (!type %in% names(type_time)) type_time[type] <- 0
        type_time[type] <- type_time[type] + duration
      }
      next
    }
    dst_state <- NULL
    if (identical(dst_fallback, "ordered") &&
        type %in% c("datetime", "datetime2", "smalldatetime")) {
      if (is.null(dst_states)) dst_states <- new.env(parent=emptyenv())
      key <- as.character(j)
      if (!exists(key, envir=dst_states, inherits=FALSE))
        assign(key, new.env(parent=emptyenv()), envir=dst_states)
      dst_state <- get(key, envir=dst_states, inherits=FALSE)
    }
    converted <- .sql_convert_column(dt[[j]], types[[j]], bigint, datetime,
                                      binary, unsupported, precision,
                                      tz=tz, datetime_parser=datetime_parser,
                                      row_offset=row_offset,
                                      column_name=names(dt)[[j]],
                                      dst_fallback=dst_fallback, dst_state=dst_state)
    data.table::set(dt, j=j, value=converted)
    duration <- proc.time()[["elapsed"]] - t_col
    if (!type %in% names(type_time)) type_time[type] <- 0
    type_time[type] <- type_time[type] + duration
  }
  list(data=dt, type_seconds=type_time)
}

.sql_columnar_query <- function(con, request, timeout = 600L, batch_rows = 32768L,
                                bigint = NULL, datetime = NULL, binary = "raw",
                                unsupported = "error", precision = c("full", "native"),
                                tz = "", datetime_parser = c("lubridate", "base"),
                                dst_fallback = "na", callback = NULL) {
  if (!is.null(callback) && !is.function(callback))
    stop("callback must be a function(batch) or NULL.", call.=FALSE)
  if (isTRUE(con$stream_active))
    stop("Connection is already streaming. Do not query it from its callback.", call.=FALSE)
  if (isTRUE(con$dbi_cursor_active))
    stop("Clear active DBI cursor before querying.", call.=FALSE)
  if (!identical(con$transport, "rcpp"))
    stop("Columnar results require transport='rcpp'.", call. = FALSE)
  if (!sql_is_valid(con)) stop(".NET helper process is not running.", call. = FALSE)
  if (!is.numeric(timeout) || length(timeout) != 1L || !is.finite(timeout) || timeout <= 0)
    stop("timeout must be a positive finite number of seconds.", call. = FALSE)
  # SQL Server / SqlClient has not yet been validated with 65536-row batches
  # on Windows. Retain that size for the SQL-free memory_test diagnostic only.
  is_synthetic_memory_test <- identical(request$op, "memory_test")
  max_batch_rows <- if (is_synthetic_memory_test) 65536L else 32768L
  if (!is.numeric(batch_rows) || length(batch_rows) != 1L || is.na(batch_rows) ||
      !is.finite(batch_rows) || batch_rows < 1 || batch_rows > max_batch_rows ||
      batch_rows != as.integer(batch_rows))
    stop(sprintf("batch_rows must be a whole number from 1 to %d%s.",
      max_batch_rows, if (is_synthetic_memory_test) "" else
        " for SQL queries in v0.0.27 (65536 remains diagnostic-only)"), call. = FALSE)
  tz <- .sql_validate_tz(tz)
  datetime_parser <- match.arg(datetime_parser)
  dst_fallback <- .sql_dst_fallback(dst_fallback)
  options <- .sql_precision_options(precision, bigint, datetime)
  precision <- options$precision
  bigint <- options$bigint
  datetime <- options$datetime
  binary <- match.arg(binary, c("raw", "base64"))
  unsupported <- match.arg(unsupported, c("error", "character"))
  request$resultFormat <- "columnar"
  request$batchRows <- as.integer(batch_rows)
  # Protocol v2 negotiates the wire kind for each SQL column. Naive timestamps
  # are binary ticks (T=UTC; W=local wall clock with DST resolution in R).
  request$columnarPrecision <- precision
  request$columnarDatetime <- datetime
  request$columnarTz <- tz
  con$stream_active <- TRUE
  on.exit(con$stream_active <- FALSE, add=TRUE)
  rss_before <- .sql_memory_rss()
  r_query_rss_peak <- rss_before
  sample_rss <- function() {
    rss <- .sql_memory_rss()
    if (is.finite(rss))
      r_query_rss_peak <<- if (is.finite(r_query_rss_peak)) max(r_query_rss_peak, rss) else rss
    invisible(NULL)
  }
  deadline <- Sys.time() + timeout
  t_total <- proc.time()[["elapsed"]]
  completed <- FALSE
  on.exit(if (!completed) .sql_invalidate(con), add = TRUE)
  t0 <- proc.time()[["elapsed"]]
  command <- jsonlite::toJSON(request, auto_unbox = TRUE, null = "null", digits = NA)
  serialize_seconds <- proc.time()[["elapsed"]] - t0
  t0 <- proc.time()[["elapsed"]]
  .sql_send(con, command, deadline)
  send_seconds <- proc.time()[["elapsed"]] - t0
  t0 <- proc.time()[["elapsed"]]
  start <- .sql_read_query_start(con, deadline, function() completed <<- TRUE)
  wait_start_seconds <- proc.time()[["elapsed"]] - t0
  if (!identical(start$kind, "start") || !identical(start$valueEncoding, "columnar-v2"))
    stop("Helper did not negotiate columnar-v2; rebuild the C# helper.", call. = FALSE)
  types <- unlist(start$types, use.names = FALSE)
  column_names <- unlist(start$columns, use.names = FALSE)
  if (length(types) != length(column_names) || length(types) > 4096L)
    stop("Invalid columnar schema.", call. = FALSE)
  if (as.integer(start$batchRows) != as.integer(batch_rows))
    stop("Helper did not accept batch_rows.", call. = FALSE)
  expected_kinds <- .sql_wire_kind(types, precision, datetime, tz)
  if (!identical(unlist(start$wireKinds, use.names=FALSE), expected_kinds))
    stop("Columnar-v2 wire schema differs from the requested precision/timezone.", call.=FALSE)
  # Geometric growth avoids repeated reallocation of the batch list.
  batches <- if (is.null(callback)) vector("list", 32L) else NULL
  callback_seconds <- 0
  type_time_stream <- numeric(0)
  type_convert_stream <- 0
  dst_states <- if (identical(dst_fallback, "ordered"))
    new.env(parent=emptyenv()) else NULL
  n_batches <- 0L
  n_bytes <- 0
  rows_received <- 0
  read_stream_seconds <- 0
  native_decode_seconds <- 0
  repeat {
    t0 <- proc.time()[["elapsed"]]
    reply <- .sql_read_reply(con, deadline, allow_data = TRUE)
    read_stream_seconds <- read_stream_seconds + proc.time()[["elapsed"]] - t0
    if (identical(reply$kind, "end")) break
    if (!identical(reply$kind, "chunk")) stop("Unexpected columnar stream message.", call. = FALSE)
    n_bytes <- n_bytes + length(reply$data)
    t0 <- proc.time()[["elapsed"]]
    decoded <- .Call("sqlcpp_decode_batch", reply$data, bigint == "integer64", tz, PACKAGE="RSqlClientNET")
    native_decode_seconds <- native_decode_seconds + proc.time()[["elapsed"]] - t0
    if (length(decoded$columns) != length(types) ||
        !identical(as.character(decoded$kinds), expected_kinds) ||
        decoded$rows > batch_rows)
      stop("SCB2 batch schema mismatch.", call. = FALSE)
    .sql_warn_decoded_bigint_min(decoded, column_names, row_offset = rows_received)
    n_batches <- n_batches + 1L
    rows_received <- rows_received + decoded$rows
    if (is.null(callback)) {
      if (n_batches > length(batches)) length(batches) <- max(1L, 2L * length(batches))
      batches[[n_batches]] <- data.table::as.data.table(decoded$columns)
    } else if (decoded$rows > 0L) {
      batch <- data.table::as.data.table(decoded$columns)
      data.table::setnames(batch, column_names)
      tc <- proc.time()[["elapsed"]]
      cast <- .sql_columnar_cast(batch, types, expected_kinds, bigint,
          datetime, binary, unsupported, precision, tz, datetime_parser,
          row_offset = rows_received - decoded$rows,
          dst_fallback = dst_fallback, dst_states = dst_states)
      batch <- cast$data
      type_convert_stream <- type_convert_stream + proc.time()[["elapsed"]] - tc
      for (nm in names(cast$type_seconds)) {
        if (!nm %in% names(type_time_stream)) type_time_stream[nm] <- 0
        type_time_stream[nm] <- type_time_stream[nm] + cast$type_seconds[[nm]]
      }
      attr(batch, "sqlclient_schema") <- data.frame(index=seq_along(types),
           name=column_names, sql_type=types, stringsAsFactors=FALSE)
      tc <- proc.time()[["elapsed"]]
      callback(batch)
      callback_seconds <- callback_seconds + proc.time()[["elapsed"]] - tc
      rm(batch, cast)
    }
    if (n_batches %% 8L == 0L) sample_rss()
    rm(decoded, reply)
  }
  if (n_batches == 0L || rows_received != as.numeric(reply$rows) ||
      n_batches != as.numeric(reply$chunksSent) ||
      n_bytes != as.numeric(reply$bytesSent))
    stop("Incomplete columnar stream: count/byte mismatch.", call. = FALSE)
  sample_rss()
  t0 <- proc.time()[["elapsed"]]
  if (is.null(callback)) {
    batches <- batches[seq_len(n_batches)]
    dt <- data.table::rbindlist(batches, use.names=FALSE, fill=FALSE)
    data.table::setnames(dt, column_names)
    assemble_seconds <- proc.time()[["elapsed"]] - t0
    rm(batches)
    sample_rss()
  } else {
    dt <- NULL
    assemble_seconds <- 0
  }
  t0 <- proc.time()[["elapsed"]]
  # Time remaining after native decoding, by SQL type. This does not include
  # C++ decoding; the latter is measured by native_decode_seconds.
  if (is.null(callback)) {
    cast <- .sql_columnar_cast(dt, types, expected_kinds, bigint, datetime,
        binary, unsupported, precision, tz, datetime_parser,
        dst_fallback = dst_fallback, dst_states = dst_states)
    dt <- cast$data
    type_time <- cast$type_seconds
    conversion_seconds <- proc.time()[["elapsed"]] - t0
  } else {
    type_time <- type_time_stream
    conversion_seconds <- type_convert_stream
  }
  type_counts <- table(tolower(types))
  sample_rss()
  type_timings <- data.frame(sql_type=names(type_counts),
     columns=as.integer(type_counts),
     post_decode_seconds=as.numeric(type_time[names(type_counts)]),
     stringsAsFactors=FALSE)
  type_timings$post_decode_seconds[is.na(type_timings$post_decode_seconds)] <- 0
  out <- if (is.null(callback)) dt else structure(list(
    rows_received=rows_received, batches_received=n_batches,
    bytes_received=n_bytes, callback_seconds=callback_seconds),
    class="sqlclient_stream_summary")
  attr(out, "sqlclient_schema") <- data.frame(index=seq_along(types),
      name=column_names, sql_type=types, stringsAsFactors=FALSE)
  attr(out, "sqlclient_type_timings") <- type_timings
  attr(out, "sqlclient_timings") <- list(
      execute_seconds=reply$executeSeconds,
      fetch_and_stream_seconds=reply$fetchAndStreamSeconds,
      serialize_seconds=serialize_seconds,
      send_seconds=send_seconds,
      wait_start_seconds=wait_start_seconds,
      read_stream_seconds=read_stream_seconds,
      base64_decode_seconds=0,
      native_decode_seconds=native_decode_seconds,
      assemble_raw_seconds=0,
      utf8_conversion_seconds=0,
      assemble_columns_seconds=assemble_seconds,
      parse_seconds=0,
      quote_correction_seconds=0,
      type_conversion_seconds=conversion_seconds,
      import_r_seconds=native_decode_seconds + assemble_seconds + conversion_seconds,
      chunk_kib=NA_integer_,
      batch_rows=as.integer(batch_rows),
      transport="rcpp",
      result_format="columnar",
      chunks_received=n_batches,
      bytes_received=n_bytes,
      helper_peak_working_set_bytes=reply$helperPeakWorkingSetBytes %||% NA_real_,
      helper_query_peak_rss_bytes=reply$helperQueryPeakWorkingSetBytes %||% NA_real_,
      helper_query_peak_private_bytes=reply$helperQueryPeakPrivateBytes %||% NA_real_,
      helper_rss_before_bytes=reply$helperStartWorkingSetBytes %||% NA_real_,
      r_rss_before_bytes=rss_before,
      r_query_sampled_peak_rss_bytes=r_query_rss_peak,
      r_rss_after_bytes=.sql_memory_rss(),
      stream_callback_seconds=callback_seconds,
      parser=NA_character_,
      bigint=bigint, datetime=datetime, binary=binary,
      precision=precision, tz=tz, datetime_parser=datetime_parser,
      quote_correction=FALSE,
      total_seconds=proc.time()[["elapsed"]] - t_total)
  completed <- TRUE
  out
}

#' Query SQL Server and return a data.table without writing result files
#' @param parser "fread" (default) or "read.csv". Transport is chosen
#'   at sql_connect() and is reported by sql_timings().
#' @param quote_correction Undo CSV doubled quotes after fread() (default TRUE).
#' @param chunk_kib Transport chunk size in KiB, 8..1024 (default 256).
#' @param precision "full" (default) keeps exact decimal and timestamp strings;
#'   "native" converts decimal/money to numeric, SQL timestamps to POSIXct
#'   (local timezone for datetime/datetime2/smalldatetime; UTC for datetimeoffset),
#'   SQL time to difftime, and bigint to bit64::integer64. Native conversions
#'   may round values. Existing bigint= and datetime= options override modes.
#' @param tz Time zone for SQL datetime/datetime2/smalldatetime converted to
#'   POSIXct. Default "" is R's local time zone; use an IANA identifier like
#'   "Europe/Copenhagen" or "UTC" to override. Does not change datetimeoffset.
#' @param datetime_parser "lubridate" (default, fast) or "base". Used only
#'   when SQL datetime/datetime2/smalldatetime/datetimeoffset is converted to
#'   POSIXct. Nonexistent local times (DST gaps) are converted to NA with a
#'   warning, regardless of parser; precision="full" retains original text.
#' @param dst_fallback "na" (default) converts ambiguous local fall-back
#'   times to NA with a warning; "ordered" assumes chronologically ordered
#'   SQL rows, uses the first occurrence as daylight time, and switches to
#'   standard time when the local clock goes backwards. Still warns.
#'   Always provide ORDER BY for queries relying on this policy.
#' @param result_format NULL (default) uses sql_default_format(), initially
#'   "columnar"; explicitly choose "csv" for the diagnostic text path.
#'   Both formats use the native Rcpp pipe.
#' @param batch_rows Rows per columnar binary batch (default 32768).
#' @export
#' @noRd
sql_query <- function(con, sql, params = list(), timeout = 600L,
                      parser = c("fread", "read.csv"), quote_correction = TRUE,
                      chunk_kib = 256L, bigint = NULL,
                      datetime = NULL, binary = "raw", unsupported = "error",
                      precision = c("full", "native"), tz = "",
                      datetime_parser = c("lubridate", "base"),
                      dst_fallback = "na", result_format = NULL,
                      batch_rows = 32768L) {
  con <- .sql_handle(con, "sql_query()")
  .sql_check_not_busy(con)
  if (isTRUE(con$stream_active))
    stop("Cannot issue SQL on a connection while its streaming callback is running.", call.=FALSE)
  if (isTRUE(con$dbi_cursor_active))
    stop("Clear active DBI cursor before querying.", call.=FALSE)
  tz <- .sql_validate_tz(tz)
  datetime_parser <- match.arg(datetime_parser)
  dst_fallback <- .sql_dst_fallback(dst_fallback)
  result_format <- .sql_result_format(result_format, con$transport)
  t0 <- proc.time()[["elapsed"]]
  parser <- match.arg(parser)
  precision <- match.arg(precision)
  stopifnot(is.character(sql), length(sql) == 1L, !is.na(sql))
  if (length(params) && (is.null(names(params)) || any(!nzchar(names(params)))))
    stop("params must be a named list.", call. = FALSE)
  request <- list(op = "query", sql = sql, params = .sql_encode_params(params),
                  timeout = as.integer(timeout))
  if (identical(result_format, "columnar")) {
    dt <- .sql_columnar_query(con, request, timeout=timeout, batch_rows=batch_rows,
               bigint=bigint, datetime=datetime, binary=binary,
               unsupported=unsupported, precision=precision, tz=tz,
               datetime_parser=datetime_parser, dst_fallback=dst_fallback)
  } else {
    dt <- .sql_stream_query(con, request, timeout=timeout,
               parser=parser, quote_correction=quote_correction,
               chunk_kib=chunk_kib, bigint=bigint, datetime=datetime,
               binary=binary, unsupported=unsupported, precision=precision,
               tz=tz, datetime_parser=datetime_parser, dst_fallback=dst_fallback)
  }

  tm <- attr(dt, "sqlclient_timings", exact = TRUE)
  tm$total_seconds <- proc.time()[["elapsed"]] - t0
  attr(dt, "sqlclient_timings") <- tm
  dt
}

#' Stream SQL Server column batches to an R callback without assembling a result
#'
#' Each batch is a data.table with the same datatype/precision rules as
#' sql_query(result_format="columnar"). The callback executes synchronously;
#' the next batch is not read until it returns. A callback error or interrupt
#' closes the helper and invalidates the connection (no stale frames).
#' NEVER query on the same connection from inside the callback. Use a separate
#' connection if the callback needs to write data to SQL Server.
#' @param con Open transport="rcpp" connection.
#' @param sql SQL SELECT query.
#' @param callback Function(batch) invoked once per non-empty batch. Must be a
#'   synchronous function. It may save aggregate state in a separate R object.
#' @param params Named parameters, as for sql_query().
#' @param timeout Total query timeout in seconds INCLUDING callback time.
#' @param batch_rows C# binary batch size 1..32768 for SQL queries. Synthetic memory_test permits 65536.
#' @param ... Optional sql_query() conversion settings: precision, bigint,
#'   datetime, binary, unsupported, tz, datetime_parser, dst_fallback.
#' @return Summary with row/batch/byte counts; sql_timings() and sql_schema()
#'   work on the summary. SQL rows are never retained by this function.
#' @export
#' @noRd
sql_query_stream <- function(con, sql, callback, params = list(), timeout = NULL,
                              batch_rows = NULL, ...) {
  # DBI connections inherit conversion defaults configured at dbConnect().
  # A legacy connection retains the historic defaults of the direct API.
  dbi_options <- if (inherits(con, "SqlClientNETConnection")) con@handle$dbi_options else NULL
  con <- .sql_handle(con, "sql_query_stream()")
  .sql_check_not_busy(con)
  if (is.null(timeout)) timeout <- dbi_options$timeout %||% 600
  if (is.null(batch_rows)) batch_rows <- dbi_options$batch_rows %||% 32768L
  if (!is.function(callback)) stop("callback must be a function(batch).", call.=FALSE)
  if (!inherits(con, "sqlclient_connection") || !sql_is_valid(con) ||
      !identical(con$transport, "rcpp"))
    stop("sql_query_stream() requires a live transport='rcpp' connection.", call.=FALSE)
  if (isTRUE(con$stream_active))
    stop("Connection already has an active streamed result.", call.=FALSE)
  if (!is.character(sql) || length(sql)!=1L || is.na(sql))
    stop("sql must be a single character string.", call.=FALSE)
  if (length(params) && (is.null(names(params)) || any(!nzchar(names(params)))))
    stop("params must be a named list.", call.=FALSE)
  dots <- list(...)
  allowed <- c("precision", "bigint", "datetime", "binary", "unsupported",
               "tz", "datetime_parser", "dst_fallback")
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))) ||
                      any(!names(dots) %in% allowed) || anyDuplicated(names(dots))))
    stop("Unsupported or duplicate setting in ...", call.=FALSE)
  if (!is.null(dbi_options)) {
    defaults <- dbi_options[c("precision", "bigint", "datetime", "binary",
                              "unsupported", "tz", "datetime_parser",
                              "dst_fallback")]
    dots <- utils::modifyList(defaults, dots, keep.null=TRUE)
  }
  request <- list(op="query", sql=sql, params=.sql_encode_params(params),
                  timeout=as.integer(timeout))
  do.call(.sql_columnar_query, c(list(con=con, request=request, timeout=timeout,
       batch_rows=batch_rows, callback=callback), dots))
}

#' Exercise C# to R memory-only transport, without SQL Server
#' @param parser CSV parser ("fread" or "read.csv").
#' @param quote_correction Whether to correct escaped quotes with fread.
#' @param chunk_kib Chunk size in KiB; default 256.
#' @param n_rows Number of synthetic test rows; use e.g. 5000 to test multiple chunks.
#' @param precision "full" (default) or "native", as in sql_query().
#' @param tz Time zone for timezone-naive SQL timestamps converted to POSIXct;
#'   default "" uses the R session's local time zone.
#' @param datetime_parser "lubridate" (default) or "base" for POSIXct conversion.
#' @param transport Only "rcpp" is supported.
#' @keywords internal
#' @noRd
sql_memory_test <- function(parser = c("fread", "read.csv"),
                            quote_correction = TRUE, chunk_kib = 256L,
                            n_rows = 2L, bigint = NULL, datetime = NULL,
                            binary = "raw", unsupported = "error",
                            precision = c("full", "native"), tz = "",
                            datetime_parser = c("lubridate", "base"),
                            transport = "rcpp",
                            result_format = NULL, batch_rows = 32768L) {
  tz <- .sql_validate_tz(tz)
  datetime_parser <- match.arg(datetime_parser)
  parser <- match.arg(parser)
  if (!is.numeric(n_rows) || length(n_rows) != 1L ||
      is.na(n_rows) || !is.finite(n_rows) || n_rows != floor(n_rows) ||
      n_rows < 2 || n_rows > 100000)
    stop("n_rows must be a whole number between 2 and 100000.", call. = FALSE)
  transport <- .sql_transport(transport)
  result_format <- .sql_result_format(result_format, transport)
  con <- .sql_start_helper(transport)
  on.exit(sql_disconnect(con), add = TRUE)
  request <- list(op = "memory_test", nRows = as.integer(n_rows))
  if (identical(result_format, "columnar"))
    return(.sql_columnar_query(con, request, timeout=60L, batch_rows=batch_rows,
                    bigint=bigint, datetime=datetime, binary=binary,
                    unsupported=unsupported, precision=match.arg(precision),
                    tz=tz, datetime_parser=datetime_parser))
  .sql_stream_query(con, request,
                    timeout = 60L, parser = parser,
                    quote_correction = quote_correction, chunk_kib = chunk_kib,
                    bigint = bigint, datetime = datetime, binary = binary,
                    unsupported = unsupported, precision = match.arg(precision),
                    tz = tz, datetime_parser = datetime_parser)
}

#' Test SQL datatype mapping without contacting SQL Server
#' @param parser CSV parser.
#' @param bigint "integer64" (default) or exact "character".
#' @param datetime "character" (exact, default) or "POSIXct" (may lose 100 ns precision).
#' @param binary "raw" (list of raw vectors, default) or "base64" (character).
#' @param precision "full" (default) or "native", as in sql_query().
#' @param tz Time zone for timezone-naive SQL timestamps converted to POSIXct;
#'   default "" uses the R session's local time zone.
#' @param datetime_parser "lubridate" (default) or "base" for POSIXct conversion.
#' @param transport Only "rcpp" is supported.
#' @keywords internal
#' @noRd
sql_types_test <- function(parser = c("fread", "read.csv"),
                           quote_correction = TRUE, chunk_kib = 256L,
                           bigint = NULL, datetime = NULL,
                           binary = "raw", precision = c("full", "native"),
                           tz = "", datetime_parser = c("lubridate", "base"),
                           transport = "rcpp",
                           result_format = NULL, batch_rows = 32768L) {
  tz <- .sql_validate_tz(tz)
  datetime_parser <- match.arg(datetime_parser)
  transport <- .sql_transport(transport)
  result_format <- .sql_result_format(result_format, transport)
  con <- .sql_start_helper(transport)
  on.exit(sql_disconnect(con), add = TRUE)
  if (identical(result_format, "columnar"))
    return(.sql_columnar_query(con, list(op="types_test"), timeout=60L,
                    batch_rows=batch_rows, bigint=bigint, datetime=datetime,
                    binary=binary, precision=match.arg(precision), tz=tz,
                    datetime_parser=datetime_parser))
  .sql_stream_query(con, list(op = "types_test"), timeout = 60L,
                    parser = match.arg(parser), quote_correction = quote_correction,
                    chunk_kib = chunk_kib, bigint = bigint,
                    datetime = datetime, binary = binary,
                    precision = match.arg(precision), tz = tz,
                    datetime_parser = datetime_parser)
}

#' Test adversarial SQL type values with no SQL Server connection
#' @param ... Formatting arguments accepted by sql_types_test().
#' @return A data.table containing exact numeric limits, DST wall times,
#'   Unicode, NULL, and raw binary edge values.
#' @export
#' @noRd
# Exact lookup matters here: R's list `$` operator performs partial matching.
# opts$datetime would silently match `datetime_parser` if only the latter was
# supplied, wrongly passing `"lubridate"` or `"base"` as the datetime format.
.sql_edge_fixture_options <- function(opts, result_format = NULL) {
  if (!is.list(opts)) stop("opts must be a list.", call. = FALSE)
  if (length(opts) && (is.null(names(opts)) || any(!nzchar(names(opts)))))
    stop("All sql_edge_types_test(...) arguments must be named.", call. = FALSE)
  if (anyDuplicated(names(opts)))
    stop("Duplicate sql_edge_types_test(...) argument names.", call. = FALSE)
  unknown <- setdiff(names(opts), names(formals(sql_types_test)))
  if (length(unknown))
    stop("Unknown arguments: ", paste(unknown, collapse = ", "), call. = FALSE)
  arg <- function(name, default = NULL) {
    # `[[` is exact (unlike `$`), and a supplied NULL uses the default.
    value <- opts[[name, exact = TRUE]]
    if (is.null(value)) default else value
  }
  parser <- match.arg(arg("parser", "fread"), c("fread", "read.csv"))
  transport <- .sql_transport(arg("transport", "rcpp"))
  format <- .sql_result_format(result_format, transport)
  tz <- .sql_validate_tz(arg("tz", "UTC"))
  precision <- match.arg(arg("precision", "full"), c("full", "native"))
  choice <- .sql_precision_options(
    precision,
    bigint = arg("bigint", "character"),
    datetime = arg("datetime", NULL)
  )
  list(
    parser = parser,
    transport = transport,
    result_format = format,
    tz = tz,
    precision = choice$precision,
    bigint = choice$bigint,
    datetime = choice$datetime,
    quote_correction = arg("quote_correction", TRUE),
    chunk_kib = arg("chunk_kib", 256L),
    batch_rows = arg("batch_rows", 32768L),
    binary = arg("binary", "raw"),
    datetime_parser = match.arg(arg("datetime_parser", "lubridate"),
                                c("lubridate", "base"))
  )
}

sql_edge_types_test <- function(..., result_format = NULL) {
  opts <- .sql_edge_fixture_options(list(...), result_format)
  con <- .sql_start_helper(opts$transport)
  on.exit(sql_disconnect(con), add = TRUE)
  if (identical(opts$result_format, "columnar"))
    return(.sql_columnar_query(con, list(op = "edge_types_test"), timeout = 60,
      batch_rows = opts$batch_rows,
      bigint = opts$bigint, datetime = opts$datetime,
      binary = opts$binary, precision = opts$precision, tz = opts$tz,
      datetime_parser = opts$datetime_parser))
  .sql_stream_query(con, list(op = "edge_types_test"), timeout = 60,
    parser = opts$parser, chunk_kib = opts$chunk_kib,
    quote_correction = opts$quote_correction,
    bigint = opts$bigint, datetime = opts$datetime,
    binary = opts$binary, precision = opts$precision, tz = opts$tz,
    datetime_parser = opts$datetime_parser)
}

#' Test error and partial-stream invalidation with a synthetic .NET helper
#' @param transport Only "rcpp" is supported.
#' @return A named list. No SQL Server is contacted.
#' @keywords internal
#' @noRd
sql_robustness_test <- function(transport = "rcpp") {
  transport <- .sql_transport(transport)
  errors <- character()
  for (kind in c("error", "partial")) {
    con <- .sql_start_helper(transport)
    msg <- tryCatch({
      if (kind == "error") .sql_rpc(con, list(op="fault_test", mode=kind), timeout=5)
      else .sql_stream_query(con, list(op="fault_test", mode=kind), timeout=5)
      NA_character_
    }, error=function(e) conditionMessage(e),
       finally={ if (sql_is_valid(con)) sql_cancel(con) })
    if (is.na(msg) || sql_is_valid(con)) stop("Fault did not invalidate helper: ", kind, call.=FALSE)
    errors <- c(errors, msg)
  }
  # A fresh helper must still operate after the failed processes have exited.
  pong <- sql_ping(transport=transport)
  stopifnot(isTRUE(pong$ok))
  list(ok=TRUE, transport=transport, errors=errors, fresh_helper_ok=TRUE)
}

#' Benchmark large SQL results and approximate R memory cost
#' @param con Open connection.
#' @param query_builder Function taking a row count and returning SQL text.
#' @param sizes Integer row counts; defaults to 5 and 10 million.
#' @param result_format Result encoding; defaults to sql_default_format().
#' @param ... Additional arguments to sql_query(), e.g. precision and tz.
#' @return data.frame with elapsed times, received bytes, result object size
#'   and R's GC-reported maximum vector heap (global process metric).
#'   Not a measure of .NET/SQL Server peak memory or resident set size.
#' @keywords internal
#' @noRd
sql_stress_test <- function(con, query_builder,
                            sizes = c(5000000L, 10000000L),
                            result_format = "columnar", ...) {
  if (!is.function(query_builder)) stop("query_builder must be a function(n).", call.=FALSE)
  if (!is.numeric(sizes) || !length(sizes) || anyNA(sizes) ||
      any(!is.finite(sizes)) || any(sizes < 1) || any(sizes != floor(sizes)))
    stop("sizes must contain positive whole numbers.", call.=FALSE)
  if (!sql_is_valid(con)) stop("Connection is not valid.", call.=FALSE)
  dots <- list(...)
  if (any(names(dots) %in% c("con", "sql", "result_format")))
    stop("Do not repeat con/sql/result_format in ...", call.=FALSE)
  out <- vector("list", length(sizes))
  for (i in seq_along(sizes)) {
    n <- sizes[[i]]
    gc(reset=TRUE)
    query <- query_builder(n)
    if (!is.character(query) || length(query)!=1L || is.na(query))
      stop("query_builder(n) must return one SQL string.", call.=FALSE)
    x <- do.call(sql_query, c(list(con=con, sql=query, result_format=result_format), dots))
    if (nrow(x) != n)
      stop(sprintf("Query requested %.0f rows but returned %.0f. Check query_builder().",
                   n, nrow(x)), call.=FALSE)
    tm <- sql_timings(x)
    heap <- gc()
    out[[i]] <- data.frame(rows_requested=n, rows_received=nrow(x),
      seconds=as.numeric(tm$total_seconds),
      bytes_received=as.numeric(tm$bytes_received),
      helper_peak_working_set_mb=as.numeric(tm$helper_peak_working_set_bytes)/1024^2,
      helper_query_sampled_peak_rss_mb=as.numeric(tm$helper_query_peak_rss_bytes)/1024^2,
      helper_query_sampled_peak_private_mb=as.numeric(tm$helper_query_peak_private_bytes)/1024^2,
      r_query_sampled_peak_rss_mb=as.numeric(tm$r_query_sampled_peak_rss_bytes)/1024^2,
      r_rss_before_mb=as.numeric(tm$r_rss_before_bytes)/1024^2,
      r_rss_after_mb=as.numeric(tm$r_rss_after_bytes)/1024^2,
      result_object_mb=as.numeric(utils::object.size(x))/1024^2,
      r_vcells_peak_mb=unname(as.numeric(heap["Vcells", "max used"]) * 8 / 1024^2),
      stringsAsFactors=FALSE)
    rm(x)
    gc()
  }
  do.call(rbind, out)
}

#' Benchmark batch streaming with bounded R result memory
#' @param con An open Rcpp SQL connection.
#' @param query_builder Function(n) returning SQL text for exactly n rows.
#' @param sizes Requested row counts.
#' @param callback A function(batch) to process each batch. The default counts
#'   rows without retaining them.
#' @param ... Conversion options and batch_rows/timeout for sql_query_stream().
#' @return data.frame with rows, seconds, transferred bytes and sampled RSS.
#' @keywords internal
#' @noRd
sql_stream_stress_test <- function(con, query_builder,
                                   sizes=c(5000000L, 10000000L),
                                   callback=function(batch) invisible(NULL), ...) {
  if (!is.function(query_builder)) stop("query_builder must be a function(n).", call.=FALSE)
  if (!is.function(callback)) stop("callback must be a function(batch).", call.=FALSE)
  if (!is.numeric(sizes) || !length(sizes) || anyNA(sizes) ||
      any(!is.finite(sizes)) || any(sizes < 1) || any(sizes != floor(sizes)))
    stop("sizes must contain positive whole numbers.", call.=FALSE)
  dots <- list(...)
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))) ||
                      any(names(dots) %in% c("con", "sql", "callback", "params")) ||
                      anyDuplicated(names(dots))))
    stop("Do not repeat con/sql/callback/params in ...", call.=FALSE)
  ans <- vector("list",length(sizes))
  for (i in seq_along(sizes)) {
    n <- sizes[[i]]
    gc()
    query <- query_builder(n)
    if (!is.character(query) || length(query) != 1L || is.na(query))
      stop("query_builder(n) must return one SQL string.", call.=FALSE)
    summary <- do.call(sql_query_stream, c(list(con=con, sql=query,
          callback=callback), dots))
    if (summary$rows_received != n)
      stop(sprintf("Query requested %.0f rows but streamed %.0f. Check query_builder().",
                   n, summary$rows_received), call.=FALSE)
    tm <- sql_timings(summary)
    ans[[i]] <- data.frame(rows_requested=n, rows_received=summary$rows_received,
      seconds=as.numeric(tm$total_seconds), bytes_received=as.numeric(tm$bytes_received),
      batches_received=summary$batches_received,
      r_query_sampled_peak_rss_mb=as.numeric(tm$r_query_sampled_peak_rss_bytes)/1024^2,
      helper_query_sampled_peak_rss_mb=as.numeric(tm$helper_query_peak_rss_bytes)/1024^2,
      callback_seconds=as.numeric(tm$stream_callback_seconds),
      stringsAsFactors=FALSE)
  }
  do.call(rbind,ans)
}

#' Inspect SQL Server column types attached to a query result
#' @export
#' @noRd
sql_schema <- function(x) {
  out <- attr(x, "sqlclient_schema", exact = TRUE)
  if (is.null(out)) stop("No SQL column schema is attached to this result.", call. = FALSE)
  out
}

#' Remaining R-side conversion time by SQL type for columnar results
#' @param x Data returned by sql_query().
#' @return data.frame with SQL type, number of columns and seconds spent
#'   converting after the native decoder. CSV entries are NA (not measured).
#'   Native decoding is timed separately
#'   under sql_timings(x)$native_decode_seconds.
#' @export
#' @noRd
sql_type_timings <- function(x) {
  ans <- attr(x, "sqlclient_type_timings", exact=TRUE)
  if (is.null(ans))
    stop("No SQL type timings attached to this result.", call.=FALSE)
  ans
}

#' Get the detailed timing metrics returned by sql_query or sql_memory_test
#' @param x Result from sql_query() or sql_memory_test().
#' @param format "list" (default) or "data.frame" (one row for comparing runs).
#' @return Named list or one-row data.frame of timing and stream statistics.
#' @export
#' @noRd
sql_timings <- function(x, format = c("list", "data.frame")) {
  format <- match.arg(format)
  tm <- attr(x, "sqlclient_timings", exact = TRUE)
  if (is.null(tm))
    stop("No sqlclient_timings attribute found. Supply a sql_query() or sql_memory_test() result.",
         call. = FALSE)
  if (identical(format, "data.frame"))
    return(as.data.frame(tm, stringsAsFactors = FALSE, optional = TRUE))
  tm
}

#' Execute DDL/DML and return affected row count
#' @export
#' @noRd
sql_execute <- function(con, sql, params = list(), timeout = 600L) {
  con <- .sql_handle(con, "sql_execute()")
  .sql_check_not_busy(con)
  stopifnot(is.character(sql), length(sql) == 1L, !is.na(sql))
  if (length(params) && (is.null(names(params)) || any(!nzchar(names(params)))))
    stop("params must be a named list.", call. = FALSE)
  out <- .sql_rpc(con, list(op = "execute", sql = sql, params = .sql_encode_params(params),
                            timeout = as.integer(timeout)), timeout = timeout)
  as.integer(out$affected)
}

#' Start an explicit SQL Server transaction on an existing connection
#' @param con An open RSqlClientNET connection.
#' @param isolation One of ReadCommitted, ReadUncommitted, RepeatableRead,
#'   Serializable or Snapshot (subject to SQL Server support).
#' @param timeout RPC timeout in seconds.
#' @return Invisible transaction status.
#' @export
#' @noRd
sql_begin <- function(con, isolation = "ReadCommitted", timeout = 30) {
  con <- .sql_handle(con, "sql_begin()")
  .sql_check_not_busy(con)
  isolation <- match.arg(isolation, c("ReadCommitted", "ReadUncommitted",
                                    "RepeatableRead", "Serializable", "Snapshot"))
  invisible(.sql_rpc(con, list(op="begin", isolation=isolation), timeout=timeout))
}

#' Commit the active transaction. A communication failure can leave an
#' unknown outcome at SQL Server: never automatically replay the transaction.
#' @param con Connection.
#' @param timeout RPC timeout in seconds.
#' @return Invisible server status.
#' @export
#' @noRd
sql_commit <- function(con, timeout = 30) {
  con <- .sql_handle(con, "sql_commit()")
  .sql_check_not_busy(con)
  invisible(.sql_rpc(con, list(op="commit"), timeout=timeout))
}

#' Roll back an explicit SQL transaction
#' @param con Connection.
#' @param timeout RPC timeout in seconds.
#' @return Invisible server status.
#' @export
#' @noRd
sql_rollback <- function(con, timeout = 30) {
  con <- .sql_handle(con, "sql_rollback()")
  .sql_check_not_busy(con)
  invisible(.sql_rpc(con, list(op="rollback"), timeout=timeout))
}

#' Reconnect *explicitly*, creating a NEW helper and SQL Server connection.
#' The caller must supply credentials again; no statement or transaction is
#' replayed, and no password or connection string is retained on the old con.
#' @param con Invalid/closed original connection.
#' @param ... Named arguments forwarded to sql_connect(), except transport.
#' @return A new RSqlClientNET connection.
#' @export
#' @noRd
sql_reconnect <- function(con, ...) {
  is_dbi <- inherits(con, "SqlClientNETConnection")
  h <- .sql_handle(con, "sql_reconnect()")
  if (sql_is_valid(h)) stop("Original connection is still valid; disconnect it first.", call.=FALSE)
  args <- list(...)
  if (length(args) && (is.null(names(args)) || any(!nzchar(names(args))) ||
                      anyDuplicated(names(args))))
    stop("Supply unique named connection arguments.", call.=FALSE)
  if ("transport" %in% names(args))
    stop("sql_reconnect() preserves the original transport; do not pass transport.", call.=FALSE)
  if (!is_dbi)
    return(do.call(sql_connect, c(args, list(transport=h$transport))))
  o <- h$dbi_options
  config_names <- c("batch_rows", "timeout", "precision", "bigint",
                    "datetime", "binary", "unsupported", "tz", "datetime_parser",
                    "result_format")
  if (any(names(args) %in% config_names))
    stop("sql_reconnect() preserves the DBI options; change them with a new dbConnect() instead.",
         call.=FALSE)
  dbi_args <- c(list(drv=SqlClientNET()), args, o[setdiff(config_names, "result_format")])
  do.call(DBI::dbConnect, dbi_args)
}

#' Close the SQL Server connection and helper process
#' @export
#' @noRd
sql_disconnect <- function(con) {
  if (inherits(con, "SqlClientNETConnection"))
    return(DBI::dbDisconnect(con))
  con <- .sql_handle(con, "sql_disconnect()")
  if (!isTRUE(con$closed)) {
    if (.sql_alive(con)) try(.sql_rpc(con, list(op="disconnect"), timeout=5L), silent=TRUE)
    .sql_native_close(con$native)
    con$closed <- TRUE
  }
  invisible(TRUE)
}

#' Check whether connection process is alive
#' @export
#' @noRd
sql_is_valid <- function(con) {
  if (inherits(con, "SqlClientNETConnection")) con <- con@handle
  inherits(con, "sqlclient_connection") && !isTRUE(con$closed) &&
    tryCatch(.sql_alive(con), error=function(e) FALSE)
}

#' Inspect helper process / connection status without contacting SQL Server
#' @param con A RSqlClientNET connection.
#' @return Named list. "alive" means the helper is running; it does NOT
#'   guarantee the SQL Server connection itself is still reachable.
#' @export
#' @noRd
sql_connection_status <- function(con) {
  con <- .sql_handle(con, "sql_connection_status()")
  alive <- if (isTRUE(con$closed)) FALSE else tryCatch(.sql_alive(con), error=function(e) FALSE)
  list(valid = !isTRUE(con$closed) && alive,
       helper_alive = alive, transport = con$transport,
       closed = isTRUE(con$closed),
       invalid_reason = con$invalid_reason %||% NA_character_)
}

#' @export
#' @noRd
print.sqlclient_connection <- function(x, ...) {
  cat("<RSqlClientNET connection>", if (sql_is_valid(x)) "open" else "closed", "\n")
  invisible(x)
}

#' Cancel outstanding work by terminating the .NET helper
#'
#' A hard cancellation immediately invalidates the connection. For a running
#' synchronous R query use Esc/Ctrl+C; the Rcpp I/O layer detects the interrupt
#' and closes the helper. Call sql_disconnect() afterwards if desired.
#' @param con An open RSqlClientNET connection.
#' @return Invisibly TRUE. The connection cannot be reused.
#' @export
#' @noRd
sql_cancel <- function(con) {
  con <- .sql_handle(con, "sql_cancel()")
  .sql_invalidate(con, "Cancelled by user")
  invisible(TRUE)
}

#' Test that a stuck helper can be interrupted by the native timeout
#'
#' Starts a disposable C# helper WITHOUT SQL Server, asks it to sleep,
#' and checks that the Rcpp bridge returns within a bounded time.
#' The temporary helper is killed on timeout; its connection cannot be reused.
#' @param timeout Timeout in seconds, default 0.25.
#' @param sleep_seconds Synthetic C# sleep duration, default 3.
#' @return A list with `timed_out`, `helper_still_alive`, and `elapsed_seconds`.
#' @keywords internal
#' @noRd
sql_timeout_test <- function(timeout = 0.25, sleep_seconds = 3) {
  stopifnot(is.numeric(timeout), length(timeout) == 1L,
            is.finite(timeout), timeout > 0,
            is.numeric(sleep_seconds), length(sleep_seconds) == 1L,
            is.finite(sleep_seconds), sleep_seconds > timeout, sleep_seconds <= 30)
  con <- .sql_start_helper("rcpp")
  on.exit(sql_cancel(con), add = TRUE)
  start <- proc.time()[["elapsed"]]
  timed_out <- FALSE
  error_text <- NULL
  tryCatch(
    .sql_rpc(con, list(op = "sleep_test", milliseconds = as.integer(sleep_seconds * 1000)),
             timeout = timeout),
    error = function(e) {
      error_text <<- conditionMessage(e)
      timed_out <<- grepl("timeout|Timed out", error_text, ignore.case = TRUE)
    }
  )
  duration <- proc.time()[["elapsed"]] - start
  list(timed_out = timed_out, helper_still_alive = .sql_alive(con),
       elapsed_seconds = duration, error = error_text)
}


#' Test cancellation of a partially streamed result without SQL Server
#'
#' The synthetic C# helper writes a query-start frame followed by one data
#' frame, then stalls. On deadline the entire helper is terminated and cannot
#' contaminate subsequent commands with the remaining frames.
#' @param timeout Seconds to wait for the complete stream (e.g. 0.25).
#' @param sleep_seconds How long the helper stalls (must exceed timeout).
#' @param transport Only "rcpp" is supported.
#' @keywords internal
#' @noRd
sql_stream_timeout_test <- function(timeout = 0.25, sleep_seconds = 3,
                                    transport = "rcpp") {
  stopifnot(is.numeric(timeout), length(timeout) == 1L,
            is.finite(timeout), timeout > 0,
            is.numeric(sleep_seconds), length(sleep_seconds) == 1L,
            is.finite(sleep_seconds), sleep_seconds > timeout,
            sleep_seconds <= 30)
  con <- .sql_start_helper(transport)
  on.exit(sql_cancel(con), add = TRUE)
  timed_out <- FALSE
  error_text <- NULL
  start <- proc.time()[["elapsed"]]
  tryCatch(
    .sql_stream_query(
      con, list(op = "stream_stall_test",
                milliseconds = as.integer(sleep_seconds * 1000)),
      timeout = timeout, precision = "full"),
    error = function(e) {
      error_text <<- conditionMessage(e)
      timed_out <<- grepl("timeout|Timed out", error_text, ignore.case = TRUE)
    }
  )
  list(timed_out = timed_out, helper_still_alive = .sql_alive(con),
       connection_valid = sql_is_valid(con),
       elapsed_seconds = proc.time()[["elapsed"]] - start,
       error = error_text)
}

#' Verify streamed integer IDs exactly without retaining SQL rows
#'
#' Verifies that the chosen ID column contains every integer from 1 through
#' expected_rows exactly once, independent of SQL result order. A native
#' bitmap uses ceiling(expected_rows / 8) bytes (1.25 MB for 10 million IDs).
#' It detects duplicate, missing, out-of-range, non-integer and NA IDs.
#' Only for consecutive numeric/integer IDs; use a SQL projection with ROW_NUMBER()
#' when source identifiers are not consecutive. ROW_NUMBER() must have a
#' deterministic ORDER BY when mapping arbitrary source keys.
#' @param con Live Rcpp SQL connection.
#' @param sql SQL SELECT statement with a column of integral IDs 1..expected_rows.
#' @param expected_rows Expected unique row/ID count (1..200 million).
#' @param id_column Name of integral integer/numeric ID column.
#' @param strict If TRUE, stop on failed integrity checks after the stream ends.
#'   A *completed* integrity failure does not invalidate the connection.
#' @param batch_rows Binary rows per frame, 1..32768 for SQL queries.
#' @param timeout Entire streaming deadline including callbacks.
#' @param ... Conversion arguments forwarded to sql_query_stream().
#' @return Report: unique, duplicated, missing and invalid IDs, example IDs,
#'   bitmap size, and sql_timings() on the report.
#' @keywords internal
#' @noRd
sql_stream_integrity_test <- function(con, sql, expected_rows,
                                      id_column = "test_id", strict = TRUE,
                                      batch_rows = 32768L, timeout = 600, ...) {
  if (!is.character(id_column) || length(id_column) != 1L ||
      is.na(id_column) || !nzchar(id_column))
    stop("id_column must be one non-empty column name.", call. = FALSE)
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict))
    stop("strict must be TRUE or FALSE.", call. = FALSE)
  bitmap <- .Call("sqlcpp_id_bitmap_new", as.numeric(expected_rows), PACKAGE="RSqlClientNET")
  callback <- function(batch) {
    if (!id_column %in% names(batch))
      stop("Integrity ID column not returned by SQL: ", id_column, call.=FALSE)
    .Call("sqlcpp_id_bitmap_add", bitmap, batch[[id_column]], PACKAGE="RSqlClientNET")
    invisible(NULL)
  }
  summary <- sql_query_stream(con, sql, callback=callback,
                 batch_rows=batch_rows, timeout=timeout, ...)
  report <- .Call("sqlcpp_id_bitmap_report", bitmap, PACKAGE="RSqlClientNET")
  report$rows_received <- summary$rows_received
  report$batches_received <- summary$batches_received
  report$ok <- isTRUE(report$ok) && identical(as.numeric(report$examined_ids),
                                               as.numeric(summary$rows_received))
  attr(report, "sqlclient_timings") <- attr(summary, "sqlclient_timings", exact=TRUE)
  attr(report, "sqlclient_schema") <- attr(summary, "sqlclient_schema", exact=TRUE)
  if (isTRUE(strict) && !isTRUE(report$ok))
    stop(sprintf(paste0("Stream ID integrity FAILED: missing=%.0f, ",
          "duplicate=%.0f, invalid=%.0f; received=%.0f, expected=%.0f."),
          report$missing_ids, report$duplicate_ids, report$invalid_ids,
          report$rows_received, report$expected_rows), call.=FALSE)
  report
}

#' Benchmark batch sizes using streaming without retaining result rows
#'
#' Repeats the same SQL at several binary batch sizes, alternating test order
#' between repetitions. The callback defaults to a no-op so this measures
#' transport and conversion, not user data processing. Use a stable query and
#' an otherwise idle system. Always includes a warm-up run (not reported).
#' @param con Live transport="rcpp" SQL connection.
#' @param sql SQL query; runs multiple times and must be safe to repeat.
#' @param batch_sizes Vector of distinct batch sizes between 1 and 32768.
#' @param repetitions Number of measured repetitions per batch size.
#' @param timeout Total timeout per query in seconds.
#' @param callback Synchronous function(batch), default discards batch.
#' @param ... Additional conversion options, e.g. precision, bigint and tz.
#' @return data.frame containing seconds, throughput, bytes and sampled RSS.
#' @keywords internal
#' @noRd
sql_batch_benchmark <- function(con, sql,
                                batch_sizes = c(8192L, 16384L, 32768L),
                                repetitions = 3L, timeout = 600,
                                callback = function(batch) invisible(NULL), ...) {
  if (!sql_is_valid(con) || !identical(con$transport, "rcpp"))
    stop("sql_batch_benchmark() requires a live Rcpp connection.", call.=FALSE)
  if (!is.character(sql) || length(sql) != 1L || is.na(sql) || !nzchar(sql))
    stop("sql must be one non-empty SQL statement.", call.=FALSE)
  if (!is.numeric(batch_sizes) || !length(batch_sizes) ||
      anyNA(batch_sizes) || any(!is.finite(batch_sizes)) ||
      any(batch_sizes != floor(batch_sizes)) || any(batch_sizes < 1) ||
      any(batch_sizes > 32768) || anyDuplicated(batch_sizes))
    stop("batch_sizes must contain distinct whole numbers 1..32768 in v0.0.27; 65536 is disabled for SQL queries after a Windows helper crash.", call.=FALSE)
  if (!is.numeric(repetitions) || length(repetitions)!=1L || is.na(repetitions) ||
      !is.finite(repetitions) || repetitions < 1 || repetitions > 30 ||
      repetitions != floor(repetitions))
    stop("repetitions must be a whole number between 1 and 30.", call.=FALSE)
  if (!is.function(callback)) stop("callback must be a function(batch).", call.=FALSE)
  dots <- list(...)
  if (length(dots) && (is.null(names(dots)) || any(!nzchar(names(dots))) ||
      anyDuplicated(names(dots)) ||
      any(names(dots) %in% c("con", "sql", "callback", "params", "timeout", "batch_rows"))))
    stop("Duplicate/unsupported arguments in ...", call.=FALSE)
  run_one <- function(size) do.call(sql_query_stream, c(
    list(con=con, sql=sql, batch_rows=as.integer(size), timeout=timeout,
         callback=callback), dots))
  invisible(run_one(batch_sizes[[1L]])) # warm-up
  out <- vector("list", length(batch_sizes) * repetitions)
  i <- 0L
  for (r in seq_len(repetitions)) {
    order <- if (r %% 2L == 1L) batch_sizes else rev(batch_sizes)
    for (bs in order) {
      gc()
      s <- run_one(bs)
      tm <- sql_timings(s)
      i <- i + 1L
      out[[i]] <- data.frame(
        repetition=r, batch_rows=as.integer(bs),
        rows_received=as.numeric(s$rows_received),
        batches_received=as.numeric(s$batches_received),
        total_seconds=as.numeric(tm$total_seconds),
        rows_per_second=as.numeric(s$rows_received) / as.numeric(tm$total_seconds),
        bytes_received=as.numeric(s$bytes_received),
        callback_seconds=as.numeric(s$callback_seconds),
        r_peak_rss_mb=as.numeric(tm$r_query_sampled_peak_rss_bytes) / 1024^2,
        helper_peak_rss_mb=as.numeric(tm$helper_query_peak_rss_bytes) / 1024^2,
        stringsAsFactors=FALSE)
    }
  }
  ans <- do.call(rbind, out)
  # Frame overhead differs with batch size; only equal row counts are required.
  if (length(unique(ans$rows_received)) != 1L)
    stop("Benchmark returned different row counts across batch sizes.", call.=FALSE)
  ans
}

#' Summarize streaming benchmark by batch size
#' @param benchmark data.frame returned by sql_batch_benchmark().
#' @return data.frame with medians of total seconds, rows/sec and RSS.
#' @keywords internal
#' @noRd
sql_batch_summary <- function(benchmark) {
  required <- c("batch_rows", "total_seconds", "rows_per_second",
                "r_peak_rss_mb", "helper_peak_rss_mb")
  if (!is.data.frame(benchmark) || !all(required %in% names(benchmark)))
    stop("Supply the result of sql_batch_benchmark().", call.=FALSE)
  med <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm=TRUE)
  ans <- do.call(rbind, lapply(split(benchmark, benchmark$batch_rows), function(d) {
    data.frame(batch_rows=d$batch_rows[[1L]], runs=nrow(d),
      median_seconds=med(d$total_seconds),
      median_rows_per_second=med(d$rows_per_second),
      median_r_peak_rss_mb=med(d$r_peak_rss_mb),
      median_helper_peak_rss_mb=med(d$helper_peak_rss_mb))
  }))
  rownames(ans) <- NULL
  ans[order(ans$batch_rows), , drop=FALSE]
}
