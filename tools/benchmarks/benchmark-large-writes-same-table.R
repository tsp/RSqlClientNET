# RC33 diagnostic benchmark: RSqlClientNET / SqlBulkCopy vs odbc writing
# precisely the SAME R fixture to precisely the SAME physical SQL Server table.
# Run from the package's project root. Only disposable, uniquely named tables
# are created/dropped. No user data is read or modified.
#
# SQL Server per-session wait/cpu snapshots and database-file IO deltas are
# OPTIONAL (requires DMV permissions on SQL Server 2022/2025). They are measured
# outside the upload timer; file IO is database-wide, NOT attributable to one
# writer when other activity occurs. DMV counters are not exact transaction-log
# byte counts; file writes, wait times and GC must not be added as wall time.
#
# Same .data.frame, one column, one table, one physical SQL type and identical
# NULL placement for both drivers. TRUNCATE + count/DATALENGTH verification
# outside the stopwatch. The odbc and RSqlClientNET append calls use separate
# connections (but the identical server/database/table), as they must.

.rscn_cmp_scalar <- function(x, name, lower, upper) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < lower || x > upper)
    stop(name, " must be a whole number between ", lower, " and ", upper,
         call. = FALSE)
  as.integer(x)
}
.rscn_cmp_optional_query <- function(con, sql) {
  tryCatch(DBI::dbGetQuery(con, sql), error = function(e) NULL)
}
.rscn_cmp_profile_sum <- function(messages, pattern) {
  lines <- grep("RSqlClientNET binary bulk batch", messages,
                value = TRUE, fixed = TRUE)
  if (!length(lines)) return(NA_real_)
  match <- regmatches(lines, regexec(pattern, lines, perl = TRUE))
  values <- vapply(match, function(x) if (length(x) > 1L)
    suppressWarnings(as.numeric(x[2L])) else NA_real_, numeric(1L))
  if (all(is.na(values))) NA_real_ else sum(values, na.rm = TRUE)
}
.rscn_cmp_metrics <- function(con, server_diagnostics = TRUE) {
  if (!server_diagnostics) return(list(session=NULL, waits=NULL, io=NULL))
  session <- .rscn_cmp_optional_query(con, paste(
    "SELECT CONVERT(float, cpu_time) AS cpu_ms,",
    "CONVERT(float, reads) AS physical_reads,",
    "CONVERT(float, writes) AS physical_writes,",
    "CONVERT(float, logical_reads) AS logical_reads",
    "FROM sys.dm_exec_sessions WHERE session_id=@@SPID"))
  waits <- .rscn_cmp_optional_query(con, paste(
    "SELECT wait_type, CONVERT(float, wait_time_ms) AS wait_ms,",
    "CONVERT(float, waiting_tasks_count) AS waits",
    "FROM sys.dm_exec_session_wait_stats WHERE session_id=@@SPID"))
  io <- .rscn_cmp_optional_query(con, paste(
    "SELECT f.file_id, f.type_desc,",
    "CONVERT(float, v.num_of_bytes_written) AS bytes_written,",
    "CONVERT(float, v.num_of_writes) AS writes,",
    "CONVERT(float, v.io_stall_write_ms) AS stall_write_ms",
    "FROM sys.dm_io_virtual_file_stats(DB_ID(), NULL) AS v",
    "INNER JOIN sys.database_files AS f ON f.file_id=v.file_id"))
  list(session=session, waits=waits, io=io)
}
.rscn_cmp_session_delta <- function(before, after) {
  names_out <- c("cpu_ms", "physical_reads", "physical_writes", "logical_reads")
  vals <- stats::setNames(rep(NA_real_, length(names_out)), names_out)
  if (!is.null(before) && !is.null(after) && nrow(before)==1L && nrow(after)==1L)
    for (k in names_out) vals[k] <- as.numeric(after[[k]][1L]-before[[k]][1L])
  vals
}
.rscn_cmp_io_delta <- function(before, after, type="LOG") {
  vals <- c(bytes_written=NA_real_, writes=NA_real_, stall_write_ms=NA_real_)
  if (is.null(before) || is.null(after) || !nrow(before) || !nrow(after))
    return(vals)
  before <- before[before$type_desc == type, , drop=FALSE]
  after <- after[after$type_desc == type, , drop=FALSE]
  if (!nrow(before) || nrow(before)!=nrow(after) ||
      !setequal(before$file_id, after$file_id)) return(vals)
  before <- before[match(after$file_id, before$file_id), , drop=FALSE]
  for (k in names(vals)) vals[k] <- sum(after[[k]]-before[[k]])
  vals
}
.rscn_cmp_wait_delta <- function(before, after, type, fraction, driver, repetition) {
  if (is.null(before) || is.null(after)) return(NULL)
  x <- merge(after, before, by="wait_type", all=TRUE, suffixes=c("_after", "_before"))
  if (!nrow(x)) return(NULL)
  for (name in c("wait_ms_after", "wait_ms_before", "waits_after", "waits_before"))
    x[[name]][is.na(x[[name]])] <- 0
  x$delta_wait_ms <- pmax(0, x$wait_ms_after - x$wait_ms_before)
  x$delta_waits <- pmax(0, x$waits_after - x$waits_before)
  x <- x[x$delta_wait_ms > 0 | x$delta_waits > 0,
         c("wait_type", "delta_wait_ms", "delta_waits"), drop=FALSE]
  if (!nrow(x)) return(NULL)
  cbind(data.frame(type=type, null_fraction=fraction, driver=driver,
                   repetition=repetition, stringsAsFactors=FALSE), x)
}

#' Compare RSqlClientNET and odbc writing large values into the same SQL table.
#'
#' @param n Rows per write. Five repetitions are recommended, using a disposable DB.
#' @param types One or more of varchar, unicode, binary, binary_unique.
#' @param null_fractions Fractions NULL, e.g. 0 and 0.5.
#' @param server_diagnostics Query optional SQL Server DMVs (permissions-dependent).
#' @param profile_separately An additional RSqlClientNET internal profile run,
#'   not included in timed results. This adds another large write per scenario.
#' @return List: summary, attempts, phases, waits, io, endpoint, diagnostics.
run_large_write_server_comparison <- function(
    n = 10000L, repetitions = 5L,
    types = c("varchar", "unicode", "binary"),
    null_fractions = 0,
    text_length = 10000L, binary_bytes = 8192L,
    batch_rows = 65536L,
    dsn = "SqlClientRTest",
    connection_string = if (exists("con_str", envir=.GlobalEnv, inherits=FALSE))
      get("con_str", envir=.GlobalEnv) else
        Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", ""),
    odbc_connection_string = Sys.getenv("ODBC_CONNECTION_STRING", ""),
    schema = "dbo", server_diagnostics = TRUE,
    profile_separately = TRUE, seed = 20261028L,
    output_dir = "tools/benchmarks/benchmark_results") {
  n <- .rscn_cmp_scalar(n, "n", 1, 100000)
  repetitions <- .rscn_cmp_scalar(repetitions, "repetitions", 1, 20)
  text_length <- .rscn_cmp_scalar(text_length, "text_length", 1, 100000)
  binary_bytes <- .rscn_cmp_scalar(binary_bytes, "binary_bytes", 1, 100000)
  batch_rows <- .rscn_cmp_scalar(batch_rows, "batch_rows", 1, 131072)
  seed <- .rscn_cmp_scalar(seed, "seed", 1, .Machine$integer.max)
  stopifnot(is.character(connection_string), length(connection_string)==1L,
    !is.na(connection_string), nzchar(connection_string),
    is.character(schema), length(schema)==1L, nzchar(schema),
    grepl("^[A-Za-z_][A-Za-z0-9_]*$", schema),
    is.character(output_dir), length(output_dir)==1L, nzchar(output_dir),
    is.character(types), length(types)>0L, !anyNA(types),
    all(types %in% c("varchar", "unicode", "binary", "binary_unique")),
    is.numeric(null_fractions), length(null_fractions)>0L,
    all(is.finite(null_fractions)), all(null_fractions>=0),
    all(null_fractions<1), is.logical(server_diagnostics),
    length(server_diagnostics)==1L, !is.na(server_diagnostics),
    is.logical(profile_separately), length(profile_separately)==1L,
    !is.na(profile_separately))
  for (pkg in c("DBI", "odbc", "RSqlClientNET"))
    if (!requireNamespace(pkg, quietly=TRUE)) stop("Install package: ", pkg)
  if (any(types %in% c("binary", "binary_unique")) &&
      !requireNamespace("blob", quietly=TRUE)) stop("Install package: blob")
  if (!file.exists("tools/benchmarks/benchmark_dbi.R"))
    stop("Run from the RSqlClientNET project root.")
  env <- new.env(parent=globalenv())
  sys.source("tools/benchmarks/benchmark_dbi.R", envir=env)

  old_options <- options(RSqlClientNET.bulk_profile=FALSE,
                         RSqlClientNET.bulk_alloc_profile=FALSE,
                         RSqlClientNET.bulk_profile_stages=FALSE)
  on.exit(options(old_options), add=TRUE)
  con_r <- con_o <- NULL
  owned <- character()
  on.exit({
    if (!is.null(con_r) && tryCatch(DBI::dbIsValid(con_r), error=function(e) FALSE)) {
      for (t in rev(owned)) try(DBI::dbExecute(con_r, paste0(
        "DROP TABLE IF EXISTS ",
        as.character(DBI::dbQuoteIdentifier(con_r, DBI::Id(schema=schema,table=t))))),
        silent=TRUE)
    }
    if (!is.null(con_o)) try(DBI::dbDisconnect(con_o), silent=TRUE)
    if (!is.null(con_r)) try(DBI::dbDisconnect(con_r), silent=TRUE)
  }, add=TRUE)
  con_r <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
                          connection_string=connection_string)
  if (nzchar(odbc_connection_string)) {
    con_o <- DBI::dbConnect(odbc::odbc(),
                            .connection_string=odbc_connection_string)
  } else con_o <- DBI::dbConnect(odbc::odbc(), dsn=dsn)
  connections <- list(RSqlClientNET=con_r, odbc=con_o)
  endpoint_sql <- paste(
    "SELECT DB_NAME() AS db,",
    "CONVERT(nvarchar(128), SERVERPROPERTY('ServerName')) AS server,",
    "@@SPID AS spid,",
    "CONVERT(nvarchar(256), @@VERSION) AS version")
  endpoints <- lapply(connections, DBI::dbGetQuery, statement=endpoint_sql)
  endpoint_key <- vapply(endpoints, function(x) paste(as.character(x$db[[1L]]),
    as.character(x$server[[1L]]), sep=" / "), character(1L))
  if (length(unique(endpoint_key)) != 1L)
    stop("Connections target DIFFERENT server/databases: ",
         paste(names(endpoint_key), endpoint_key, collapse=" ; "))
  cat("SQL Server/database: ", endpoint_key[[1L]], "\n", sep="")
  cat("SPIDs: ", paste(names(endpoints), vapply(endpoints,
    function(x) as.integer(x$spid[[1L]]), integer(1L)), collapse="; "), "\n", sep="")
  if (server_diagnostics) {
    warm <- .rscn_cmp_metrics(con_r, TRUE)
    if (is.null(warm$waits)) message(
      "Session wait DMV unavailable; waiting-time counters will be omitted.")
    if (is.null(warm$io)) message(
      "File IO DMV unavailable; log/data IO counters will be omitted.")
  }

  # Only affect local RNG state; reproducible randomized order.
  had_seed <- exists(".Random.seed", .GlobalEnv, inherits=FALSE)
  if (had_seed) prior_seed <- get(".Random.seed", .GlobalEnv)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", prior_seed, envir=.GlobalEnv)
    } else if (exists(".Random.seed", .GlobalEnv, inherits=FALSE)) {
      rm(".Random.seed", envir=.GlobalEnv)
    }
  }, add=TRUE)
  set.seed(seed)
  tag <- sprintf("rscn_io33_%d_%s", Sys.getpid(),
    gsub("[^0-9]", "", format(Sys.time(), "%y%m%d%H%M%OS6")))
  attempts <- list(); phases <- list(); waits <- list(); io <- list(); notes <- list()
  k <- 0L
  for (type in unique(types)) for (fraction in unique(null_fractions)) {
    # Do not silently exceed memory budget in fixture preparation.
    payload_guess <- as.double(n) * as.double(if (type %in% c("binary","binary_unique"))
      binary_bytes else if (type == "unicode") 4 * text_length else text_length)
    if (payload_guess > 700 * 1024^2)
      stop("Estimated fixture is >700 MiB; reduce rows or value length")
    dat <- env$.bench_make_data(list(type=type, n_rows=n,
      text_length=text_length, binary_bytes=binary_bytes,
      null_fraction=fraction), seed=17L)
    sql_type <- switch(type, varchar="VARCHAR(MAX)", unicode="NVARCHAR(MAX)",
      binary="VARBINARY(MAX)", binary_unique="VARBINARY(MAX)")
    k <- k + 1L
    tab <- sprintf("%s_%03d", tag, k)
    id <- DBI::Id(schema=schema, table=tab)
    if (DBI::dbExistsTable(con_r,id)) stop("Refusing existing table: ", tab)
    quoted <- as.character(DBI::dbQuoteIdentifier(con_r,id))
    owned <- c(owned,tab) # register cleanup before potentially failing CREATE
    DBI::dbExecute(con_r, paste0("CREATE TABLE ", quoted,
      " ([value] ", sql_type, " NULL)"))
    expected_present <- if (type %in% c("binary", "binary_unique"))
      sum(!vapply(unclass(dat$value), is.null, logical(1L))) else
      sum(!is.na(dat$value))
    cat(sprintf("\n%s, n=%d, NULL=%.0f%%, %s\n",
      type, n, fraction*100, sql_type))
    expected_totals <- NULL
    # Warm both drivers before the timed test. Pre-warm result validation too.
    run_one <- function(driver, rep, prof=FALSE) {
      con <- connections[[driver]]
      # TRUNCATE and snapshots are OUTSIDE the timed section.
      DBI::dbExecute(con_r, paste0("TRUNCATE TABLE ", quoted))
      before <- .rscn_cmp_metrics(con, server_diagnostics && rep>0L)
      options(RSqlClientNET.bulk_profile=prof,
              RSqlClientNET.bulk_alloc_profile=FALSE,
              RSqlClientNET.bulk_profile_stages=FALSE)
      msg <- character()
      tic <- proc.time()[["elapsed"]]
      error <- tryCatch({
        withCallingHandlers({
          if (driver == "RSqlClientNET") {
            DBI::dbAppendTable(con,id,dat,
              batch_rows=batch_rows, row.names=NULL, table_lock=FALSE)
          } else {
            DBI::dbAppendTable(con,id,dat, row.names=NULL)
          }
        }, message=function(m) {
          msg <<- c(msg, conditionMessage(m))
          invokeRestart("muffleMessage")
        })
        NULL
      }, error=function(e) conditionMessage(e))
      elapsed <- proc.time()[["elapsed"]] - tic
      options(RSqlClientNET.bulk_profile=FALSE)
      after <- .rscn_cmp_metrics(con, server_diagnostics && rep>0L)
      # Verify off-clock: count rows, non-NULL values and sum DATALENGTH.
      check <- tryCatch(DBI::dbGetQuery(con_r, paste0(
        "SELECT CONVERT(float, COUNT_BIG(*)) AS n, ",
        "CONVERT(float, COUNT_BIG([value])) AS nn, ",
        "CONVERT(float, COALESCE(SUM(CONVERT(bigint, DATALENGTH([value]))),0)) AS nb ",
        "FROM ", quoted)), error=function(e) NULL)
      total <- if (!is.null(check) && nrow(check)==1L) {
        as.numeric(unlist(check[1L,c("n","nn","nb")], use.names=FALSE))
      } else {
        rep(NA_real_,3L)
      }
      names(total) <- c("n","nn","nb")
      if (is.null(error)) {
        if (anyNA(total) || total[["n"]] != n ||
            total[["nn"]] != expected_present) {
          error <- "Row/NULL count mismatch"
        } else if (is.null(expected_totals)) {
          expected_totals <<- total
        } else if (!isTRUE(all.equal(total,expected_totals,tolerance=0))) {
          error <- "DATALENGTH/row/NULL sums differ from reference"
        }
      }
      row <- data.frame(type=type,null_fraction=fraction,driver=driver,
        repetition=rep, sql_type=sql_type, rows=n,
        elapsed_s=elapsed, rows_written=total[["n"]],
        nonnull_written=total[["nn"]], bytes_in_sql=total[["nb"]],
        status=if (is.null(error)) "ok" else "error",
        error=if (is.null(error)) "" else error,
        stringsAsFactors=FALSE)
      if (prof) {
        phases[[length(phases)+1L]] <<- data.frame(type=type,
          null_fraction=fraction,driver=driver,
          total_s=elapsed,
          r_encode_s=.rscn_cmp_profile_sum(msg,"R encode ([0-9.]+)s"),
          rpc_sql_s=.rscn_cmp_profile_sum(msg,"RPC \\+ SQL ([0-9.]+)s"),
          bulkcopy_s=.rscn_cmp_profile_sum(msg,"SqlBulkCopy ([0-9.]+)ms")/1000,
          pipe_send_s=.rscn_cmp_profile_sum(msg,"native pipe send ([0-9.]+)ms")/1000,
          stringsAsFactors=FALSE)
      }
      if (rep>0L) {
        sd <- .rscn_cmp_session_delta(before$session,after$session)
        ld <- .rscn_cmp_io_delta(before$io,after$io,"LOG")
        dd <- .rscn_cmp_io_delta(before$io,after$io,"ROWS")
        io[[length(io)+1L]] <<- cbind(row[c("type","null_fraction","driver","repetition")],
          data.frame(cpu_ms=unname(sd["cpu_ms"]),
            session_reads=unname(sd["physical_reads"]),
            session_writes=unname(sd["physical_writes"]),
            session_logical_reads=unname(sd["logical_reads"]),
            log_file_written_MB=unname(ld["bytes_written"])/1e6,
            log_file_writes=unname(ld["writes"]),
            log_file_stall_ms=unname(ld["stall_write_ms"]),
            data_file_written_MB=unname(dd["bytes_written"])/1e6,
            data_file_stall_ms=unname(dd["stall_write_ms"])))
        wa <- .rscn_cmp_wait_delta(before$waits,after$waits,
          type,fraction,driver,rep)
        if (!is.null(wa)) waits[[length(waits)+1L]] <<- wa
      }
      cat(sprintf("  %-13s rep %d: %.3f s [%s]\n", driver, rep,
          elapsed, if (is.null(error)) "OK" else error))
      if (!is.null(error)) stop("Benchmark failed: ", driver, ", ",error)
      invisible(row)
    }
    # Warm-up is not included in performance results, but still verifies data.
    for (driver in c("RSqlClientNET","odbc")) run_one(driver,-1L)
    if (profile_separately) run_one("RSqlClientNET",0L,prof=TRUE)
    for (rep in seq_len(repetitions)) {
      order <- if (rep %% 2L == 1L) {
        sample(c("RSqlClientNET","odbc"))
      } else {
        rev(previous_order)
      }
      previous_order <- order
      for (driver in order) attempts[[length(attempts)+1L]] <-
        run_one(driver,rep)
    }
    DBI::dbExecute(con_r, paste0("DROP TABLE ",quoted))
    owned <- setdiff(owned,tab)
    rm(dat)
    invisible(gc(verbose=FALSE))
  }
  attempts <- do.call(rbind,attempts)
  phases <- if (length(phases)) do.call(rbind,phases) else data.frame()
  waits <- if (length(waits)) do.call(rbind,waits) else data.frame()
  io <- if (length(io)) do.call(rbind,io) else data.frame()
  good <- attempts[attempts$status=="ok", ,drop=FALSE]
  summary <- stats::aggregate(elapsed_s ~ type + null_fraction + driver,
    data=good,FUN=median)
  names(summary)[names(summary)=="elapsed_s"] <- "median_s"
  ref <- setNames(summary$median_s[summary$driver=="odbc"],
    paste(summary$type[summary$driver=="odbc"],
          summary$null_fraction[summary$driver=="odbc"],sep="|"))
  summary$speedup_vs_odbc <- unname(ref[paste(summary$type,
    summary$null_fraction,sep="|")])/summary$median_s
  summary <- summary[order(summary$type,summary$null_fraction,summary$driver),]
  rownames(summary) <- NULL
  server_summary <- if (nrow(io)) {
    stats::aggregate(
      cbind(cpu_ms, log_file_written_MB, log_file_writes,
            log_file_stall_ms, data_file_written_MB) ~
        type + null_fraction + driver, data=io,
      FUN=function(x) if (all(is.na(x))) NA_real_ else median(x,na.rm=TRUE),
      na.action=stats::na.pass)
  } else data.frame()
  wait_summary <- if (nrow(waits)) {
    z <- stats::aggregate(delta_wait_ms ~ type + null_fraction + driver + wait_type,
                          data=waits,FUN=sum)
    z[order(z$type,z$null_fraction,z$driver,-z$delta_wait_ms), ,drop=FALSE]
  } else data.frame()
  print(summary,row.names=FALSE)
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(attempts,file.path(output_dir,"same-table-attempts.csv"),row.names=FALSE)
  utils::write.csv(summary,file.path(output_dir,"same-table-summary.csv"),row.names=FALSE)
  utils::write.csv(phases,file.path(output_dir,"same-table-phases.csv"),row.names=FALSE)
  utils::write.csv(waits,file.path(output_dir,"same-table-session-waits.csv"),row.names=FALSE)
  utils::write.csv(io,file.path(output_dir,"same-table-server-io.csv"),row.names=FALSE)
  utils::write.csv(server_summary,file.path(output_dir,"same-table-server-summary.csv"),row.names=FALSE)
  utils::write.csv(wait_summary,file.path(output_dir,"same-table-wait-summary.csv"),row.names=FALSE)
  invisible(list(summary=summary,attempts=attempts,phases=phases,
                 waits=waits,io=io,server_summary=server_summary,
                 wait_summary=wait_summary,endpoints=endpoints))
}
