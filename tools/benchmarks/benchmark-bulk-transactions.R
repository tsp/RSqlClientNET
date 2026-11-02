# SQL Server bulk-write transaction, TableLock and logical-log benchmark.
# Fix: ensure verification SQL inserts a space before FROM (SQL error 102).
# Run from the RSqlClientNET source root after devtools::load_all() or install().
# Creates/drops uniquely named disposable tables. NEVER uses production tables.
# Primary timing: 4 modes on exactly the same SQL table and fixture.
# Separately (outside performance summary): explicit DBI transaction +
# sys.dm_tran_database_transactions log usage BEFORE COMMIT for BOTH clients.
# Requires RC33 benchmark-large-writes-same-table.R for common diagnostics.
# SQL Server 2022+ DMVs generally need VIEW SERVER PERFORMANCE STATE;
# missing privileges cause NA diagnostics, not aborted benchmark runs.

.rscn34_number <- function(x, name, min, max) {
  if (length(x) != 1L || !is.numeric(x) || is.na(x) || !is.finite(x) ||
      x != floor(x) || x < min || x > max) {
    stop(name, " must be an integer in [", min, ", ", max, "].", call. = FALSE)
  }
  as.integer(x)
}

.rscn34_query_log <- function(con) {
  # Must be queried INSIDE an active explicit transaction and before commit.
  # An inaccessible DMV is reported as missing, not as zero log bytes.
  sql <- paste(
    "SELECT COUNT_BIG(*) AS transactions_found,",
    "CONVERT(float, SUM(d.database_transaction_log_bytes_used)) AS log_bytes_used,",
    "CONVERT(float, SUM(d.database_transaction_log_record_count)) AS log_records,",
    "CONVERT(float, SUM(d.database_transaction_log_bytes_reserved)) AS log_bytes_reserved",
    "FROM sys.dm_tran_session_transactions AS s",
    "JOIN sys.dm_tran_database_transactions AS d",
    "ON d.transaction_id = s.transaction_id",
    "WHERE s.session_id = @@SPID AND s.is_user_transaction = 1",
    "AND d.database_id = DB_ID()"
  )
  tryCatch({
    x <- DBI::dbGetQuery(con, sql)
    if (nrow(x) != 1L || is.na(x$transactions_found[1L]) ||
        as.numeric(x$transactions_found[1L]) < 1) {
      return(list(log_bytes=NA_real_, log_records=NA_real_,
                  reserved_bytes=NA_real_, reason="No matching active transaction"))
    }
    list(log_bytes=as.numeric(x$log_bytes_used[1L]),
         log_records=as.numeric(x$log_records[1L]),
         reserved_bytes=as.numeric(x$log_bytes_reserved[1L]), reason="")
  }, error=function(e) list(log_bytes=NA_real_, log_records=NA_real_,
                            reserved_bytes=NA_real_,
                            reason=paste0("DMV unavailable: ",conditionMessage(e))))
}

# Construct the verification SQL with paste(), not paste0().  In RC34 the
# missing space before FROM generated "AS bytesFROM [dbo]" (SQL error 102).
.rscn34_verify_sql <- function(quoted_table) {
  stopifnot(is.character(quoted_table), length(quoted_table) == 1L,
            !is.na(quoted_table), nzchar(quoted_table))
  paste(
    "SELECT CONVERT(float, COUNT_BIG(*)) AS n,",
    "CONVERT(float, COUNT_BIG([value])) AS nn,",
    "CONVERT(float, COALESCE(SUM(CONVERT(bigint, DATALENGTH([value]))), 0)) AS bytes",
    "FROM", quoted_table
  )
}

.rscn34_median <- function(x) if (all(is.na(x))) NA_real_ else stats::median(x, na.rm=TRUE)
.rscn34_bind <- function(x) if (length(x)) do.call(rbind,x) else data.frame()
.rscn34_write_csv <- function(x, dir, name) {
  utils::write.csv(x, file.path(dir, paste0("bulk-tx-",name,".csv")),row.names=FALSE)
}

#' Compare RSqlClientNET TableLock and odbc transaction behavior.
#'
#' @param n Number of rows per write; default 10000. For large run use 50000.
#' @param types One or more of varchar, unicode, binary, binary_unique.
#' @param repetitions Timed iterations for EACH of 4 modes.
#' @param logical_log TRUE runs separate diagnostic uploads with DBI transaction
#'   for BOTH drivers to measure transaction-specific log bytes before commit;
#'   these are excluded from performance summaries.
#' @param profile_separately TRUE runs extra RSqlClientNET stage profiles for
#'   explicit table_lock=FALSE and TRUE before the timed repetitions.
#' @return Invisible list with summary, attempts, phases, server_summary,
#'   wait_summary, logical_log, recovery_model and other diagnostic data.
run_bulk_transaction_benchmark <- function(
    n=10000L, repetitions=5L,
    types=c("varchar","unicode","binary"), null_fractions=0,
    text_length=10000L, binary_bytes=8192L, batch_rows=65536L,
    dsn="SqlClientRTest",
    connection_string=if (exists("con_str", .GlobalEnv, inherits=FALSE))
      get("con_str", .GlobalEnv) else Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING",""),
    odbc_connection_string=Sys.getenv("ODBC_CONNECTION_STRING",""),
    schema="dbo", server_diagnostics=TRUE, logical_log=TRUE,
    profile_separately=TRUE, seed=20261029L,
    output_dir="tools/benchmarks/benchmark_results") {
  n <- .rscn34_number(n,"n",1,100000)
  repetitions <- .rscn34_number(repetitions,"repetitions",1,20)
  text_length <- .rscn34_number(text_length,"text_length",1,100000)
  binary_bytes <- .rscn34_number(binary_bytes,"binary_bytes",1,100000)
  batch_rows <- .rscn34_number(batch_rows,"batch_rows",1,131072)
  seed <- .rscn34_number(seed,"seed",1,.Machine$integer.max)
  stopifnot(is.character(types),length(types)>0L,!anyNA(types),
    all(types %in% c("varchar","unicode","binary","binary_unique")),
    is.numeric(null_fractions),length(null_fractions)>0L,
    all(is.finite(null_fractions)),all(null_fractions>=0),all(null_fractions<1),
    is.character(schema),length(schema)==1L,grepl("^[A-Za-z_][A-Za-z0-9_]*$",schema),
    is.character(connection_string),length(connection_string)==1L,
    !is.na(connection_string),nzchar(connection_string),
    is.character(odbc_connection_string),length(odbc_connection_string)==1L,
    is.character(dsn),length(dsn)==1L,nzchar(dsn),
    is.character(output_dir),length(output_dir)==1L,nzchar(output_dir),
    is.logical(server_diagnostics),length(server_diagnostics)==1L,!is.na(server_diagnostics),
    is.logical(logical_log),length(logical_log)==1L,!is.na(logical_log),
    is.logical(profile_separately),length(profile_separately)==1L,!is.na(profile_separately))
  for (pkg in c("DBI","odbc","RSqlClientNET")) {
    if (!requireNamespace(pkg,quietly=TRUE)) stop("Install package: ",pkg)
  }
  if (any(types %in% c("binary","binary_unique")) &&
      !requireNamespace("blob",quietly=TRUE)) stop("Install package: blob")
  base_file <- "tools/benchmarks/benchmark-large-writes-same-table.R"
  fixture_file <- "tools/benchmarks/benchmark_dbi.R"
  if (!file.exists(base_file) || !file.exists(fixture_file)) {
    stop("Run from the RSqlClientNET source root (RC33/RC34 benchmark files are required).")
  }
  common <- new.env(parent=globalenv())
  sys.source(base_file,envir=common)
  fixtures <- new.env(parent=globalenv())
  sys.source(fixture_file,envir=fixtures)

  oldopts <- options(RSqlClientNET.bulk_profile=FALSE,
                     RSqlClientNET.bulk_alloc_profile=FALSE,
                     RSqlClientNET.bulk_profile_stages=FALSE)
  on.exit(options(oldopts),add=TRUE)
  had_seed <- exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE)
  if (had_seed) previous_seed <- get(".Random.seed",envir=.GlobalEnv)
  on.exit({
    if (had_seed) assign(".Random.seed",previous_seed,envir=.GlobalEnv)
    else if (exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
      rm(".Random.seed",envir=.GlobalEnv)
  },add=TRUE)
  set.seed(seed)

  con_r <- con_o <- NULL
  owned <- character()
  on.exit({
    if (!is.null(con_r) && tryCatch(DBI::dbIsValid(con_r),error=function(e) FALSE)) {
      for (tbl in rev(owned)) try(DBI::dbExecute(con_r,paste0("DROP TABLE IF EXISTS ",
        as.character(DBI::dbQuoteIdentifier(con_r,DBI::Id(schema=schema,table=tbl))))),
        silent=TRUE)
    }
    if (!is.null(con_o)) try(DBI::dbDisconnect(con_o),silent=TRUE)
    if (!is.null(con_r)) try(DBI::dbDisconnect(con_r),silent=TRUE)
  },add=TRUE)
  con_r <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
                          connection_string=connection_string)
  con_o <- if (nzchar(odbc_connection_string))
    DBI::dbConnect(odbc::odbc(),.connection_string=odbc_connection_string)
  else DBI::dbConnect(odbc::odbc(),dsn=dsn)
  connections <- list(RSqlClientNET=con_r,odbc=con_o)
  endpoint_sql <- paste("SELECT DB_NAME() AS db,",
    "CONVERT(nvarchar(128),SERVERPROPERTY('ServerName')) AS server,",
    "@@SPID AS spid,",
    "CONVERT(nvarchar(256),@@VERSION) AS version")
  endpoints <- lapply(connections,DBI::dbGetQuery,statement=endpoint_sql)
  signature <- vapply(endpoints,function(x) paste(x$server[1L],x$db[1L],sep=" / "),character(1L))
  if (length(unique(signature))!=1L) stop("Driver connections differ: ",
    paste(names(signature),signature,collapse=" ; "))
  cat("Server / database: ",signature[1L],"\n",sep="")
  cat("SPIDs: ",paste(names(endpoints),vapply(endpoints,
      function(x) as.integer(x$spid[[1L]]),integer(1L)),collapse="; "),"\n",sep="")
  recovery_model <- tryCatch(DBI::dbGetQuery(con_r,paste(
    "SELECT name, recovery_model_desc, log_reuse_wait_desc,",
    "compatibility_level, is_read_committed_snapshot_on",
    "FROM sys.databases WHERE database_id=DB_ID()")),error=function(e) {
      message("Cannot read recovery model: ",conditionMessage(e)); data.frame()
    })
  if (nrow(recovery_model)) cat("Recovery model: ",
    recovery_model$recovery_model_desc[1L],"\n",sep="")
  if (server_diagnostics) {
    probe <- common$.rscn_cmp_metrics(con_r,TRUE)
    if (is.null(probe$waits)) message("Session waits DMV inaccessible; continue with NA.")
    if (is.null(probe$io)) message("File IO DMV inaccessible; continue with NA.")
  }

  mode_info <- data.frame(
    mode=c("rsql_standard","rsql_table_lock","odbc_default","odbc_transaction"),
    driver=c("RSqlClientNET","RSqlClientNET","odbc","odbc"),
    table_lock=c(FALSE,TRUE,FALSE,FALSE),
    explicit_tx=c(FALSE,FALSE,FALSE,TRUE),stringsAsFactors=FALSE)
  attempts <- list(); phases <- list(); io <- list(); waits <- list(); logical <- list()
  stamp <- paste0(Sys.getpid(),"_",gsub("[^0-9]","",format(Sys.time(),"%y%m%d%H%M%OS6")))
  counter <- 0L
  for (type in unique(types)) for (fraction in unique(null_fractions)) {
    estimated <- as.double(n)*as.double(if(type %in% c("binary","binary_unique"))
      binary_bytes else if(type=="unicode") 4*text_length else text_length)
    if (estimated > 700*1024^2) stop("Estimated fixture >700 MiB; lower n or value size.")
    dat <- fixtures$.bench_make_data(list(type=type,n_rows=n,
      text_length=text_length,binary_bytes=binary_bytes,null_fraction=fraction),seed=17L)
    sql_type <- switch(type,varchar="VARCHAR(MAX)",unicode="NVARCHAR(MAX)",
      binary="VARBINARY(MAX)",binary_unique="VARBINARY(MAX)")
    counter <- counter+1L
    tab <- sprintf("rscn_tx34_%s_%02d",stamp,counter)
    id <- DBI::Id(schema=schema,table=tab)
    quoted <- as.character(DBI::dbQuoteIdentifier(con_r,id))
    if (DBI::dbExistsTable(con_r,id)) stop("Refusing to touch an existing table: ",tab)
    owned <- c(owned,tab)
    DBI::dbExecute(con_r,paste0("CREATE TABLE ",quoted," ([value] ",sql_type," NULL)"))
    expected_present <- if(type %in% c("binary","binary_unique"))
      sum(!vapply(unclass(dat$value),is.null,logical(1L))) else sum(!is.na(dat$value))
    reference <- NULL
    cat(sprintf("\n%s: %d rows, %.0f%% NULL, %s\n",type,n,100*fraction,sql_type))

    verify <- function() {
      z <- DBI::dbGetQuery(con_r, .rscn34_verify_sql(quoted))
      v <- as.numeric(unlist(z[1L,c("n","nn","bytes")],use.names=FALSE))
      names(v) <- c("rows","nonnull","bytes")
      if(anyNA(v) || v["rows"]!=n || v["nonnull"]!=expected_present)
        stop("Row or NULL count mismatch in ",tab,call.=FALSE)
      if(is.null(reference)) reference <<- v
      else if (!isTRUE(all.equal(v,reference,tolerance=0)))
        stop("Stored data DATALENGTH differs between modes in ",tab,call.=FALSE)
      v
    }
    append <- function(con, driver, table_lock=FALSE) {
      if(identical(driver,"RSqlClientNET"))
        DBI::dbAppendTable(con,id,dat,batch_rows=batch_rows,row.names=NULL,
                           table_lock=table_lock)
      else DBI::dbAppendTable(con,id,dat,row.names=NULL)
    }
    set_mode <- function(profile=FALSE) {
      options(RSqlClientNET.bulk_profile=profile,
        RSqlClientNET.bulk_profile_stages=FALSE)
    }
    # TRUNCATE from OTHER session so its CPU/waits are excluded from
    # writer-session snapshots (and always before any explicit transaction).
    prepare <- function(driver) {
      other <- if(identical(driver,"odbc")) con_r else con_o
      DBI::dbExecute(other,paste0("TRUNCATE TABLE ",quoted))
    }
    run_one <- function(mode,rep,prof=FALSE) {
      info <- mode_info[match(mode,mode_info$mode),,drop=FALSE]
      con <- connections[[info$driver]]
      prepare(info$driver)
      set_mode(prof)
      before <- common$.rscn_cmp_metrics(con,server_diagnostics && rep>0L)
      msg <- character(); tx_open <- FALSE
      tic <- proc.time()[["elapsed"]]
      err <- tryCatch({
        withCallingHandlers({
          if(info$explicit_tx) {
            DBI::dbBegin(con)
            tx_open <- TRUE
          }
          append(con,info$driver,info$table_lock)
          if(info$explicit_tx) {
            DBI::dbCommit(con)
            tx_open <- FALSE
          }
        },message=function(m) {
          msg <<- c(msg,conditionMessage(m))
          invokeRestart("muffleMessage")
        })
        NULL
      },error=function(e) conditionMessage(e))
      elapsed <- proc.time()[["elapsed"]]-tic
      if (tx_open) try(DBI::dbRollback(con),silent=TRUE)
      set_mode()
      after <- common$.rscn_cmp_metrics(con,server_diagnostics && rep>0L)
      if(!is.null(err)) stop(sprintf("%s failed: %s",mode,err),call.=FALSE)
      v <- verify()
      row <- data.frame(type=type,null_fraction=fraction,sql_type=sql_type,
        mode=mode,driver=info$driver,table_lock=info$table_lock,
        explicit_tx=info$explicit_tx,repetition=rep,elapsed_s=elapsed,
        rows_written=v["rows"],nonnull_written=v["nonnull"],
        stored_bytes=v["bytes"],status="ok",stringsAsFactors=FALSE)
      if(prof) phases[[length(phases)+1L]] <<- data.frame(
        type=type,null_fraction=fraction,mode=mode,elapsed_s=elapsed,
        r_encode_s=common$.rscn_cmp_profile_sum(msg,"R encode ([0-9.]+)s"),
        rpc_sql_s=common$.rscn_cmp_profile_sum(msg,"RPC \\+ SQL ([0-9.]+)s"),
        bulkcopy_s=common$.rscn_cmp_profile_sum(msg,"SqlBulkCopy ([0-9.]+)ms")/1000,
        pipe_send_s=common$.rscn_cmp_profile_sum(msg,"native pipe send ([0-9.]+)ms")/1000)
      if(rep>0L) {
        ses <- common$.rscn_cmp_session_delta(before$session,after$session)
        ld <- common$.rscn_cmp_io_delta(before$io,after$io,"LOG")
        dd <- common$.rscn_cmp_io_delta(before$io,after$io,"ROWS")
        io[[length(io)+1L]] <<- data.frame(type=type,null_fraction=fraction,
          mode=mode,driver=info$driver,repetition=rep,
          session_cpu_ms=unname(ses["cpu_ms"]),
          physical_log_written_mb=unname(ld["bytes_written"])/1e6,
          physical_log_writes=unname(ld["writes"]),
          physical_log_stall_ms=unname(ld["stall_write_ms"]),
          physical_data_written_mb=unname(dd["bytes_written"])/1e6)
        w <- common$.rscn_cmp_wait_delta(before$waits,after$waits,
          type,fraction,mode,rep)
        if(!is.null(w)) {
          w$mode <- w$driver
          w$driver <- info$driver
          waits[[length(waits)+1L]] <<- w
        }
      }
      cat(sprintf("  %-18s rep %2d: %.3f s [OK]\n",mode,rep,elapsed))
      invisible(row)
    }
    # Logical log diagnostics: a SEPARATE run, always with an explicit DBI
    # transaction, so both drivers are measured before commit. These values
    # are NOT timings of their normal production transaction policies.
    run_logical <- function(mode) {
      info <- mode_info[match(mode,mode_info$mode),,drop=FALSE]
      con <- connections[[info$driver]]
      prepare(info$driver)
      set_mode(FALSE)
      opened <- FALSE
      q <- list(log_bytes=NA_real_,log_records=NA_real_,
                reserved_bytes=NA_real_,reason="")
      err <- tryCatch({
        DBI::dbBegin(con)
        opened <- TRUE
        append(con,info$driver,info$table_lock)
        q <- .rscn34_query_log(con)  # still inside transaction
        DBI::dbCommit(con)
        opened <- FALSE
        NULL
      },error=function(e) conditionMessage(e))
      if(opened) try(DBI::dbRollback(con),silent=TRUE)
      set_mode()
      if(is.null(err)) verify()
      if(!is.null(err)) q$reason <- paste("Diagnostic upload:",err)
      x <- data.frame(type=type,null_fraction=fraction,mode=mode,
        driver=info$driver,table_lock=info$table_lock,
        diagnostic_explicit_tx=TRUE,
        log_bytes_used=as.numeric(q$log_bytes),
        log_records=as.numeric(q$log_records),
        log_bytes_reserved=as.numeric(q$reserved_bytes),
        note=q$reason,stringsAsFactors=FALSE)
      cat(sprintf("  log %-14s: %s bytes %s\n",mode,
        if(is.na(x$log_bytes_used)) "NA" else
          format(x$log_bytes_used,scientific=FALSE,trim=TRUE,big.mark=""),
        if(nzchar(x$note)) paste0("(",x$note,")") else ""))
      x
    }

    # Warm-up is correctness-tested, but excluded from median time.
    for(mode in mode_info$mode) run_one(mode,-1L)
    if(profile_separately) for(mode in c("rsql_standard","rsql_table_lock"))
      run_one(mode,0L,prof=TRUE)
    if(logical_log) for(mode in c("rsql_standard","rsql_table_lock","odbc_transaction"))
      logical[[length(logical)+1L]] <- run_logical(mode)
    for(rep in seq_len(repetitions)) {
      order <- sample(mode_info$mode)
      for(mode in order) attempts[[length(attempts)+1L]] <- run_one(mode,rep)
    }
    DBI::dbExecute(con_r,paste0("DROP TABLE ",quoted))
    owned <- setdiff(owned,tab)
    rm(dat)
    invisible(gc(verbose=FALSE))
  }
  attempts <- .rscn34_bind(attempts)
  phases <- .rscn34_bind(phases)
  io <- .rscn34_bind(io)
  waits <- .rscn34_bind(waits)
  logical <- .rscn34_bind(logical)
  summary <- stats::aggregate(elapsed_s ~ type+null_fraction+mode+driver,
    data=attempts,FUN=stats::median)
  names(summary)[names(summary)=="elapsed_s"] <- "median_s"
  baseline <- summary[summary$mode=="odbc_default",c("type","null_fraction","median_s")]
  key <- function(x) paste(x$type,x$null_fraction,sep="|")
  ref <- setNames(baseline$median_s,key(baseline))
  summary$speedup_vs_odbc_default <- unname(ref[key(summary)])/summary$median_s
  summary <- summary[order(summary$type,summary$null_fraction,summary$mode),,drop=FALSE]
  rownames(summary) <- NULL
  server_summary <- if(nrow(io)) stats::aggregate(
    cbind(session_cpu_ms,physical_log_written_mb,physical_log_writes,
          physical_log_stall_ms,physical_data_written_mb) ~
      type+null_fraction+mode, data=io, FUN=.rscn34_median,
      na.action=stats::na.pass) else data.frame()
  wait_summary <- if(nrow(waits)) {
    w <- stats::aggregate(delta_wait_ms ~ type+null_fraction+mode+driver+wait_type,
      data=waits,FUN=sum)
    w[order(w$type,w$null_fraction,w$mode,-w$delta_wait_ms),,drop=FALSE]
  } else data.frame()
  print(summary,row.names=FALSE)
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  .rscn34_write_csv(attempts,output_dir,"attempts")
  .rscn34_write_csv(summary,output_dir,"summary")
  .rscn34_write_csv(phases,output_dir,"phases")
  .rscn34_write_csv(io,output_dir,"server-io")
  .rscn34_write_csv(waits,output_dir,"waits")
  .rscn34_write_csv(server_summary,output_dir,"server-summary")
  .rscn34_write_csv(wait_summary,output_dir,"wait-summary")
  .rscn34_write_csv(logical,output_dir,"logical-log")
  .rscn34_write_csv(recovery_model,output_dir,"database-info")
  invisible(list(summary=summary,attempts=attempts,phases=phases,
    server_summary=server_summary,server_io=io,waits=waits,
    wait_summary=wait_summary,logical_log=logical,
    recovery_model=recovery_model,endpoints=endpoints))
}
