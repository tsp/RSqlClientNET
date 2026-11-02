# RC35e: CREATE + INSERT transaction versus append to an existing table.
# A disposable benchmark: makes uniquely named dbo tables; never use on a
# production database. Also measures indexed table appends separately.
# Results are not guaranteed speedups; no hard-coded recovery model changes.
# A fixed set.seed() is used for reproducible benchmark case ORDER, not for
# table identities. These identifiers must remain unique across repeated runs
# in the same R process, including after failed diagnostics left tables behind.
.create_append_run_id <- function() {
  id <- paste0(Sys.getpid(), "_",
               format(Sys.time(), "%Y%m%d%H%M%OS6", tz="UTC"), "_",
               basename(tempfile(pattern="ca35_")))
  gsub("[^A-Za-z0-9_]", "", id)
}

run_create_append_benchmark <- function(
    n=10000L, repetitions=3L, text_length=10000L,
    connection_string=if (exists("con_str", .GlobalEnv, inherits=FALSE))
      get("con_str", .GlobalEnv) else Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", ""),
    schema="dbo", batch_rows=65536L, logical_log=FALSE,
    output_dir="tools/benchmarks/benchmark_results") {
  if (length(n)!=1L || !is.numeric(n) || is.na(n) || n<1 || n>100000 || n!=floor(n))
    stop("n must be an integer from 1 to 100000")
  if (length(repetitions)!=1L || !is.numeric(repetitions) || is.na(repetitions) ||
      repetitions<1 || repetitions>20 || repetitions!=floor(repetitions))
    stop("repetitions must be a whole number from 1 to 20")
  if (length(text_length)!=1L || !is.numeric(text_length) || is.na(text_length) ||
      text_length<1 || text_length>100000 || text_length!=floor(text_length))
    stop("text_length must be a whole number from 1 to 100000")
  if (!is.character(schema) || length(schema)!=1L || is.na(schema) ||
      !grepl("^[A-Za-z_][A-Za-z0-9_]*$",schema)) stop("Invalid schema")
  if (!is.logical(logical_log) || length(logical_log)!=1L || is.na(logical_log))
    stop("logical_log must be TRUE or FALSE")
  if (!is.character(connection_string) || length(connection_string)!=1L ||
      is.na(connection_string) || !nzchar(connection_string)) stop("connection_string is required")
  if (!requireNamespace("DBI",quietly=TRUE) || !requireNamespace("RSqlClientNET",quietly=TRUE))
    stop("Requires DBI and an installed RSqlClientNET")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),connection_string=connection_string)
  on.exit(try(DBI::dbDisconnect(con),silent=TRUE),add=TRUE)
  model <- tryCatch(DBI::dbGetQuery(con,"SELECT recovery_model_desc FROM sys.databases WHERE database_id = DB_ID()")$recovery_model_desc[1L],error=function(e) NA_character_)
  dat <- data.frame(id=seq_len(n),value=rep(paste(rep("x",text_length),collapse=""),n),stringsAsFactors=FALSE)
  # Use SQL VARCHAR(MAX) explicitly in every mode, not inferred NVARCHAR(MAX).
  fields <- c(id="INT",value="VARCHAR(MAX)")
  modes <- c("write","append","append_index")
  cases <- expand.grid(mode=modes,table_lock=c(FALSE,TRUE),rep=seq_len(repetitions),stringsAsFactors=FALSE)
  had_seed <- exists(".Random.seed", envir=.GlobalEnv, inherits=FALSE)
  if (had_seed) previous_seed <- get(".Random.seed",envir=.GlobalEnv)
  on.exit({
    if (had_seed) assign(".Random.seed",previous_seed,envir=.GlobalEnv)
    else if (exists(".Random.seed",envir=.GlobalEnv,inherits=FALSE))
      rm(".Random.seed",envir=.GlobalEnv)
  },add=TRUE)
  set.seed(20261029L)
  cases <- cases[sample.int(nrow(cases)),,drop=FALSE]
  # Do not draw the table suffix from the seeded benchmark RNG. A prior
  # failed run could leave a table with the very same predictable name.
  stamp <- .create_append_run_id()
  rows <- vector("list",nrow(cases)); owned <- character()
  on.exit(for(tab in rev(owned)) try(DBI::dbExecute(con,paste0("DROP TABLE IF EXISTS ",
       as.character(DBI::dbQuoteIdentifier(con,DBI::Id(schema=schema,table=tab))))),silent=TRUE),add=TRUE)
  for (i in seq_len(nrow(cases))) {
    mode <- cases$mode[i]; lock <- cases$table_lock[i]
    name <- paste0("rscn_ca35_",stamp,"_",i)
    id <- DBI::Id(schema=schema,table=name)
    qt <- as.character(DBI::dbQuoteIdentifier(con,id))
    if(DBI::dbExistsTable(con,id))stop("Unique destination already exists")
    owned <- c(owned,name)
    create_s <- 0; append_s <- NA_real_; write_s <- NA_real_
    if (mode=="write") {
      tic <- proc.time()[["elapsed"]]
      DBI::dbWriteTable(con,id,dat,field.types=fields,
                        batch_rows=batch_rows,table_lock=lock)
      write_s <- proc.time()[["elapsed"]]-tic
    } else {
      tic <- proc.time()[["elapsed"]]
      DBI::dbCreateTable(con,id,fields)
      if (mode=="append_index")
        DBI::dbExecute(con,paste0("CREATE NONCLUSTERED INDEX [IX_rscn35] ON ",qt," ([id])"))
      create_s <- proc.time()[["elapsed"]]-tic
      tic <- proc.time()[["elapsed"]]
      DBI::dbAppendTable(con,id,dat,batch_rows=batch_rows,table_lock=lock)
      append_s <- proc.time()[["elapsed"]]-tic
    }
    v <- DBI::dbGetQuery(con,paste("SELECT COUNT_BIG(*) AS n,",
      "COALESCE(SUM(CONVERT(bigint,DATALENGTH([value]))),0) AS bytes FROM",qt))
    if (as.numeric(v$n[1L]) != n || as.numeric(v$bytes[1L]) != n*text_length)
      stop("Incorrect rows/bytes for ",mode," table_lock=",lock)
    DBI::dbRemoveTable(con,id)
    owned <- setdiff(owned,name)
    rows[[i]] <- data.frame(mode=mode,table_lock=lock,rep=cases$rep[i],
      create_s=create_s,append_s=append_s,write_s=write_s,
      total_s=if (mode=="write") write_s else create_s+append_s,
      rows=n,recovery_model=model)
    cat(sprintf("  %-12s table_lock=%-5s rep %d: %.3f s
",
                mode,lock,cases$rep[i],rows[[i]]$total_s))
  }
  attempts <- do.call(rbind,rows)
  keys <- unique(attempts[c("mode","table_lock")])
  summary <- do.call(rbind,lapply(seq_len(nrow(keys)),function(i){
    x <- attempts[attempts$mode==keys$mode[i] & attempts$table_lock==keys$table_lock[i],]
    data.frame(mode=keys$mode[i],table_lock=keys$table_lock[i],
      median_total_s=stats::median(x$total_s),
      median_create_s=stats::median(x$create_s),
      median_upload_s=stats::median(if (keys$mode[i]=="write") x$write_s else x$append_s))
  }))
  rownames(summary) <- NULL
  dir.create(output_dir,recursive=TRUE,showWarnings=FALSE)
  utils::write.csv(attempts,file.path(output_dir,"create-append-attempts.csv"),row.names=FALSE)
  utils::write.csv(summary,file.path(output_dir,"create-append-summary.csv"),row.names=FALSE)
  # Persist and report the complete timing results BEFORE optional diagnostics.
  # A failure in DMV permissions or a large diagnostic transaction must never
  # make the already completed benchmark results disappear.
  print(summary,row.names=FALSE)

  # Separate diagnostic uploads use their own connection for EACH case.
  # Read dm_tran_database_transactions while the transaction is active, then
  # COMMIT and DROP the disposable table. Rolling back 100+ MB of fully logged
  # LOBs can exceed dbRollback()'s default 30-second RPC timeout and invalidate
  # the SqlClientNET helper, as seen with the original RC35c diagnostic loop.
  # Timing measurements above do not include any of these diagnostics.
  logs <- list()
  if (logical_log) {
    sql <- paste(
      "SELECT CONVERT(float,SUM(d.database_transaction_log_bytes_used)) AS bytes,",
      "CONVERT(float,SUM(d.database_transaction_log_record_count)) AS records",
      "FROM sys.dm_tran_session_transactions s",
      "JOIN sys.dm_tran_database_transactions d ON d.transaction_id=s.transaction_id",
      "WHERE s.session_id=@@SPID AND s.is_user_transaction=1 AND d.database_id=DB_ID()")

    diagnose_one <- function(mode, lock, j) {
      name <- paste0("rscn_ca35_",stamp,"_log_",j)
      id <- DBI::Id(schema=schema,table=name)
      qt <- as.character(DBI::dbQuoteIdentifier(con,id))
      # Never register a name for cleanup until this diagnostic has checked
      # that no existing table will be accidentally removed.
      claimed <- FALSE
      dcon <- NULL
      in_tx <- FALSE
      committed <- FALSE
      state <- new.env(parent=emptyenv())
      state$bytes <- NA_real_
      state$records <- NA_real_
      state$status <- "ok"
      state$note <- ""

      tryCatch({
        dcon <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
                               connection_string=connection_string)
        if (DBI::dbExistsTable(dcon, id))
          stop("Benchmark destination name collision: ", qt,
               ". A previous run may have left this table behind.")
        claimed <- TRUE
        # Register only a name known to have been absent before our attempt.
        owned <<- c(owned,name)
        if (mode != "write") {
          DBI::dbCreateTable(dcon,id,fields)
          if (mode == "append_index")
            DBI::dbExecute(dcon,paste0("CREATE NONCLUSTERED INDEX [IX_rscn35] ON ",
                                       qt," ([id])"))
        }
        DBI::dbBegin(dcon)
        in_tx <- TRUE
        if (mode == "write")
          DBI::dbWriteTable(dcon,id,dat,field.types=fields,
                            batch_rows=batch_rows,table_lock=lock)
        else
          DBI::dbAppendTable(dcon,id,dat,batch_rows=batch_rows,table_lock=lock)

        # DMV access is optional; failed reads do not abort the transaction.
        reading <- tryCatch(DBI::dbGetQuery(dcon,sql),error=function(e)e)
        if (inherits(reading,"error")) {
          state$status <- "dmv_unavailable"
          state$note <- conditionMessage(reading)
        } else if (nrow(reading) != 1L ||
                   is.na(reading$bytes[1L])) {
          state$status <- "dmv_unavailable"
          state$note <- "No active transaction log row returned by DMV"
        } else {
          state$bytes <- as.numeric(reading$bytes[1L])
          state$records <- as.numeric(reading$records[1L])
        }
        # Preserve accurate pre-commit log measurements above, but avoid the
        # potentially expensive rollback of large, fully logged LOB uploads.
        DBI::dbCommit(dcon)
        in_tx <- FALSE
        committed <- TRUE
      }, error=function(e) {
        state$status <- "error"
        state$note <- conditionMessage(e)
      })

      if (!is.null(dcon)) {
        if (in_tx && !committed &&
            isTRUE(tryCatch(DBI::dbIsValid(dcon),error=function(e)FALSE)))
          try(DBI::dbRollback(dcon),silent=TRUE)
        try(DBI::dbDisconnect(dcon),silent=TRUE)
      }

      # Clean up only if we verified the name was unused before beginning.
      # Do not DROP another run's existing table after a name collision.
      if (claimed) {
        drop_err <- tryCatch({
          DBI::dbExecute(con,paste0("DROP TABLE IF EXISTS ",qt))
          NULL
        },error=function(e) conditionMessage(e))
        if (is.null(drop_err)) {
          owned <<- setdiff(owned,name)
        } else {
          state$status <- "error"
          state$note <- paste(state$note,"Cleanup:",drop_err)
        }
      }
      data.frame(mode=mode,table_lock=lock,
        log_bytes_used=state$bytes,log_records=state$records,
        status=state$status,note=state$note)
    }

    for (j in seq_len(nrow(keys))) {
      mode <- keys$mode[j]; lock <- keys$table_lock[j]
      z <- diagnose_one(mode,lock,j)
      logs[[length(logs)+1L]] <- z
      # Checkpoint after EACH diagnostic, including cases that could not read
      # the DMV. One bad case must not discard all earlier results.
      utils::write.csv(do.call(rbind,logs),
        file.path(output_dir,"create-append-logical-log.csv"),row.names=FALSE)
      cat(sprintf("  log %-12s table_lock=%-5s: %s [%s]\n",
                  mode,lock,if (is.na(z$log_bytes_used)) "NA" else
                  format(z$log_bytes_used,scientific=FALSE),z$status))
    }
  }
  log_report <- if(length(logs)) do.call(rbind,logs) else data.frame()
  invisible(list(summary=summary,attempts=attempts,
                 logical_log=log_report,recovery_model=model))
}
