# MANUAL SQL SERVER BENCHMARK — NOT RUN DURING testthat.
# Start from an R session with an existing open RSqlClientNET DBI connection
# called `sqlclient`. Requires nycflights13 and RSqlClientNET >= 0.0.34.4.
# Upload is to one dedicated session-local #temp table, never user tables.
# Run: source("tools/benchmarks/flights-batch-size-benchmark.R")
#      out <- benchmark_RSqlClientNET_upload_batches(sqlclient)
#      out$summary
#      out$individual

benchmark_RSqlClientNET_upload_batches <- function(con,
    dat=as.data.frame(nycflights13::flights),
    batch_sizes=c(8192L,16384L,32768L,65536L,131072L),
    repetitions=5L, seed=20261020L, warmup=TRUE) {
  stopifnot(DBI::dbIsValid(con), is.data.frame(dat), nrow(dat)>0L,
            length(batch_sizes)>=2L, all(!is.na(batch_sizes)),
            all(batch_sizes == as.integer(batch_sizes)),
            all(batch_sizes>=1L & batch_sizes<=131072L),
            repetitions>=1L, length(repetitions)==1L)
  name <- "#RSqlClientNET_bench_batch_0344"
  old_opt <- options(RSqlClientNET.bulk_profile=FALSE,
                     RSqlClientNET.bulk_profile_stages=FALSE)
  on.exit(options(old_opt),add=TRUE)
  on.exit({
    if(DBI::dbIsValid(con)) {
      try(if(DBI::dbExistsTable(con,name)) DBI::dbRemoveTable(con,name),silent=TRUE)
    }
  },add=TRUE)
  if(DBI::dbExistsTable(con,name))
    stop("Benchmark temporary table already exists: ", name, call.=FALSE)

  if(isTRUE(warmup)) {
    DBI::dbWriteTable(con,name,dat[seq_len(min(10000L,nrow(dat))),,drop=FALSE],
                      batch_rows=16384L)
    DBI::dbRemoveTable(con,name)
  }
  # Random order per repetition to balance cache/log growth / server load.
  set.seed(seed)
  schedule <- unlist(replicate(as.integer(repetitions), sample(as.integer(batch_sizes)),
                               simplify=FALSE),use.names=FALSE)
  individual <- vector("list",length(schedule))
  for(i in seq_along(schedule)) {
    bs <- schedule[[i]]
    elapsed <- unname(system.time({
      DBI::dbWriteTable(con,name,dat,overwrite=TRUE,
                        batch_rows=bs)
    })[["elapsed"]])
    got <- DBI::dbGetQuery(con,paste0("SELECT COUNT_BIG(*) AS n FROM ",
                        as.character(DBI::dbQuoteIdentifier(con,name))))$n[[1L]]
    if(as.numeric(got)!=nrow(dat))
      stop(sprintf("Row count mismatch after batch_rows=%d: expected %d got %s",
                   bs,nrow(dat),as.character(got)),call.=FALSE)
    individual[[i]] <- data.frame(run=i,batch_rows=bs,seconds=elapsed,
                                  rows_per_second=nrow(dat)/elapsed)
    message(sprintf("%2d/%d: %6d rows/batch  %.3f s",
                    i,length(schedule),bs,elapsed))
  }
  individual <- do.call(rbind,individual)
  summary <- do.call(rbind,lapply(split(individual,individual$batch_rows),function(z)
    data.frame(batch_rows=z$batch_rows[[1L]],n=nrow(z),
               median_seconds=median(z$seconds),
               min_seconds=min(z$seconds),max_seconds=max(z$seconds),
               median_rows_per_second=median(z$rows_per_second))))
  summary <- summary[order(summary$batch_rows),,drop=FALSE]
  rownames(summary) <- NULL
  print(summary,row.names=FALSE)
  invisible(list(summary=summary,individual=individual))
}
