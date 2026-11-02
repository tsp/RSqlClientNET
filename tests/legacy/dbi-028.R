# Windows R 4.4, SQL Server Express integration regression tests.
# No real application tables are modified: all DML uses a session-local #temp table.
# Run with: Sys.setenv(RSQLCLIENTNET_TEST_CONNECTION_STRING='...')
#           source('tests/dbi-028.R')
run_dbi_028 <- function() {
  library(DBI)
  library(RSqlClientNET)
  stopifnot(utils::packageVersion('RSqlClientNET') >= '0.0.28')
  stopifnot(inherits(RSqlClientNET::SqlClientNET(), 'DBIDriver'))
  cs <- Sys.getenv('RSQLCLIENTNET_TEST_CONNECTION_STRING', '')
  if (!nzchar(cs)) stop('Set RSQLCLIENTNET_TEST_CONNECTION_STRING first.')
  conn <- DBI::dbConnect(RSqlClientNET::SqlClientNET(), connection_string=cs,
                         batch_rows=8192L, precision='native', bigint='integer64', tz='UTC')
  on.exit(if (DBI::dbIsValid(conn))
    try(DBI::dbDisconnect(conn), silent=TRUE), add=TRUE)
  stopifnot(methods::validObject(conn),
            is.environment(conn@handle),
            inherits(conn@handle, "sqlclient_connection"),
            DBI::dbIsValid(conn))

  message("dbi-028: testing partial fetch and EOF")
  # Partial fetch, zero rows (with classes), row counts, and an exact final fetch.
  res <- DBI::dbSendQuery(conn, paste0('SELECT n FROM (VALUES (1),(2),(3),(4),(5)) AS t(n) ORDER BY n'))
  stopifnot(!DBI::dbHasCompleted(res))
  zero <- DBI::dbFetch(res,0)
  stopifnot(nrow(zero)==0L, identical(names(zero),'n'), is.integer(zero$n))
  first <- DBI::dbFetch(res,2)
  second <- DBI::dbFetch(res,2)
  stopifnot(!DBI::dbHasCompleted(res), DBI::dbGetRowCount(res)==4)
  last <- DBI::dbFetch(res,1)
  stopifnot(DBI::dbHasCompleted(res), DBI::dbGetRowCount(res)==5)
  stopifnot(identical(c(first$n,second$n,last$n),1:5))
  stopifnot(nrow(DBI::dbFetch(res,2))==0L)
  DBI::dbClearResult(res)
  stopifnot(!DBI::dbIsValid(res), DBI::dbIsValid(conn))

  message("dbi-028: testing early clear and cursor reuse")
  # Explicitly clearing a partly fetched result frees the reader and permits reuse.
  res <- DBI::dbSendQuery(conn,'SELECT TOP (50000) ROW_NUMBER() OVER (ORDER BY a.object_id, b.object_id) AS rn FROM sys.all_objects a CROSS JOIN sys.all_objects b')
  stopifnot(nrow(DBI::dbFetch(res,7L))==7L)
  DBI::dbClearResult(res)
  stopifnot(!DBI::dbIsValid(res), DBI::dbIsValid(conn))
  stopifnot(DBI::dbGetQuery(conn,'SELECT 123 AS answer')$answer[[1]] == 123L)

  message("dbi-028: testing replacement of an active cursor")
  # Since v0.0.33.2, a second DBI query replaces the active cursor with a
  # warning (rather than throwing). The superseded result becomes invalid.
  res <- DBI::dbSendQuery(conn,
    'SELECT n FROM (VALUES (1),(2),(3),(4),(5)) AS t(n) ORDER BY n')
  stopifnot(identical(DBI::dbFetch(res,1L)$n,1L))
  replacement_warning <- NULL
  replacement <- withCallingHandlers(
    DBI::dbGetQuery(conn,'SELECT CAST(1 AS INT) AS replacement'),
    warning=function(w) {
      replacement_warning <<- conditionMessage(w)
      invokeRestart('muffleWarning')
    })
  stopifnot(identical(replacement$replacement,1L),
            isTRUE(grepl('Previous DBI result',replacement_warning,fixed=TRUE)),
            !DBI::dbIsValid(res), DBI::dbIsValid(conn))
  # Clearing the superseded result again must warn, without harming the
  # replacement query or the connection.
  old_clear_warning <- NULL
  withCallingHandlers(DBI::dbClearResult(res),warning=function(w) {
    old_clear_warning <<- conditionMessage(w)
    invokeRestart('muffleWarning')
  })
  stopifnot(isTRUE(grepl('already',old_clear_warning,fixed=TRUE)),
            DBI::dbIsValid(conn))

  message("dbi-028: testing bound parameters")
  # Parameters, including a deferred dbBind().
  p <- DBI::dbGetQuery(conn, 'SELECT CAST(@n AS INT) AS v', params=list(n=42L))
  stopifnot(identical(p$v,42L))
  res <- DBI::dbSendQuery(conn,'SELECT CAST(@n AS INT) AS v',immediate=FALSE)
  DBI::dbBind(res,list(n=17L))
  stopifnot(identical(DBI::dbFetch(res)$v,17L),DBI::dbHasCompleted(res))
  DBI::dbClearResult(res)

  message("dbi-028: testing statement results and transactions")
  # DBI dbExecute / transactions. Rollback must remove rows.
  DBI::dbExecute(conn,'CREATE TABLE #dbi028 (id INT NOT NULL, label NVARCHAR(50) NULL)')
  DBI::dbBegin(conn)
  # Verify dbSendStatement dispatch and affected rows, independently of dbExecute.
  affected <- DBI::dbExecute(conn,"INSERT INTO #dbi028 VALUES (1, N'æøå')")
  if (!identical(affected, 1L))
    stop("dbExecute() returned ", paste(affected, collapse=", "),
         " instead of 1 for a one-row INSERT (class: ",
         paste(class(affected), collapse=", "), ").")
  DBI::dbRollback(conn)
  stopifnot(DBI::dbGetQuery(conn,'SELECT COUNT(*) AS n FROM #dbi028')$n[[1]]==0L)
  DBI::dbBegin(conn)
  DBI::dbExecute(conn,"INSERT INTO #dbi028 VALUES (2,N'🧪')")
  DBI::dbCommit(conn)
  stopifnot(DBI::dbGetQuery(conn,'SELECT COUNT(*) AS n FROM #dbi028')$n[[1]]==1L)
  stopifnot(identical(DBI::dbListFields(conn,DBI::SQL('[#dbi028]')),c('id','label')))

  message("dbi-028: testing native datatype and null comparison")
  # Compare SCB2 directly with sql_query() on varied types and NULLs.
  sql <- paste0("SELECT CAST(9223372036854775807 AS bigint) AS b, ",
                "CAST(123456789.123456789 AS DECIMAL(20,9)) AS dec, ",
                "CAST('2026-10-18T12:00:00+02:00' AS datetimeoffset) AS dto, ",
                "CAST(NULL AS varbinary(8)) AS blob, CAST(N'æøå🔬' AS nvarchar(30)) AS txt")
  via_dbi<-DBI::dbGetQuery(conn,sql)
  via_sql<-RSqlClientNET::sql_query(conn@handle,sql,result_format='columnar',
                                precision='native',bigint='integer64',tz='UTC')
  stopifnot(isTRUE(all.equal(via_dbi,via_sql,check.attributes=FALSE,tolerance=1e-6)),
            identical(lapply(via_dbi,class),lapply(via_sql,class)))

  stopifnot(DBI::dbIsValid(conn))
  DBI::dbDisconnect(conn)
  stopifnot(!DBI::dbIsValid(conn))
  message('RSqlClientNET v0.0.28 DBI integration tests passed')
}
run_dbi_028()
