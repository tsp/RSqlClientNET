testthat::test_that("SQL Server: vectorized DBI bindings and repeated binding", {
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  testthat::skip_if(!nzchar(cs), "RSQLCLIENTNET_TEST_CONNECTION_STRING not set")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),connection_string=cs,
                        bigint="integer64",tz="UTC")
  on.exit(try(DBI::dbDisconnect(con),silent=TRUE),add=TRUE)
  DBI::dbExecute(con,"CREATE TABLE #bind033 (id INT, val NVARCHAR(100) NULL)")
  ins <- DBI::dbSendStatement(con,
       "INSERT INTO #bind033(id,val) VALUES (@id,@val)",immediate=FALSE)
  on.exit(if (DBI::dbIsValid(ins))
    try(DBI::dbClearResult(ins), silent=TRUE), add=TRUE)
  DBI::dbBind(ins,list(id=1:3,val=c("æøå", NA_character_, "NA")))
  testthat::expect_identical(DBI::dbGetRowsAffected(ins),3L)
  DBI::dbBind(ins,list(id=4L,val=""))
  testthat::expect_identical(DBI::dbGetRowsAffected(ins),1L)
  DBI::dbClearResult(ins)
  testthat::expect_identical(DBI::dbGetQuery(con,"SELECT COUNT(*) AS n FROM #bind033")$n[[1L]],4L)
  sel <- DBI::dbSendQuery(con,
    "SELECT id, val FROM #bind033 WHERE id = @id",immediate=FALSE)
  DBI::dbBind(sel,list(id=c(3L,1L,2L,4L)))
  testthat::expect_false(DBI::dbHasCompleted(sel))
  got1 <- DBI::dbFetch(sel,n=2L)
  got2 <- DBI::dbFetch(sel,n=2L)
  testthat::expect_identical(c(got1$id,got2$id),c(3L,1L,2L,4L))
  testthat::expect_true(DBI::dbHasCompleted(sel))
  testthat::expect_true(is.na(got2$val[[1L]]))
  DBI::dbBind(sel,list(id=c(4L,1L))) # Rebind same SQL result
  again <- DBI::dbFetch(sel,n=NA_integer_)
  testthat::expect_identical(again$id,c(4L,1L))
  DBI::dbClearResult(sel)
  testthat::expect_true(DBI::dbIsValid(con))
  # Direct dbExecute(params=...) uses same vectorized statement path.
  n <- DBI::dbExecute(con,
       "DELETE FROM #bind033 WHERE id=@id",params=list(id=c(1L,3L)))
  testthat::expect_identical(n,2L)
  testthat::expect_identical(DBI::dbGetQuery(con,"SELECT COUNT(*) AS n FROM #bind033")$n[[1L]],2L)
})
