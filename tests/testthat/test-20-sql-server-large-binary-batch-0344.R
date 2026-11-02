testthat::test_that("SQL Server SBIN uploads 65536 and 131072 row batches", {
  .rsqlclientnet_skip_no_sql()
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),connection_string=cs,tz="UTC")
  on.exit(if(DBI::dbIsValid(con)) DBI::dbDisconnect(con),add=TRUE)
  n <- 131075L # one full 131072-row frame, followed by 3 rows
  x <- data.frame(id=seq_len(n),
                  val=rep(c(NA_real_,3.25),length.out=n),
                  label=rep(c("æøå", "", NA_character_),length.out=n))
  for (bs in c(65536L,131072L)) {
    testthat::expect_true(DBI::dbWriteTable(con,"#sbin_large_0344",x,
      overwrite=TRUE,batch_rows=bs))
    q <- DBI::dbGetQuery(con, paste(
      "SELECT COUNT_BIG(*) AS n,",
      "SUM(CONVERT(bigint, id)) AS id_sum,",
      "SUM(CASE WHEN val IS NULL THEN 1 ELSE 0 END) AS nulls,",
      "SUM(CASE WHEN label = N'æøå' THEN 1 ELSE 0 END) AS unicode_n",
      "FROM #sbin_large_0344"))
    testthat::expect_equal(as.numeric(q$n[[1L]]), as.numeric(n))
    testthat::expect_equal(as.numeric(q$id_sum[[1L]]), n*(n+1)/2)
    testthat::expect_equal(as.numeric(q$nulls[[1L]]), as.numeric(ceiling(n/2)))
    testthat::expect_equal(as.numeric(q$unicode_n[[1L]]),
                           as.numeric(length(seq.int(1L,n,by=3L))))
    testthat::expect_true(DBI::dbIsValid(con))
  }
})
