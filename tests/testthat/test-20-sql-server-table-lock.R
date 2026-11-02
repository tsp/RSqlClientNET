testthat::test_that("TableLock roundtrip on temporary and indexed permanent SQL tables", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add=TRUE)
  d <- data.frame(id=1:3, txt=c("æøå 漢字", NA_character_, paste(rep("X", 10000), collapse="")),
                  stringsAsFactors=FALSE)
  d$bin <- I(list(as.raw(c(0L,255L,1L)), NULL, raw(0)))
  for (lock in c(FALSE,TRUE)) {
    tbl <- paste0("#rscn35_lock_", as.integer(lock))
    testthat::expect_true(DBI::dbWriteTable(con,tbl,d,table_lock=lock,batch_rows=2L))
    extra <- d[1L,,drop=FALSE]; extra$id <- 4L
    testthat::expect_equal(DBI::dbAppendTable(con,tbl,extra,table_lock=lock),1)
    z <- DBI::dbGetQuery(con,paste0("SELECT [id], [txt], [bin] FROM [",tbl,"] ORDER BY [id]"))
    testthat::expect_identical(z$id,1:4)
    testthat::expect_identical(z$txt,c(d$txt,d$txt[1L]))
    testthat::expect_true(is.na(z$txt[2L]))
    testthat::expect_identical(as.integer(z$bin[[1L]]),c(0L,255L,1L))
    testthat::expect_true(DBI::dbRemoveTable(con,tbl))
  }
  # Permanent heap with a secondary index; TableLock must preserve semantics
  # even when the engine is not eligible for the same minimal-logging strategy.
  tab <- paste0("rscn35_idx_",Sys.getpid(),"_",sample.int(1e6,1L))
  id <- DBI::Id(schema="dbo",table=tab)
  qt <- as.character(DBI::dbQuoteIdentifier(con,id))
  on.exit(try(DBI::dbExecute(con,paste0("DROP TABLE IF EXISTS ",qt)),silent=TRUE),add=TRUE)
  DBI::dbCreateTable(con,id,c(id="INT",txt="NVARCHAR(MAX)",bin="VARBINARY(MAX)"))
  DBI::dbExecute(con,paste0("CREATE NONCLUSTERED INDEX [IX_rscn35] ON ",qt," ([id])"))
  testthat::expect_equal(DBI::dbAppendTable(con,id,d,table_lock=TRUE),3)
  testthat::expect_equal(as.numeric(DBI::dbGetQuery(con,paste0("SELECT COUNT_BIG(*) n FROM ",qt))$n),3)
  testthat::expect_equal(DBI::dbAppendTable(con,id,d[1L,,drop=FALSE],table_lock=FALSE),1)
  testthat::expect_true(DBI::dbRemoveTable(con,id))
})

testthat::test_that("invalid table_lock does not create or append a table", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
  on.exit(if(DBI::dbIsValid(con)) DBI::dbDisconnect(con),add=TRUE)
  tbl <- "#rscn35_invalid_flag"
  testthat::expect_error(DBI::dbWriteTable(con,tbl,data.frame(x=1L),table_lock=NA),"table_lock")
  testthat::expect_false(DBI::dbExistsTable(con,tbl))
  DBI::dbWriteTable(con,tbl,data.frame(x=1L))
  on.exit(try(DBI::dbRemoveTable(con,tbl),silent=TRUE),add=TRUE)
  testthat::expect_error(DBI::dbAppendTable(con,tbl,data.frame(x=2L),table_lock=NULL),"table_lock")
  testthat::expect_error(DBI::dbAppendTable(con,tbl,data.frame(x=2L),table_lock="TRUE"),"table_lock")
  testthat::expect_equal(DBI::dbGetQuery(con,paste0("SELECT COUNT(*) n FROM ",tbl))$n,1L)
})

testthat::test_that("TableLock default works for write and append, explicit FALSE remains available", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
  on.exit(if(DBI::dbIsValid(con)) DBI::dbDisconnect(con), add=TRUE)
  name <- "#rscn010_default_lock"
  testthat::expect_true(DBI::dbWriteTable(con, name, data.frame(x=1:3)))
  testthat::expect_equal(DBI::dbAppendTable(con, name, data.frame(x=4:5)), 2)
  testthat::expect_equal(DBI::dbAppendTable(con, name, data.frame(x=6L),
                                           table_lock=FALSE), 1)
  testthat::expect_identical(DBI::dbGetQuery(con,
    "SELECT [x] FROM [#rscn010_default_lock] ORDER BY [x]")$x, 1:6)
  testthat::expect_true(DBI::dbRemoveTable(con, name))
})
