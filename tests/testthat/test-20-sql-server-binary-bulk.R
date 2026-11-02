testthat::test_that("SQL Server SBIN-only write and append roundtrip exactly", {
  .rsqlclientnet_skip_no_sql()
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),connection_string=cs,
                        bigint="integer64",tz="UTC")
  on.exit(if(DBI::dbIsValid(con)) DBI::dbDisconnect(con),add=TRUE)
  frame <- data.frame(id=1:5, val=c(NA_real_,pi,-1,0,1.5),
      label=c("NA","æøå 日本語",NA_character_,"","x"),
      ok=c(TRUE,FALSE,NA,TRUE,FALSE),
      day=as.Date(c("2026-01-01","2026-01-02",NA,"2026-01-04","2026-01-05")),
      stringsAsFactors=FALSE)
  frame$big <- bit64::as.integer64(c("9223372036854775807", "-1", NA, "0", "2"))
  frame$binary <- I(list(as.raw(c(0,255)),raw(0),NULL,as.raw(11),raw(0)))
  testthat::expect_true(DBI::dbWriteTable(con,"#sbin_rt",frame,
                      batch_rows=2L))
  readback <- DBI::dbGetQuery(con,"SELECT * FROM #sbin_rt ORDER BY id")
  testthat::expect_identical(readback$id,frame$id)
  testthat::expect_equal(readback$val,frame$val)
  testthat::expect_identical(readback$label,frame$label)
  testthat::expect_identical(readback$ok,frame$ok)
  testthat::expect_equal(as.character(readback$big),as.character(frame$big))
  testthat::expect_true(all(as.character(readback$day)==as.character(frame$day) |
                            (is.na(readback$day) & is.na(frame$day))))
  testthat::expect_true(DBI::dbWriteTable(con,"#sbin_default",frame,
                      batch_rows=3L))
  testthat::expect_identical(DBI::dbGetQuery(con,"SELECT COUNT(*) n FROM #sbin_default")$n[[1L]],5L)
  DBI::dbAppendTable(con,"#sbin_rt",frame[1:2,],batch_rows=1L)
  testthat::expect_identical(DBI::dbGetQuery(con,"SELECT COUNT(*) n FROM #sbin_rt")$n[[1L]],7L)
  testthat::expect_error(
    DBI::dbAppendTable(con,"#sbin_rt",frame[1,,drop=FALSE],bulk_transport="json"),
    "accepts only named batch_rows"
  )
  testthat::expect_error(
    DBI::dbWriteTable(con,"#sbin_unsupported",frame[1,,drop=FALSE],bulk_transport="json"),
    "Unsupported additional arguments"
  )
  testthat::expect_false(DBI::dbExistsTable(con,"#sbin_unsupported"))
  testthat::expect_true(DBI::dbIsValid(con))
})


testthat::test_that("SQL Server SBIN native POSIXct preserves UTC wallclock and NULL", {
  .rsqlclientnet_skip_no_sql()
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING")
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),connection_string=cs,tz="UTC")
  on.exit(if(DBI::dbIsValid(con)) DBI::dbDisconnect(con),add=TRUE)
  x <- as.POSIXct(c("1969-12-31 23:59:59", "2026-10-20 12:30:00", NA),tz="UTC")
  frame <- data.frame(id=seq_along(x),tm=x,offset=x)
  DBI::dbWriteTable(con,"#sbin_native_time",frame,
     field.types=c(tm="DATETIME2(6)",offset="DATETIMEOFFSET(6)"),
     batch_rows=2L)
  got <- DBI::dbGetQuery(con,"SELECT id, tm, offset FROM #sbin_native_time ORDER BY id")
  testthat::expect_identical(got$id,frame$id)
  testthat::expect_equal(as.numeric(got$tm),as.numeric(frame$tm),tolerance=1e-6)
  testthat::expect_equal(as.numeric(got$offset),as.numeric(frame$offset),tolerance=1e-6)
  testthat::expect_true(DBI::dbIsValid(con))
})
