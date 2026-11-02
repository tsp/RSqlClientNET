# Integration test for SQL Server, run in unpacked package root after install.
# Set RSQLCLIENTNET_TEST_CONNECTION_STRING; no persistent table is created.
run_dbi_write_030 <- function() {
  library(DBI)
  library(RSqlClientNET)
  cs <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", "")
  stopifnot(nzchar(cs))
  con <- dbConnect(SqlClientNET(),connection_string=cs,tz="UTC",precision="native")
  on.exit(if (DBI::dbIsValid(con))
    try(dbDisconnect(con), silent=TRUE), add=TRUE)
  # The helper keeps one persistent SQL session, including during SqlBulkCopy.
  spid <- dbGetQuery(con,"SELECT @@SPID AS spid")$spid[[1]]
  data <- data.frame(
    id=c(1L,2L,3L),
    text=c("æøå 🐱", "", NA_character_),
    number=c(1.25,NA_real_,-4.5),
    flag=c(TRUE,FALSE,NA),
    day=as.Date(c("1899-12-31","2026-03-29",NA)),
    stamp=as.POSIXct(c("2026-03-29 01:30:00","2026-10-25 01:30:00",NA),tz="UTC"),
    stringsAsFactors=FALSE,check.names=FALSE
  )
  message("dbi-write-030: explicit #temp name and roundtrip")
  dbWriteTable(con,"#dbi030",data,batch_rows=2L)
  stopifnot(dbExistsTable(con,"#dbi030"),
            identical(dbGetQuery(con,"SELECT @@SPID AS spid")$spid[[1]],spid))
  a <- dbReadTable(con, "#dbi030")
  # SQL tables are *unordered*. dbReadTable() performs SELECT * without
  # ORDER BY, so even a successful SqlBulkCopy roundtrip need not return
  # rows in insertion order. Align by the unique, test-defined row ID first.
  # Do not weaken the Unicode/NULL comparison: text is still checked exactly.
  stopifnot(nrow(a) == nrow(data),
            !anyNA(a$id), !anyDuplicated(a$id),
            !anyDuplicated(data$id),
            identical(sort(a$id), sort(data$id)))
  a <- a[order(a$id), ]
  expected <- data[order(data$id), , drop = FALSE]
  if (!identical(a$text, expected$text)) {
    different <- which(!vapply(seq_along(expected$text), function(i)
      identical(a$text[[i]], expected$text[[i]]), logical(1)))
    if (!length(different)) {
      stop("Bulk text roundtrip: vector attributes or encodings differ ",
           "even though every scalar compares equal.", call. = FALSE)
    }
    i <- different[[1L]]
    codepoints <- function(z) {
      if (is.na(z)) return("SQL NULL / R NA")
      cp <- utf8ToInt(enc2utf8(z))
      if (!length(cp)) return("<empty string>")
      paste(sprintf("U+%04X", cp), collapse = " ")
    }
    stop(sprintf(paste0("Bulk Unicode/NULL roundtrip mismatch at id=%s ",
      "(ordered row %d). Expected codepoints: %s; received: %s; ",
      "expected Encoding=%s, received Encoding=%s"),
      as.character(expected$id[[i]]), i,
      codepoints(expected$text[[i]]), codepoints(a$text[[i]]),
      Encoding(expected$text[[i]]), Encoding(a$text[[i]])), call. = FALSE)
  }
  stopifnot(identical(a$id, expected$id),
            identical(as.numeric(a$number), as.numeric(expected$number)),
            identical(a$flag, expected$flag),
            identical(as.character(a$day), as.character(expected$day)),
            identical(is.na(a$stamp), is.na(expected$stamp)))
  dbAppendTable(con,"#dbi030",data[1,],batch_rows=1L)
  stopifnot(dbGetQuery(con,"SELECT COUNT(*) n FROM #dbi030")$n[[1]]==4)
  # tempdb # table is not accessible in a separate physical SqlConnection.
  con2<-dbConnect(SqlClientNET(),connection_string=cs,tz="UTC")
  stopifnot(!dbExistsTable(con2,"#dbi030"))
  dbDisconnect(con2)
  # A temporary=TRUE logical name transparently becomes a local # table.
  message("dbi-write-030: temporary=TRUE alias and row preservation")
  dbWriteTable(con,"session_alias",data[1:2,],temporary=TRUE,batch_rows=1L)
  stopifnot(dbExistsTable(con,"session_alias"),
            identical(as.character(dbQuoteIdentifier(con,"session_alias")),"[#session_alias]"),
            nrow(dbReadTable(con,"session_alias"))==2L)
  dbAppendTable(con,"session_alias",data[3,],batch_rows=1L)
  stopifnot(nrow(dbReadTable(con,"session_alias"))==3L,
            identical(dbGetQuery(con,"SELECT @@SPID spid")$spid[[1]],spid))
  if (requireNamespace("dplyr",quietly=TRUE) && requireNamespace("dbplyr",quietly=TRUE) &&
      utils::packageVersion("dbplyr") >= "2.6.0") {
    message("dbi-write-030: dbplyr copy_to temporary=TRUE")
    copied <- dplyr::copy_to(con, data.frame(id=11:12, txt=c("æ", "")),
                             name="dbi030_copy", temporary=TRUE, overwrite=TRUE)
    stopifnot(nrow(dplyr::collect(copied)) == 2L,
              dbExistsTable(con,"dbi030_copy"),
              dbExistsTable(con,"#dbi030_copy"),
              nrow(dbReadTable(con,"dbi030_copy")) == 2L,
              nrow(dbReadTable(con,"#dbi030_copy")) == 2L)
    copy_other <- dbConnect(SqlClientNET(), connection_string=cs, tz="UTC")
    stopifnot(!dbExistsTable(copy_other,"#dbi030_copy"))
    dbDisconnect(copy_other)
    dbRemoveTable(con,"dbi030_copy")
    stopifnot(!dbExistsTable(con,"dbi030_copy"),
              !dbExistsTable(con,"#dbi030_copy"))
  }
  message("dbi-write-030: dbCreateTable, NULL, bigint and binary")
  dbCreateTable(con,"#dbi030b",list(id="BIGINT",v="VARBINARY(MAX)",d="DECIMAL(38,10)"))
  payload<-data.frame(id=bit64::as.integer64(c("9223372036854775807",NA)),
                      d=c("1234567890123456789012345678.1234567890",NA_character_),
                      stringsAsFactors=FALSE)
  payload$v<-I(list(as.raw(c(0,255,7)),raw(0)))
  dbAppendTable(con,"#dbi030b",payload,batch_rows=1L)
  value<-dbGetQuery(con,"SELECT CAST(id AS VARCHAR(30)) id, CAST(d AS VARCHAR(60)) d, DATALENGTH(v) length FROM #dbi030b ORDER BY CASE WHEN id IS NULL THEN 1 ELSE 0 END")
  stopifnot(identical(value$id[[1]],"9223372036854775807"),
            identical(value$d[[1]],"1234567890123456789012345678.1234567890"),
            identical(as.integer(value$length),c(3L,0L)))
  message("dbi-write-030: overwrite, cleanup and transaction rollback")
  dbWriteTable(con,"#dbi030",data[1,],overwrite=TRUE)
  stopifnot(nrow(dbReadTable(con,"#dbi030"))==1)
  dbBegin(con)
  dbAppendTable(con,"#dbi030",data[2,])
  dbRollback(con)
  stopifnot(nrow(dbReadTable(con,"#dbi030"))==1)
  dbRemoveTable(con,"#dbi030")
  dbRemoveTable(con,"#dbi030b")
  dbRemoveTable(con,"session_alias")
  stopifnot(!dbExistsTable(con,"#dbi030"), !dbExistsTable(con,"session_alias"))
  dbWriteTable(con,"#dbi030lifetime",data[1,])
  stopifnot(dbExistsTable(con,"#dbi030lifetime"))
  dbDisconnect(con)
  newcon<-dbConnect(SqlClientNET(),connection_string=cs)
  on.exit(try(dbDisconnect(newcon),silent=TRUE),add=TRUE)
  stopifnot(!dbExistsTable(newcon,"#dbi030lifetime"))
  message("RSqlClientNET 0.0.30 DBI SqlBulkCopy and #temp session lifetime tests passed")
  invisible(TRUE)
}
run_dbi_write_030()
