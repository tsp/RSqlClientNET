test_that("SQL Server ordered DST fold is retained across DBI fetch calls", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    precision="native", tz="Europe/Copenhagen", dst_fallback="ordered",
    batch_rows=2L)
  res <- NULL
  # Clear an active cursor BEFORE disconnecting the connection.
  on.exit({
    if (!is.null(res) && DBI::dbIsValid(res)) DBI::dbClearResult(res)
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  }, add=TRUE)
  sql <- paste0(
    "SELECT stamp FROM (VALUES ",
    "(1, CAST('2026-10-25T01:59:59' AS datetime2(7))),",
    "(2, CAST('2026-10-25T02:30:00' AS datetime2(7))),",
    "(3, CAST('2026-10-25T02:00:00' AS datetime2(7))),",
    "(4, CAST('2026-10-25T02:30:00' AS datetime2(7))),",
    "(5, CAST('2026-10-25T03:00:00' AS datetime2(7)))",
    ") AS t(seq, stamp) ORDER BY seq")
  res <- DBI::dbSendQuery(con,sql)
  warnings <- character()
  rows <- withCallingHandlers({
    one <- DBI::dbFetch(res, n=2L)
    two <- DBI::dbFetch(res, n=2L)
    three <- DBI::dbFetch(res, n=2L)
    rbind(one,two,three)
  }, warning=function(w) {
    warnings <<- c(warnings,conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  expect_equal(nrow(rows),5L)
  expect_false(anyNA(rows$stamp))
  expect_true(all(diff(as.numeric(rows$stamp)) > 0))
  expect_equal(as.numeric(difftime(rows$stamp[3],rows$stamp[2],units="mins")),30)
  expect_true(any(grepl("resolved using row order",warnings,fixed=TRUE)))
  expect_true(any(grepl("Row(s): 3",warnings,fixed=TRUE)))
})
