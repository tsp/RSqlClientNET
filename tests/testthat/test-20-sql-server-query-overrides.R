test_that("individual DBI query, cursor and table reads override connection options", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
    connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"),
    precision="native", tz="UTC", dst_fallback="na", bigint="integer64",
    batch_rows=2L)
  res <- NULL
  temp_created <- FALSE
  # Single cleanup block: active cursor -> temporary table -> connection.
  on.exit({
    if (!is.null(res) && DBI::dbIsValid(res)) DBI::dbClearResult(res)
    if (isTRUE(temp_created) && DBI::dbIsValid(con))
      try(DBI::dbExecute(con, "DROP TABLE IF EXISTS #RSqlClientNET_override_test"), silent=TRUE)
    if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
  }, add=TRUE)
  stmt <- paste0("SELECT CAST('2026-10-25T02:30:00' AS datetime2(7)) AS stamp, ",
                 "CAST(1234567890123456789 AS bigint) AS big")
  exact <- DBI::dbGetQuery(con,stmt,precision="full",bigint="character")
  expect_type(exact$stamp,"character")
  expect_type(exact$big,"character")
  native <- DBI::dbGetQuery(con,stmt)
  expect_s3_class(native$stamp,"POSIXct")
  expect_true(bit64::is.integer64(native$big))
  expect_identical(con@handle$dbi_options$tz,"UTC")
  expect_identical(con@handle$dbi_options$precision,"native")

  fold_sql <- paste0(
    "SELECT stamp FROM (VALUES",
    "(1, CAST('2026-10-25T02:30:00' AS datetime2(7))),",
    "(2, CAST('2026-10-25T02:59:59' AS datetime2(7))),",
    "(3, CAST('2026-10-25T02:00:00' AS datetime2(7)))",
    ") x(ord,stamp) ORDER BY ord")
  res <- DBI::dbSendQuery(con,fold_sql,precision="native",
     tz="Europe/Copenhagen",dst_fallback="ordered",batch_rows=2L)
  warnings <- character()
  result <- withCallingHandlers({
    one <- DBI::dbFetch(res,n=2L)
    two <- DBI::dbFetch(res,n=2L)
    rbind(one,two)
  },warning=function(w) { warnings <<- c(warnings,conditionMessage(w));
                          invokeRestart("muffleWarning") })
  expect_false(anyNA(result$stamp))
  expect_true(all(diff(as.numeric(result$stamp)) > 0))
  expect_true(any(grepl("resolved using row order",warnings,fixed=TRUE)))
  DBI::dbClearResult(res)
  res <- NULL

  DBI::dbExecute(con,"SELECT CAST('2026-10-25T02:30:00' AS datetime2(7)) AS stamp INTO #RSqlClientNET_override_test")
  temp_created <- TRUE
  tbl_exact <- DBI::dbReadTable(con,"#RSqlClientNET_override_test",precision="full")
  tbl_native <- DBI::dbReadTable(con,"#RSqlClientNET_override_test")
  expect_type(tbl_exact$stamp,"character")
  expect_s3_class(tbl_native$stamp,"POSIXct")
})
