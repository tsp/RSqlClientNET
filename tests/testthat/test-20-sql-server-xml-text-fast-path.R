testthat::test_that("XML and explicit NVARCHAR projections preserve values with allocation profile", {
  .rsqlclientnet_skip_no_sql()
  con <- DBI::dbConnect(RSqlClientNET::SqlClientNET(),
     connection_string=Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING"))
  on.exit(if (DBI::dbIsValid(con)) DBI::dbDisconnect(con), add=TRUE)
  dat <- data.frame(id=1:5, doc=c("<root>æøå 漢字</root>", NA_character_,
                        "<root>x</root>", "<root />", "<root>æøå 漢字</root>"),
                    stringsAsFactors=FALSE)
  testthat::expect_true(DBI::dbWriteTable(con, "#rscn_xml_cast28", dat,
    field.types=c(doc="XML"), row.names=FALSE, batch_rows=2L))
  on.exit(try(DBI::dbRemoveTable(con,"#rscn_xml_cast28"), silent=TRUE), add=TRUE)
  opts <- options(RSqlClientNET.dbi_alloc_profile=TRUE)
  on.exit(options(opts), add=TRUE)
  collect <- function(sql) {
    logs <- character()
    ans <- withCallingHandlers(DBI::dbGetQuery(con,sql), message=function(m) {
      logs <<- c(logs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
    testthat::expect_true(any(grepl("allocation read:.*GetValue=", logs)))
    ans$doc
  }
  xml <- collect("SELECT [doc] FROM #rscn_xml_cast28 ORDER BY [id]")
  as_text <- collect(paste0("SELECT CONVERT(NVARCHAR(MAX), [doc]) AS [doc] ",
                           "FROM #rscn_xml_cast28 ORDER BY [id]"))
  testthat::expect_identical(length(xml), 5L)
  testthat::expect_identical(length(as_text), 5L)
  testthat::expect_identical(is.na(xml), is.na(as_text))
  # SQL XML is a structured type, not a verbatim text container. SQL Server's
  # NVARCHAR conversion and SqlClient's XML GetValue() are allowed to serialize
  # the same empty element as <root/> and <root /> respectively. The other
  # fixture entries should nevertheless match byte-for-byte (incl. Unicode).
  testthat::expect_identical(xml[c(1L, 2L, 3L, 5L)],
                             as_text[c(1L, 2L, 3L, 5L)])
  testthat::expect_match(xml[[4L]], "^<root\\s*/>$", perl=TRUE)
  testthat::expect_match(as_text[[4L]], "^<root\\s*/>$", perl=TRUE)
  testthat::expect_true(grepl("漢字", as_text[[1]], fixed=TRUE))
})
