test_that("all edge-type scenarios are checked, without early exit", {
  .rsqlclientnet_skip_no_dotnet()
  report <- RSqlClientNET:::sql_edge_types_report()
  bad <- report[report$status != "PASS", , drop = FALSE]
  expect_gt(nrow(report), 50L)
  expect_equal(nrow(bad), 0L, info = paste(
    capture.output(print(bad, row.names = FALSE)), collapse = "\n"))
  dst <- subset(report, tz == "Europe/Copenhagen" & precision == "native" &
                column == "wall_clock")
  expect_true(all(grepl("converted to NA", dst$csv_warnings, fixed = TRUE)))
  expect_true(all(grepl("converted to NA", dst$columnar_warnings, fixed = TRUE)))
})


test_that("edge fixture reports spring gap row 1 and fall overlap row 2", {
  .rsqlclientnet_skip_no_dotnet()
  for (format in c("csv", "columnar")) {
    caught <- character()
    x <- withCallingHandlers(RSqlClientNET:::sql_edge_types_test(
      result_format=format, precision="native", bigint="character",
      tz="Europe/Copenhagen", batch_rows=2L),
      warning=function(e) {
        caught <<- c(caught, conditionMessage(e))
        invokeRestart("muffleWarning")
      })
    expect_true(is.na(x$wall_clock[1]))
    expect_true(is.na(x$wall_clock[2]))
    expect_true(any(grepl("Row(s): 1", caught, fixed=TRUE)))
    expect_true(any(grepl("Row(s): 2", caught, fixed=TRUE)))
    expect_true(any(grepl("ambiguous", caught, fixed=TRUE)))
    expect_true(all(grepl("converted to NA", caught, fixed=TRUE)))
  }
})
