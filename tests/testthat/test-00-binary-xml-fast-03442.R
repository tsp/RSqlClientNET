testthat::test_that("single-column fast path preserves SCB2 protocol and opt-in timing", {
  candidates <- c(file.path("inst", "dotnet", "Program.cs"),
                  file.path("..", "..", "inst", "dotnet", "Program.cs"),
                  system.file("dotnet", "Program.cs", package="RSqlClientNET"))
  candidates <- candidates[nzchar(candidates) & file.exists(candidates)]
  if (!length(candidates))
    testthat::skip("Helper source is not included in the installed binary package")
  helper <- candidates[[1L]]
  text <- paste(readLines(helper, warn=FALSE, encoding="UTF-8"),collapse="\n")
  testthat::expect_match(text, "SingleColumnBinaryOrXml",fixed=TRUE)
  testthat::expect_match(text, "WriteSingleColumnBatch",fixed=TRUE)
  testthat::expect_match(text, "readAndBufferSeconds",fixed=TRUE)
  testthat::expect_match(text, "encodeAndPipeSeconds",fixed=TRUE)
  testthat::expect_match(text, "writer.Write(bitmap)",fixed=TRUE)
  testthat::expect_match(text, "Columnar batch exceeds 128 MiB",fixed=TRUE)
  # All three read pathways use the same exact-frame-size budget.
  testthat::expect_match(text, "class ColumnarBatchBudget", fixed=TRUE)
  testthat::expect_match(text, "ColumnarValueBytes", fixed=TRUE)
  testthat::expect_match(text, "budget.Fits(valueBytes)", fixed=TRUE)
})
