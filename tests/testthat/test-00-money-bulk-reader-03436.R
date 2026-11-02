testthat::test_that("MONEY bulk decoding preserves CLR decimal vs SqlDecimal metadata", {
  # Read the distributed C# source; this catches accidental regression in the
  # destination-kind negotiation even when SQL Server is not configured.
  # Prefer the source checkout during devtools::test(); an earlier installed
  # version of RSqlClientNET may be on the library path in that context.
  source_dirs <- c(file.path("inst", "dotnet"),
                   file.path("..", "..", "inst", "dotnet"))
  source_dirs <- source_dirs[file.exists(file.path(source_dirs, "BulkBinaryReader.cs"))]
  if (length(source_dirs)) {
    reader_path <- file.path(source_dirs[[1L]], "BulkBinaryReader.cs")
    program_path <- file.path(source_dirs[[1L]], "Program.cs")
  } else {
    reader_path <- system.file("dotnet", "BulkBinaryReader.cs", package="RSqlClientNET")
    program_path <- system.file("dotnet", "Program.cs", package="RSqlClientNET")
  }
  testthat::expect_true(file.exists(reader_path))
  testthat::expect_true(file.exists(program_path))
  reader <- paste(readLines(reader_path, warn=FALSE), collapse="\n")
  program <- paste(readLines(program_path, warn=FALSE), collapse="\n")
  testthat::expect_match(reader, '"money" => 14',fixed=TRUE)
  testthat::expect_match(reader, '14 => typeof(decimal)',fixed=TRUE)
  testthat::expect_match(reader, '14 => ParseMoneyValue(s)',fixed=TRUE)
  testthat::expect_match(reader, 'decimal.Round(decimal.Parse(',fixed=TRUE)
  testthat::expect_match(reader, 'MidpointRounding.AwayFromZero',fixed=TRUE)
  testthat::expect_match(program, 'SBIN MONEY scale-4 decoder self-test failed',fixed=TRUE)
  testthat::expect_match(reader, '6 => SqlDecimal.Parse(s)',fixed=TRUE)
  testthat::expect_match(program, 'kinds[i]="money"',fixed=TRUE)
  testthat::expect_match(program, 'kinds[i]="decimal"',fixed=TRUE)
})
