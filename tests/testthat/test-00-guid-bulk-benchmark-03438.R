testthat::test_that("GUID/offset SBIN destination conversion uses typed CLR values", {
  dirs <- c(file.path("inst", "dotnet"),
            file.path("..", "..", "inst", "dotnet"))
  dirs <- dirs[file.exists(file.path(dirs, "BulkBinaryReader.cs"))]
  if (length(dirs)) {
    reader_path <- file.path(dirs[[1L]], "BulkBinaryReader.cs")
    program_path <- file.path(dirs[[1L]], "Program.cs")
  } else {
    reader_path <- system.file("dotnet", "BulkBinaryReader.cs", package = "RSqlClientNET")
    program_path <- system.file("dotnet", "Program.cs", package = "RSqlClientNET")
  }
  testthat::expect_true(file.exists(reader_path))
  testthat::expect_true(file.exists(program_path))
  reader <- paste(readLines(reader_path, warn = FALSE), collapse = "\n")
  program <- paste(readLines(program_path, warn = FALSE), collapse = "\n")
  testthat::expect_match(reader, '"guid" => 15', fixed = TRUE)
  testthat::expect_match(reader, '"datetimeoffset_text" => 16', fixed = TRUE)
  testthat::expect_match(reader, '15 => typeof(Guid)', fixed = TRUE)
  testthat::expect_match(reader, '16 => typeof(DateTimeOffset)', fixed = TRUE)
  testthat::expect_match(reader, '15 => Guid.Parse(s)', fixed = TRUE)
  testthat::expect_match(reader, '16 => DateTimeOffset.Parse(s', fixed = TRUE)
  testthat::expect_match(program, 'kinds[i]="guid"', fixed = TRUE)
  testthat::expect_match(program, 'kinds[i]="datetimeoffset_text"', fixed = TRUE)
})

testthat::test_that("independent benchmark read and write row counts and Unicode widths", {
  paths <- c(file.path("tools", "benchmark", "benchmark_dbi.R"),
             file.path("..", "..", "tools", "benchmark", "benchmark_dbi.R"))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) testthat::skip("Benchmark script is not in the installed package")
  env <- new.env(parent = baseenv())
  sys.source(paths[[1L]], envir = env)
  p <- env$benchmark_plan(
    n = 1000, n_write = 100, n_read = 1000,
    types = "unicode", text_lengths = 12L,
    null_fractions = 0, max_case_payload_mb = 1)
  testthat::expect_equal(nrow(p), 1L)
  testthat::expect_equal(p$n_write_rows, 100L)
  testthat::expect_equal(p$n_read_rows, 1000L)
  testthat::expect_identical(unname(env$.bench_schema(as.list(p[1L,]))), "NVARCHAR(24)")
  testthat::expect_true(all(nchar(env$.bench_text(12, 12, unicode = TRUE)) == 12L))
  testthat::expect_error(
    env$benchmark_plan(n_write = c(1, 2), n_read = c(1, 2, 3), types = "integer"),
    "matching lengths")
})
