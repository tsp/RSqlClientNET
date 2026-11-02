testthat::test_that("SCB2 repeated/unique binary values keep R copy-on-modify and NULL semantics", {
  i32 <- function(x) writeBin(as.integer(x), raw(), size=4L, endian="little")
  # Two identical blobs, SQL NULL, empty blob, a unique blob, same as first.
  # Valid bitmap 0b111011 = 59 for six rows, with only row 3 NULL.
  payload <- c(charToRaw("SCB2"), i32(6L), i32(1L), charToRaw("Y"),
               as.raw(0x3b),
               i32(2L), charToRaw("AB"),
               i32(2L), charToRaw("AB"),
               i32(0L),
               i32(0L),
               i32(1L), as.raw(0xff),
               i32(2L), charToRaw("AB"))
  got <- .Call("sqlcpp_decode_batch", payload, FALSE, "UTC", PACKAGE="RSqlClientNET")
  testthat::expect_identical(as.character(got$kinds), "Y")
  out <- got$columns[[1L]]
  testthat::expect_length(out, 6L)
  testthat::expect_identical(out[[1L]], charToRaw("AB"))
  testthat::expect_identical(out[[2L]], charToRaw("AB"))
  testthat::expect_null(out[[3L]])
  testthat::expect_identical(out[[4L]], raw(0))
  testthat::expect_identical(out[[5L]], as.raw(255L))
  testthat::expect_identical(out[[6L]], charToRaw("AB"))
  # Reusing the same raw SEXP must not leak mutations between rows.
  out[[1L]][1L] <- as.raw(0L)
  testthat::expect_identical(out[[2L]], charToRaw("AB"))
  testthat::expect_identical(out[[6L]], charToRaw("AB"))
})

testthat::test_that("SCB2 XML UTF-8 and SQL NULL retain text and encoding", {
  i32 <- function(x) writeBin(as.integer(x), raw(), size=4L, endian="little")
  doc <- enc2utf8("<root>æøå &amp; 漢字 🙂</root>")
  txt <- charToRaw(doc)
  payload <- c(charToRaw("SCB2"), i32(3L), i32(1L), charToRaw("S"),
               as.raw(0x03), i32(length(txt)), txt,
               i32(length(txt)), txt, i32(0L))
  decoded <- .Call("sqlcpp_decode_batch",payload,FALSE,"UTC",PACKAGE="RSqlClientNET")
  testthat::expect_identical(as.character(decoded$kinds),"S")
  testthat::expect_identical(as.character(decoded$columns[[1L]][1L]),doc)
  testthat::expect_identical(as.character(decoded$columns[[1L]][2L]),doc)
  testthat::expect_true(is.na(decoded$columns[[1L]][3L]))
})

testthat::test_that("SCB2 binary null frames and invalid payloads are rejected", {
  i32 <- function(x) writeBin(as.integer(x), raw(), size=4L, endian="little")
  decode <- function(p) .Call("sqlcpp_decode_batch", p, FALSE, "UTC", PACKAGE="RSqlClientNET")
  bad_null <- c(charToRaw("SCB2"), i32(1L), i32(1L), charToRaw("Y"),
                as.raw(0), i32(1L), as.raw(7L))
  testthat::expect_error(decode(bad_null), "Nonempty payload for SQL NULL binary")
  truncated <- c(charToRaw("SCB2"), i32(1L), i32(1L), charToRaw("Y"),
                 as.raw(1L), i32(10L), as.raw(7L))
  testthat::expect_error(decode(truncated), "Truncated SCB2 batch")
})

testthat::test_that("binary_unique benchmark creates varying binary values with same SQL schema", {
  paths <- c(file.path("tools","benchmarks","benchmark_dbi.R"),
             file.path("..","..","tools","benchmarks","benchmark_dbi.R"))
  paths <- paths[file.exists(paths)]
  testthat::skip_if(!length(paths), "tools/benchmarks source not installed")
  e <- new.env(parent=globalenv())
  sys.source(paths[[1L]], envir=e)
  testthat::skip_if_not_installed("blob")
  plan <- e$benchmark_plan(n=5L, types=c("binary", "binary_unique"),
                           binary_sizes=16L, null_fractions=0)
  a <- e$.bench_make_data(plan[plan$type=="binary",,drop=FALSE])
  b <- e$.bench_make_data(plan[plan$type=="binary_unique",,drop=FALSE])
  testthat::expect_identical(e$.bench_schema(plan[1,,drop=FALSE]),
                             e$.bench_schema(plan[2,,drop=FALSE]))
  testthat::expect_identical(unclass(a$value)[[1L]], unclass(a$value)[[2L]])
  testthat::expect_false(identical(unclass(b$value)[[1L]], unclass(b$value)[[2L]]))
})

testthat::test_that(".NET serializer uses pooled UTF-8 encoding for XML and NVARCHAR", {
  paths <- c(file.path("inst","dotnet","Program.cs"),
             file.path("..","..","inst","dotnet","Program.cs"))
  paths <- paths[file.exists(paths)]
  if (!length(paths)) {
    location <- system.file("dotnet","Program.cs",package="RSqlClientNET")
    if (nzchar(location)) paths <- location
  }
  testthat::expect_true(length(paths) > 0L)
  src <- paste(readLines(paths[[1L]], warn=FALSE),collapse="\n")
  testthat::expect_match(src, "WriteUtf8ColumnarField(writer, value, ref utf8Scratch)",fixed=TRUE)
  testthat::expect_match(src, "ArrayPool<byte>.Shared.Return(utf8Scratch)",fixed=TRUE)
})
