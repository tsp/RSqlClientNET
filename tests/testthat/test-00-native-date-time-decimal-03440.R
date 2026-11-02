# SBIN v1 native types 17/18/19: fixed 8-byte doubles and NULL masks.
testthat::test_that("native DATE/TIME/numeric DECIMAL use packed doubles and SQL NULL bitmaps", {
  pack <- getFromNamespace(".dbi_bulk_binary_payload", "RSqlClientNET")
  d <- as.Date(c("1970-01-01", NA, "9999-12-31"))
  tm <- as.difftime(c(0.1234567, NA_real_, 86399.9999999), units="secs")
  money <- c(214748.3647, NA_real_, -214748.3648)
  df <- data.frame(day=d, t=tm, amount=money)
  buf <- pack(df, c("date", "time", "decimal"), 1L, 3L)
  f64 <- function(x) writeBin(as.double(x), raw(), size=8L, endian="little")
  i32 <- function(x) writeBin(as.integer(x), raw(), size=4L, endian="little")
  expected <- c(charToRaw("SBIN"), i32(1L), i32(3L), i32(3L),
    as.raw(17L), as.raw(2L), f64(c(0, 0, as.numeric(d[3L]))),
    as.raw(18L), as.raw(2L), f64(c(0.1234567, 0, 86399.9999999)),
    as.raw(19L), as.raw(2L), f64(c(214748.3647, 0, -214748.3648)))
  testthat::expect_identical(buf, expected)
  exact <- pack(data.frame(a=c("1234567890123456789012345678.1234567890", NA)),
                "decimal", 1L, 2L)
  testthat::expect_identical(exact[17L], as.raw(6L))
  testthat::expect_identical(exact[18L], as.raw(2L))
  testthat::expect_error(pack(data.frame(a=c(1, NaN)), "decimal",1L,2L), "NaN/Inf")
  testthat::expect_error(pack(data.frame(a=c(1, Inf)), "decimal",1L,2L), "NaN/Inf")
  testthat::expect_error(pack(data.frame(t=as.difftime(c(0,86400),units="secs")),
                              "time",1L,2L), "SQL TIME")
  testthat::expect_error(pack(data.frame(t=as.difftime(c(-1,0),units="secs")),
                              "time",1L,2L), "SQL TIME")
  bad <- structure(c(0.5, NA_real_), class="Date")
  testthat::expect_error(pack(data.frame(d=bad),"date",1L,2L),"whole calendar days")
})

testthat::test_that("C# decoder exposes native scalar types with legacy compatibility", {
  source_dirs <- c(file.path("inst","dotnet"), file.path("..","..","inst","dotnet"))
  source_dirs <- source_dirs[file.exists(file.path(source_dirs,"BulkBinaryReader.cs"))]
  source_path <- if(length(source_dirs)) file.path(source_dirs[[1L]],"BulkBinaryReader.cs")
    else system.file("dotnet","BulkBinaryReader.cs",package="RSqlClientNET")
  testthat::expect_true(file.exists(source_path))
  src <- paste(readLines(source_path,warn=FALSE),collapse="\n")
  for (code in c('"date_native" => 17', '"time_native" => 18',
                 '"decimal_native" => 19', '19 => typeof(SqlDecimal)',
                 '20 => typeof(decimal)', 'case 17:', 'case 18:',
                 'case 19:', 'case 20:'))
    testthat::expect_match(src,code,fixed=TRUE)
})
