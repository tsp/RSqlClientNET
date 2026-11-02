# Native SBIN encoder tests: no SQL Server / .NET service required.
testthat::test_that("SBIN bulk input preserves header, column kinds and missing bits", {
  pack <- getFromNamespace(".dbi_bulk_binary_payload","RSqlClientNET")
  frame <- data.frame(i=c(1L,NA_integer_,-7L), d=c(1.5,NA_real_,0.0),
                      b=c(TRUE,NA,FALSE), s=c("æøå","",NA_character_),
                      stringsAsFactors=FALSE)
  frame$raw <- I(list(as.raw(c(0L,255L)),raw(0),NULL))
  kinds <- c("integer","double","logical","text","binary")
  encoded <- pack(frame,kinds,1L,3L)
  testthat::expect_type(encoded,"raw")
  testthat::expect_identical(rawToChar(encoded[1:4]),"SBIN")
  integer_at <- function(pos) as.integer(readBin(encoded[pos:(pos+3L)],"integer",n=1L,size=4L,endian="little",signed=TRUE))
  testthat::expect_identical(integer_at(5L),1L)
  testthat::expect_identical(integer_at(9L),3L)
  testthat::expect_identical(integer_at(13L),5L)
  # First column starts immediately after 16-byte header.
  testthat::expect_identical(as.integer(encoded[17L]),1L)
  testthat::expect_identical(as.integer(encoded[18L]),2L) # second row null
  testthat::expect_identical(integer_at(19L),1L)
  testthat::expect_identical(integer_at(23L),0L)
  testthat::expect_identical(integer_at(27L),-7L)
  # Empty, missing and unicode strings remain separate encodings.
  testthat::expect_true(any(encoded == as.raw(0xff)))
  testthat::expect_true(length(encoded)>60L)
  testthat::expect_error(pack(frame,kinds,1L,4L))
})

testthat::test_that("SBIN encoder preserves exact integer64 bits and NA bitmap", {
  pack <- getFromNamespace(".dbi_bulk_binary_payload","RSqlClientNET")
  x <- data.frame(big=bit64::as.integer64(c("9223372036854775807",NA,"-2")))
  payload <- pack(x,"integer64",1L,3L)
  testthat::expect_identical(as.integer(payload[17L]),2L)
  testthat::expect_identical(as.integer(payload[18L]),2L)
  testthat::expect_identical(payload[19:26],as.raw(c(rep(0xff,7L),0x7f)))
})

testthat::test_that("SBIN rejects nonfinite SQL FLOAT", {
  pack <- getFromNamespace(".dbi_bulk_binary_payload","RSqlClientNET")
  testthat::expect_error(pack(data.frame(x=c(1,NaN)),"double",1L,2L),"NaN")
  testthat::expect_error(pack(data.frame(x=c(1,Inf)),"double",1L,2L),"Inf")
})


testthat::test_that("SBIN fixed-width bulk columns retain little-endian bytes and NA masks", {
  pack <- getFromNamespace(".dbi_bulk_binary_payload", "RSqlClientNET")
  vals <- data.frame(a=c(1L,NA_integer_,-2147483647L),
                     b=c(1.25,NA_real_,-3.5),
                     c=c(FALSE,TRUE,NA))
  payload <- pack(vals,c("integer","double","logical"),1L,3L)
  int_le <- function(x) writeBin(x,raw(),size=4L,endian="little")
  double_le <- function(x) writeBin(x,raw(),size=8L,endian="little")
  expected <- c(charToRaw("SBIN"), int_le(1L),int_le(3L),int_le(3L),
                as.raw(1L),as.raw(2L),
                int_le(1L),int_le(0L),int_le(-2147483647L),
                as.raw(3L),as.raw(2L),
                double_le(1.25),double_le(0),double_le(-3.5),
                as.raw(4L),as.raw(4L),as.raw(c(0L,1L,0L)))
  testthat::expect_identical(payload,expected)
  testthat::expect_true(is.na(vals$b[[2L]]))
})

testthat::test_that("SBIN can profile R preparation versus native packing", {
  pack <- getFromNamespace(".dbi_bulk_binary_payload", "RSqlClientNET")
  old <- getOption("RSqlClientNET.bulk_profile_stages")
  on.exit(options(RSqlClientNET.bulk_profile_stages=old),add=TRUE)
  options(RSqlClientNET.bulk_profile_stages=TRUE)
  testthat::expect_message(
    pack(data.frame(x=c(1L,NA_integer_)),"integer",1L,2L),
    "R column prep.*Rcpp packing"
  )
})


testthat::test_that("SBIN writes native POSIXct seconds with NULL bitmap and both datetime kinds", {
  pack <- getFromNamespace(".dbi_bulk_binary_payload", "RSqlClientNET")
  z <- as.POSIXct(c("1970-01-01 00:00:00", "2026-10-20 12:30:00", NA), tz="UTC")
  sample <- data.frame(t=z, tz=z)
  kinds <- c("datetime", "datetimeoffset")
  got <- pack(sample,kinds,1L,3L)
  i32 <- function(x) writeBin(as.integer(x),raw(),size=4L,endian="little")
  f64 <- function(x) writeBin(as.double(x),raw(),size=8L,endian="little")
  rawseconds <- c(0,as.numeric(z[2L]),0)
  exp <- c(charToRaw("SBIN"),i32(1L),i32(3L),i32(2L),
           as.raw(12L),as.raw(4L),f64(rawseconds),
           as.raw(13L),as.raw(4L),f64(rawseconds))
  testthat::expect_identical(got,exp)
  # Negative epoch fractions and NA values must remain exact IEEE doubles.
  neg <- structure(c(-0.25, NA_real_, 1.000001),class=c("POSIXct","POSIXt"),tzone="UTC")
  testthat::expect_identical(pack(data.frame(t=neg),"datetime",1L,3L)[17L],as.raw(12L))
  testthat::expect_error(pack(data.frame(t=structure(Inf,class=c("POSIXct","POSIXt"))),
                              "datetime",1L,1L),"Infinite")
})
