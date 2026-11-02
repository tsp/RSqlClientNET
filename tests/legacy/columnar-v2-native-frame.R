# Native SCB2 decoding including NULL bitmap and class metadata.
# Requires installed package with compiled Rcpp DLL, no SQL Server or dotnet.
library(RSqlClientNET)
le32 <- function(x) writeBin(as.integer(x), raw(), size=4L, endian="little")
le64 <- function(x) writeBin(unclass(bit64::as.integer64(x)), raw(), size=8L,
                            endian="little")
ldouble <- function(x) writeBin(as.double(x), raw(), size=8L, endian="little")
# two rows; second row SQL NULL in all columns
header <- c(charToRaw("SCB2"), le32(2L), le32(6L))
wire <- c(header,
  charToRaw("L"), as.raw(1), le64("9007199254740993"), le64("0"),
  charToRaw("A"), as.raw(1), le32(0), le32(0),
  charToRaw("H"), as.raw(1), le64("452961234567"), le64("0"),
  charToRaw("T"), as.raw(1), le64("621355968000000000"), le64("0"),
  charToRaw("O"), as.raw(1), le64("621355968000000000"), le64("0"),
  charToRaw("N"), as.raw(1), ldouble(12.34), ldouble(0))
call <- function(bigint64=TRUE, tz="UTC", payload=wire)
  .Call("sqlcpp_decode_batch", payload, bigint64, tz, PACKAGE="RSqlClientNET")
d <- call()
stopifnot(identical(as.character(d$kinds), c("L","A","H","T","O","N")),
          identical(d$rows, 2L),
          inherits(d$columns[[1]], "integer64"),
          identical(as.character(d$columns[[1]][1]), "9007199254740993"),
          is.na(d$columns[[1]][2]),
          inherits(d$columns[[2]], "Date"),
          identical(as.character(d$columns[[2]][1]), "1970-01-01"),
          is.na(d$columns[[2]][2]),
          inherits(d$columns[[3]], "difftime"),
          abs(as.numeric(d$columns[[3]][1], units="secs") - 45296.1234567) < 1e-8,
          inherits(d$columns[[4]], "POSIXct"),
          identical(attr(d$columns[[4]], "tzone"), "UTC"),
          identical(as.numeric(d$columns[[4]][1]), 0),
          inherits(d$columns[[5]], "POSIXct"),
          identical(as.numeric(d$columns[[5]][1]), 0),
          identical(d$columns[[6]][1], 12.34),
          is.na(d$columns[[6]][2]))
d2 <- call(FALSE)
stopifnot(is.character(d2$columns[[1]]),
          identical(d2$columns[[1]][1], "9007199254740993"),
          is.na(d2$columns[[1]][2]))
# SQL -2^63 must never be confused with bit64 missing-value sentinel.
pos <- 14L # after 12-byte header + 1-byte kind + 1-byte validity
wire_min <- wire
# The bit64 package reserves signed -2^63 as NA and warns when converting
# that text to integer64. Encode the exact two's-complement bits instead.
# Little-endian signed 64-bit minimum: 0x8000000000000000.
wire_min[(pos + 1L):(pos + 8L)] <- as.raw(c(rep(0L, 7L), 0x80L))
min_decoded <- call(TRUE, payload=wire_min)
stopifnot(is.na(min_decoded$columns[[1]][1]),
          identical(min_decoded$bigint_min_counts[[1]], 1L),
          identical(min_decoded$bigint_min_rows[[1]], 1L),
          identical(call(FALSE, payload=wire_min)$columns[[1]][1], "-9223372036854775808"),
          inherits(try(call(TRUE, tz="Europe/Copenhagen"), silent=TRUE), "try-error"),
          inherits(try(call(TRUE, payload=wire[-length(wire)]), silent=TRUE), "try-error"))
