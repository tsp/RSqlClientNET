# Decoder-only regression: runs without SQL Server or the .NET SDK.
library(RSqlClientNET)
le32 <- function(x) writeBin(as.integer(x), raw(), size = 4, endian = "little")
pack_string <- function(x) c(le32(nchar(x, type="bytes")), charToRaw(x))
# 3 rows: id 1, -2^31 (must promote to numeric), and SQL NULL.
# text: empty string, literal quote, and SQL NULL.
# raw: empty binary, three bytes, and SQL NULL.
wire <- c(charToRaw("SCB2"), le32(3), le32(3),
          charToRaw("I"), as.raw(0x03), le32(1), as.raw(c(0,0,0,128)), le32(0),
          charToRaw("S"), as.raw(0x03), pack_string(""), pack_string('"'), le32(0),
          charToRaw("Y"), as.raw(0x03), le32(0), le32(3), as.raw(c(0,1,255)), le32(0))
raw_decoded <- .Call("sqlcpp_decode_batch", wire, FALSE, "UTC", PACKAGE = "RSqlClientNET")
stopifnot(identical(raw_decoded$rows, 3L),
          identical(as.character(raw_decoded$kinds), c("I","S","Y")),
          is.double(raw_decoded$columns[[1]]),
          identical(raw_decoded$columns[[1]][2], -2147483648),
          is.na(raw_decoded$columns[[1]][3]),
          identical(raw_decoded$columns[[2]][1], ""),
          identical(raw_decoded$columns[[2]][2], '"'),
          is.na(raw_decoded$columns[[2]][3]),
          identical(raw_decoded$columns[[3]][[1]], raw(0)),
          identical(raw_decoded$columns[[3]][[2]], as.raw(c(0,1,255))),
          is.null(raw_decoded$columns[[3]][[3]]))
# Malformed / truncated frames must error, not accept partial values.
stopifnot(inherits(try(.Call("sqlcpp_decode_batch", wire[-length(wire)], FALSE, "UTC",
                            PACKAGE="RSqlClientNET"), silent=TRUE), "try-error"),
          inherits(try(.Call("sqlcpp_decode_batch", c(charToRaw("BAD!"),wire[-(1:4)]), FALSE, "UTC",
                            PACKAGE="RSqlClientNET"), silent=TRUE), "try-error"))
