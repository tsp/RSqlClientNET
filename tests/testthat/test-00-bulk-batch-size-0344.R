testthat::test_that("SBIN upload has an independent row limit from SQL reads", {
  validate <- getFromNamespace(".dbi_max_bulk_batch", "RSqlClientNET")
  old_read <- getFromNamespace(".dbi_max_batch", "RSqlClientNET")
  testthat::expect_identical(validate(65536L),65536L)
  testthat::expect_identical(validate(131072L),131072L)
  testthat::expect_error(validate(131073L),"131072")
  testthat::expect_error(validate(0L),"131072")
  testthat::expect_error(validate(1.5),"131072")
  testthat::expect_error(validate(NA_real_),"131072")
  testthat::expect_error(validate(c(1L,2L)),"131072")
  testthat::expect_identical(old_read(32768L),32768L)
  testthat::expect_error(old_read(65536L),"32768")
})

testthat::test_that("SBIN encodes a 131072-row multi-column upload batch", {
  pack <- getFromNamespace(".dbi_bulk_binary_payload", "RSqlClientNET")
  n <- 131072L
  dat <- data.frame(id=seq_len(n), val=rep(c(1.5,NA_real_),length.out=n),
                    label=rep(c("abc", "æøå", NA_character_),length.out=n),
                    stringsAsFactors=FALSE)
  blob <- pack(dat,c("integer","double","text"),1L,n)
  testthat::expect_type(blob,"raw")
  testthat::expect_identical(rawToChar(blob[1:4]),"SBIN")
  read_u32 <- function(index) readBin(blob[index:(index+3L)],what="integer",n=1L,size=4L,
                                     signed=TRUE,endian="little")
  testthat::expect_identical(read_u32(9L),n)
  testthat::expect_identical(read_u32(13L),3L)
  testthat::expect_true(length(blob) < 128*1024*1024)
})


testthat::test_that("SBIN is the only upload transport and defaults to 65536 rows", {
  default <- getFromNamespace(".dbi_default_bulk_batch", "RSqlClientNET")
  validate <- getFromNamespace(".dbi_max_bulk_batch", "RSqlClientNET")
  testthat::expect_identical(default(), 65536L)
  testthat::expect_identical(validate(default()), 65536L)
})

testthat::test_that("bit64 methods are registered without attaching bit64", {
  testthat::expect_true("bit64" %in% loadedNamespaces())
  testthat::expect_true(is.function(getS3method("as.character", "integer64", optional=TRUE)))
  testthat::expect_identical(as.character(bit64::as.integer64("336776")), "336776")
})
