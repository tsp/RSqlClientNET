# Pure unit tests for adaptive SBIN framing (no SQL Server / .NET needed).
testthat::test_that("SBIN wide batches halve safely without losing or repeating rows", {
  fit <- getFromNamespace(".dbi_bulk_encode_fitting", "RSqlClientNET")
  dat <- data.frame(value = seq_len(17L))
  recorded <- list()
  encode <- function(data, kinds, first, last) {
    recorded[[length(recorded) + 1L]] <<- c(first, last)
    if (last - first + 1L > 4L)
      stop("Binary bulk batch exceeds 128 MiB; decrease batch_rows.", call. = FALSE)
    as.raw(seq_len(last - first + 1L))
  }
  first <- 1L
  limit <- 16L
  slices <- list()
  while (first <= nrow(dat)) {
    out <- fit(dat, "integer", first, limit, encode = encode)
    testthat::expect_type(out$payload, "raw")
    testthat::expect_identical(length(out$payload), out$rows)
    slices[[length(slices) + 1L]] <- seq.int(first, out$last)
    limit <- min(limit, out$rows)
    first <- out$last + 1L
  }
  testthat::expect_identical(unlist(slices, use.names = FALSE), seq_len(17L))
  testthat::expect_identical(recorded[[1L]], c(1L, 16L))
  testthat::expect_identical(recorded[[2L]], c(1L, 8L))
  testthat::expect_identical(recorded[[3L]], c(1L, 4L))
  testthat::expect_true(all(vapply(recorded[-(1L:3L)],
                                    function(x) x[[2L]] - x[[1L]] < 4L,
                                    logical(1L))))
})

testthat::test_that("SBIN adaptive framing never swallows unrelated errors", {
  fit <- getFromNamespace(".dbi_bulk_encode_fitting", "RSqlClientNET")
  dat <- data.frame(x = 1:8)
  count <- 0L
  bad <- function(...) {
    count <<- count + 1L
    stop("Unsupported bulk column type", call. = FALSE)
  }
  testthat::expect_error(fit(dat, "integer", 1L, 8L, encode = bad),
                         "Unsupported bulk column type")
  testthat::expect_identical(count, 1L)
})

testthat::test_that("SBIN oversized individual row fails clearly", {
  fit <- getFromNamespace(".dbi_bulk_encode_fitting", "RSqlClientNET")
  too_big <- function(...) stop(
    "Binary bulk batch exceeds 128 MiB; decrease batch_rows.", call. = FALSE)
  testthat::expect_error(
    fit(data.frame(x = 1:3), "integer", 1L, 3L, encode = too_big),
    "One binary bulk row exceeds the 128 MiB")
})

testthat::test_that("SBIN fitting passes through ordinary batches unchanged", {
  fit <- getFromNamespace(".dbi_bulk_encode_fitting", "RSqlClientNET")
  frame <- data.frame(x = rep(c("abc", "æøå", NA_character_), 10L))
  original <- getFromNamespace(".dbi_bulk_binary_payload", "RSqlClientNET")
  result <- fit(frame, "text", 1L, 30L)
  testthat::expect_identical(result$rows, 30L)
  testthat::expect_identical(result$last, 30L)
  testthat::expect_identical(result$payload, original(frame, "text", 1L, 30L))
})
