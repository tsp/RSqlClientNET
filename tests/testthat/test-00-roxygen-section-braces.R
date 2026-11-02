test_that("roxygen @section braces remain balanced", {
  source_file <- system.file("R", "zzz-documentation.R", package = "RSqlClientNET")
  if (!file.exists(source_file)) skip("Original roxygen source not installed")

  lines <- readLines(source_file, warn = FALSE, encoding = "UTF-8")
  starts <- grep("^#' @section ", lines)
  expect_gt(length(starts), 0L)
  for (start in starts) {
    end <- start
    while (end < length(lines) && startsWith(lines[end + 1L], "#'") &&
           !grepl("^#' @[A-Za-z]", lines[end + 1L])) end <- end + 1L
    section <- paste(lines[start:end], collapse = "\n")
    chars <- strsplit(section, "", fixed = TRUE)[[1L]]
    depth <- cumsum((chars == "{") - (chars == "}"))
    expect_true(all(depth >= 0), info = paste("Unmatched '}' near line", start))
    expect_equal(tail(depth, 1L), 0L,
                 info = paste("Unmatched '{' near line", start))
  }
})
