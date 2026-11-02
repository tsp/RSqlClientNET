test_that("method-oriented DBI help pages are included in source", {
  # During pkgload::load_all(), system.file() may point at the source tree.
  # Installed binaries compile Rd into help databases, so skip there.
  src <- system.file(package = "RSqlClientNET")
  man <- file.path(src, "man")
  if (!dir.exists(man)) skip("Source man/ directory not available")
  for (topic in c("RSqlClientNET-db-connect", "RSqlClientNET-db-query",
                  "RSqlClientNET-db-result", "RSqlClientNET-db-write",
                  "RSqlClientNET-db-read", "RSqlClientNET-db-transaction")) {
    path <- file.path(man, paste0(topic, ".Rd"))
    expect_true(file.exists(path), info = topic)
    if (file.exists(path)) expect_silent(tools::parse_Rd(path))
  }
})


test_that("query and table-read docs list identical conversion overrides", {
  src <- system.file(package = "RSqlClientNET")
  man <- file.path(src, "man")
  if (!dir.exists(man)) skip("Source man/ directory not available")
  topics <- file.path(man, c("RSqlClientNET-db-query.Rd", "RSqlClientNET-db-read.Rd"))
  tokens <- c("precision", "bigint", "datetime", "binary", "unsupported",
              "tz", "datetime_parser", "dst_fallback", "data_table",
              "batch_rows", "timeout")
  for (path in topics) {
    rd <- paste(readLines(path, warn = FALSE), collapse = "\n")
    expect_match(rd, "Per-query read options", fixed = TRUE)
    for (opt in tokens)
      expect_true(grepl(paste0("\\code{", opt, "}"), rd, fixed = TRUE),
                  info = paste(basename(path), opt))
  }
})
