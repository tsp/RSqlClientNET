# Prevent R partial name matching from sending datetime_parser into datetime.
# This test deliberately needs neither SQL Server nor the .NET helper.
test_that("edge fixture uses exact matching for datetime and datetime_parser", {
  parse_opts <- getFromNamespace(".sql_edge_fixture_options", "RSqlClientNET")
  for (parser in c("base", "lubridate")) {
    opts <- parse_opts(list(datetime_parser = parser, precision = "native"),
                       result_format = "columnar")
    expect_identical(opts$datetime_parser, parser)
    expect_identical(opts$datetime, "POSIXct")
    expect_identical(opts$precision, "native")

    full <- parse_opts(list(datetime_parser = parser, precision = "full"),
                       result_format = "csv")
    expect_identical(full$datetime_parser, parser)
    expect_identical(full$datetime, "character")
    expect_identical(full$precision, "full")

    explicit <- parse_opts(list(datetime_parser = parser,
                               datetime = "character", precision = "native"),
                           result_format = "columnar")
    expect_identical(explicit$datetime, "character")
  }
})

test_that("edge fixture rejects invalid, duplicated and unnamed options", {
  parse_opts <- getFromNamespace(".sql_edge_fixture_options", "RSqlClientNET")
  expect_error(parse_opts(list(datetime_parser = "not-a-parser"), "csv"))
  expect_error(parse_opts(list(datetime = "base"), "csv"))
  expect_error(parse_opts(list(datetime_parser = "base", datetime_parser = "base"), "csv"),
               "Duplicate")
  expect_error(parse_opts(list("base"), "csv"), "named")
  expect_error(parse_opts(list(datetime_pars = "base"), "csv"), "Unknown")
})

test_that("edge option parsing never uses partial-dollar matching", {
  withr::local_options(warnPartialMatchDollar = TRUE)
  parse_opts <- getFromNamespace(".sql_edge_fixture_options", "RSqlClientNET")
  expect_silent(parse_opts(
    list(datetime_parser = "base", precision = "native"),
    result_format = "columnar"
  ))
})
