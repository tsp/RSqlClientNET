test_that("installed dotnet published helper is preferred without requiring SDK", {
  helper <- getFromNamespace(".sql_bundled_helper", "RSqlClientNET")()
  expect_type(helper, "character")
  expect_length(helper, 1L)
  if (nzchar(helper)) {
    old <- getOption("RSqlClientNET.helper_override")
    on.exit(options(RSqlClientNET.helper_override = old), add = TRUE)
    options(RSqlClientNET.helper_override = NULL)
    expect_identical(RSqlClientNET::sqlclient_build(), helper)
    expect_true(file.exists(helper))
  }
})

test_that("helper force argument is validated", {
  expect_error(RSqlClientNET::sqlclient_build(force = NA), "force must")
  expect_error(RSqlClientNET::sqlclient_build(force = 1), "force must")
})
