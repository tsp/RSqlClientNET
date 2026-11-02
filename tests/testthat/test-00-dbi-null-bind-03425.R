testthat::test_that("DBI bound SQL NULL remains distinct from empty VARBINARY", {
  encode <- getFromNamespace(".sql_encode_params", "RSqlClientNET")
  split <- getFromNamespace(".dbi_param_sets", "RSqlClientNET")
  raw_values <- list(a = list(as.raw(1:10)),
                     b = list(raw(3)),
                     c = list(NULL),
                     d = list(raw(0)))
  sets <- split(raw_values)
  testthat::expect_length(sets, 1L)
  encoded <- encode(sets[[1L]])
  testthat::expect_identical(names(encoded), names(raw_values))
  testthat::expect_identical(encoded$a$`$sqlclient_type`, "varbinary")
  testthat::expect_identical(encoded$b$`$sqlclient_type`, "varbinary")
  testthat::expect_null(encoded$c)
  testthat::expect_identical(encoded$d$`$sqlclient_type`, "varbinary")
  testthat::expect_identical(encoded$d$value, "")
  # This is exactly the JSON representation consumed by the .NET helper.
  json <- as.character(jsonlite::toJSON(list(params=encoded),
                                auto_unbox=TRUE, null="null", digits=NA))
  parsed <- jsonlite::fromJSON(json, simplifyVector=FALSE)
  testthat::expect_true("c" %in% names(parsed$params))
  testthat::expect_null(parsed$params$c)
  testthat::expect_identical(parsed$params$d$value, "")
  testthat::expect_error(encode(list(a=list(1L, 2L))), "scalar")
})

testthat::test_that("DBItest blob NA is encoded as SQL NULL", {
  testthat::skip_if_not_installed("blob")
  encode <- getFromNamespace(".sql_encode_params", "RSqlClientNET")
  split <- getFromNamespace(".dbi_param_sets", "RSqlClientNET")
  values <- list(
    a = blob::blob(as.raw(1:10)),
    b = blob::blob(raw(3)),
    c = blob::blob(NULL)
  )
  encoded <- encode(split(values)[[1L]])
  testthat::expect_identical(encoded$a$`$sqlclient_type`, "varbinary")
  testthat::expect_identical(encoded$b$`$sqlclient_type`, "varbinary")
  testthat::expect_null(encoded$c)
})
