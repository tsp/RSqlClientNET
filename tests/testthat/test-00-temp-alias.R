test_that("dbplyr SQL #temp identifier gets a session-local logical alias", {
  physical_name <- getFromNamespace(".dbi_local_temp_name", "RSqlClientNET")
  register <- getFromNamespace(".dbi_register_temp", "RSqlClientNET")
  resolve <- getFromNamespace(".dbi_temp_resolve", "RSqlClientNET")
  forget <- getFromNamespace(".dbi_forget_temp", "RSqlClientNET")

  expect_identical(physical_name(DBI::SQL("[#dbi030_copy]")), "#dbi030_copy")
  expect_identical(physical_name(DBI::SQL("#dbi030_copy")), "#dbi030_copy")
  expect_null(physical_name(DBI::SQL("[dbo].[table]")))
  expect_null(physical_name(DBI::SQL("[##global_temp]")))
  expect_null(physical_name(DBI::SQL("[#x]; DROP TABLE t")))

  h <- new.env(parent=emptyenv())
  class(h) <- "sqlclient_connection"
  h$dbi_temp_aliases <- new.env(parent=emptyenv())
  h$dbi_temp_names <- character()
  con <- methods::new("SqlClientNETConnection", handle=h)

  quoted <- DBI::SQL("[#dbi030_copy]")
  register(con, quoted, quoted, temporary=FALSE)  # dbplyr 2.6+ path
  expect_identical(as.character(resolve(con,"dbi030_copy")), "[#dbi030_copy]")
  expect_true(all(c("dbi030_copy","#dbi030_copy") %in% h$dbi_temp_names))
  forget(con,"#dbi030_copy")             # remove by physical name
  expect_identical(resolve(con,"dbi030_copy"), "dbi030_copy")
  expect_length(h$dbi_temp_names,0L)
  register(con, quoted, quoted, temporary=FALSE)
  forget(con,"dbi030_copy")              # remove by logical alias
  expect_identical(resolve(con,"dbi030_copy"), "dbi030_copy")
  expect_length(h$dbi_temp_names,0L)

  # Direct DBI temporary=TRUE remains unchanged.
  register(con,"logical", "#logical", temporary=TRUE)
  expect_identical(resolve(con,"logical"),"#logical")
  forget(con,"logical")
  expect_length(h$dbi_temp_names,0L)
})
