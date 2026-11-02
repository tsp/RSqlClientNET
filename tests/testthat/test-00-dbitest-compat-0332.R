test_that("DBItest-compatible bigint and SQL types", {
  options <- getFromNamespace(".sql_precision_options","RSqlClientNET")
  for (mode in c("integer64","character","numeric","integer"))
    expect_identical(options("native",bigint=mode)$bigint,mode)
  sql_type <- getFromNamespace(".dbi_sql_type","RSqlClientNET")
  expect_identical(sql_type(factor(c("a","b"))),"NVARCHAR(MAX)")
  expect_identical(sql_type(as.difftime(2,units="secs")),"TIME(7)")
  expect_identical(sql_type(list(as.raw(1),raw(0),NULL)),"VARBINARY(MAX)")
  expect_identical(unname(sql_type(data.frame(a=1L,b="x"))),
                   c("INT","NVARCHAR(MAX)"))
})
