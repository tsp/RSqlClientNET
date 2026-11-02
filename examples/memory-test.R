library(RSqlClientNET)

# C# -> R test without SQL Server or any temporary SQL data file:
sqlclient_build()
stopifnot(sql_ping()$ok)
test <- sql_memory_test()
print(test)
stopifnot(nrow(test) == 2L,
          identical(test$label[[1L]], "æøå café"),
          identical(test$label[[2L]], "Line 1\nLine 2, \"citat\""),
          is.na(test$nullable[[2L]]))

# SQL Server connection using original SSMS settings if needed:
# con <- sql_connect(connection_string = "Data Source=SERVER;Initial Catalog=DB;Integrated Security=True;Encrypt=False")
# on.exit(sql_disconnect(con))
# dt <- sql_query(con, "SELECT TOP (10) * FROM sys.objects")
# attr(dt, "sqlclient_timings")

# Compare all three parser modes. Corrected fread and read.csv should match.
fast <- sql_memory_test(parser = "fread", quote_correction = TRUE)
uncorrected <- sql_memory_test(parser = "fread", quote_correction = FALSE)
base <- sql_memory_test(parser = "read.csv")
stopifnot(
  identical(fast$label, base$label),
  identical(fast$nullable, base$nullable),
  identical(uncorrected$label[[2L]], "Line 1\nLine 2, \"\"citat\"\"")
)
