# 1. Install dependencies
install.packages(c("data.table", "jsonlite", "processx"))
# 2. In R, install this locally (once):
# install.packages("path/to/RSqlClientNET", repos = NULL, type = "source")

library(RSqlClientNET)

# First call compiles the embedded C# helper using .NET SDK 8+
sqlclient_build()

con <- sql_connect(
  server = "SERVERNAVN",
  database = "DATABASENAVN",
  integrated_security = TRUE,
  encrypt = TRUE,
  trust_server_certificate = FALSE
)

sql_query(con, "SELECT @@VERSION AS version")

# Parameter binding avoids building SQL with paste0() and untrusted values.
dt <- sql_query(con, "SELECT * FROM dbo.MinTabel WHERE Id > @minId",
                params = list(minId = 100L))

# SELECT / INSERT / UPDATE / DELETE; affected rows, often -1 for DDL.
n <- sql_execute(con, "UPDATE dbo.MinTabel SET status = @status WHERE Id = @id",
                 params = list(status = 1L, id = 123L))

sql_disconnect(con)
