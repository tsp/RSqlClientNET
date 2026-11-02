# Offline SQL Server DBI S4 dispatch checks, no server required.
library(DBI)
library(RSqlClientNET)
h <- new.env(parent=emptyenv())
class(h) <- "sqlclient_connection"
h$closed <- TRUE
h$transport <- "rcpp"
h$dbi_info <- list(db.version="16.0.1000.6", dbname="test",
                   username="test", host=NA_character_, port=NA_integer_)
con <- methods::new("SqlClientNETConnection", handle=h)

# Explicit S4 registration must override DBI defaults for BOTH argument types.
for (typ in c("Id", "character", "SQL")) {
  m <- methods::selectMethod("dbQuoteIdentifier", c("SqlClientNETConnection", typ))
  stopifnot(identical(unname(as.character(m@defined)), c("SqlClientNETConnection",typ)))
}
for (typ in c("character", "SQL")) {
  for (generic in c("dbQuoteString")) {
    m <- methods::selectMethod(generic, c("SqlClientNETConnection",typ))
    stopifnot(identical(unname(as.character(m@defined)), c("SqlClientNETConnection",typ)))
  }
}
for (generic in c("dbSendStatement", "dbSendQuery")) {
  for (typ in c("character", "SQL")) {
    m <- methods::selectMethod(generic,c("SqlClientNETConnection",typ))
    stopifnot(identical(unname(as.character(m@defined)),c("SqlClientNETConnection",typ)))
  }
}
quote <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
stopifnot(
  identical(quote(DBI::Id(schema="dbo",table="TypeCases")),"[dbo].[TypeCases]"),
  identical(quote(DBI::Id(table="TypeCases",schema="dbo")),"[dbo].[TypeCases]"),
  identical(quote(DBI::Id(schema="my]schema",table="a.b")),"[my]]schema].[a.b]"),
  identical(quote("x]y"),"[x]]y]"),
  identical(quote(DBI::SQL("[dbo].[TypeCases]")),"[dbo].[TypeCases]"),
  identical(quote(DBI::dbQuoteIdentifier(con,"x]y")),"[x]]y]"),
  identical(as.character(DBI::dbQuoteString(con,"O'Brien")),"N'O''Brien'"),
  identical(as.character(DBI::dbQuoteString(con,DBI::SQL("N'raw'"))),"N'raw'")
)
message("RSqlClientNET 0.0.29.2 DBI method dispatch and quoting passed")
