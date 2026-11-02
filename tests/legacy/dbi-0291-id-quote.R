# SQL Server/DBI identifier quoting, without opening an actual SQL connection.
library(DBI)
library(RSqlClientNET)
h <- new.env(parent=emptyenv())
class(h) <- 'sqlclient_connection'
h$closed <- TRUE
h$transport <- 'rcpp'
h$dbi_info <- list(db.version='16.0.1000.6', dbname='test',
                   username='test', host=NA_character_, port=NA_integer_)
x <- methods::new('SqlClientNETConnection', handle=h)
quoted <- function(y) as.character(DBI::dbQuoteIdentifier(x,y))
stopifnot(
  identical(quoted(DBI::Id(schema='dbo', table='TypeCases')),
            '[dbo].[TypeCases]'),
  identical(quoted(DBI::Id(table='TypeCases', schema='dbo')),
            '[dbo].[TypeCases]'),
  identical(quoted(DBI::Id(schema='my]schema', table='a.b')),
            '[my]]schema].[a.b]'),
  identical(quoted('col]umn'), '[col]]umn]'),
  identical(quoted(DBI::SQL('[dbo].[TypeCases]')),
            '[dbo].[TypeCases]')
)
message('RSqlClientNET 0.0.29.1 quoting regression passed')
