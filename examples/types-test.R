library(RSqlClientNET)

# Requires only the local .NET helper; never contacts SQL Server.
t <- sql_types_test()
print(sql_schema(t))
str(t)
stopifnot(
  identical(as.character(t$big[1]), "9223372036854775807"),
  identical(t$exact_decimal[1], "1234567890123456789012345678.1234567890"),
  identical(t$code[1], "000123"),
  identical(t$empty_text[1], ""),
  identical(t$nullable_text[1], "NA"),
  is.na(t$nullable_text[3]),
  identical(t$blob[[1]], as.raw(c(0, 1, 255))),
  identical(t$empty_blob[[1]], raw(0)),
  is.null(t$nullable_blob[[1]])
)

# Same payload using base::read.csv to independently validate CSV escaping.
t2 <- sql_types_test(parser = "read.csv")
stopifnot(identical(t$code, t2$code),
          identical(t$exact_decimal, t2$exact_decimal))

# Example of typed SQL parameters (requires a SQL Server connection):
# sql_query(con, "SELECT @id AS id, @amount AS amount", params = list(
#   id = sql_param("9007199254740993", "bigint"),
#   amount = sql_param("12345678901234567890.12345", "decimal", 25, 5)
# ))
