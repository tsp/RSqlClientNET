library(RSqlClientNET)
parse <- getFromNamespace('.sql_parse_csv', 'RSqlClientNET')
header <- list(types = c('int', 'nvarchar', 'nvarchar'),
               columns = c('id', 'label', 'nullable'),
               valueEncoding = 'tagged-v1')
# A quoted V-prefix, N for NULL; literal N and NA are not NULL.
csv <- paste0('"id","label","nullable"\n',
  '"V1","Væøå, ""citat""\nNy linje","V"\n',
  '"V2","VNA",N\n',
  '"V3","VN","V"\n')
x <- parse(csv, header, parser = 'fread', quote_correction = TRUE)
y <- parse(csv, header, parser = 'read.csv', quote_correction = TRUE)
stopifnot(identical(x$label, c('æøå, "citat"\nNy linje', 'NA', 'N')),
          identical(y$label, x$label), identical(x$nullable, c('', NA_character_, '')),
          identical(x$id, 1:3), identical(sql_schema(x)$sql_type, header$types))
# Full end-to-end memory-only fixture (requires compiled .NET helper).
if (nzchar(Sys.which('dotnet'))) {
  z <- try(sql_types_test(), silent = TRUE)
  if (!inherits(z, 'try-error')) {
    stopifnot(nrow(z) == 3L, inherits(z$big, 'integer64'),
              identical(as.character(z$big[1]), '9223372036854775807'),
              identical(z$code[[1]], '000123'),
              identical(z$code[[2]], '"citat"'),
              identical(z$empty_text[[1]], ''),
              identical(z$nullable_text[[1]], 'NA'),
              is.na(z$nullable_text[[3]]),
              identical(z$blob[[1]], as.raw(c(0,1,255))),
              identical(z$empty_blob[[1]], raw(0)),
              is.null(z$nullable_blob[[1]]),
              identical(z$exact_decimal[[1]], '1234567890123456789012345678.1234567890'),
              identical(z$stamp[[1]], '2026-10-16T12:34:56.1234567'))
  }
}
