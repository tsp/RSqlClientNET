#' Diagnose all synthetic SQL edge-type comparisons without stopping at the first failure
#'
#' Compares the CSV and SCB2/columnar decoding paths column by column, collecting
#' errors and warnings independently for every precision, timezone and parser.
#' The fixture is produced by the local .NET helper; SQL Server is not needed.
#' The result always contains all attempted scenarios, even if a query fails.
#'
#' @param timezones IANA zones to test; default UTC and Europe/Copenhagen.
#' @param precisions 'full' and/or 'native'.
#' @param datetime_parsers One or both of 'lubridate' and 'base'.
#' @param csv_parsers One or both of 'fread' and 'read.csv'.
#' @param batch_rows Binary batch size for the columnar fixture.
#' @param tolerance Comparison tolerance for floating-point values only.
#' @return Data frame with one result per SQL column and scenario, plus
#'   scenario-level ERROR rows when the result could not be decoded.
#' @keywords internal
#' @noRd
sql_edge_types_report <- function(
    timezones = c("UTC", "Europe/Copenhagen"),
    precisions = c("full", "native"),
    datetime_parsers = c("lubridate", "base"),
    csv_parsers = "fread",
    batch_rows = 2L, tolerance = 1e-6) {
  if (!length(timezones) || !length(precisions) ||
      !length(datetime_parsers) || !length(csv_parsers))
    stop("Each test dimension must contain at least one value.", call. = FALSE)
  for (tz in timezones) .sql_validate_tz(tz)
  if (any(!precisions %in% c("full", "native")) ||
      any(!datetime_parsers %in% c("lubridate", "base")) ||
      any(!csv_parsers %in% c("fread", "read.csv")))
    stop("Unknown precision, datetime parser, or CSV parser.", call. = FALSE)
  combinations <- expand.grid(
    precision = precisions, tz = timezones,
    datetime_parser = datetime_parsers, csv_parser = csv_parsers,
    stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  rows <- list()
  append_row <- function(settings, column, sql_type, status, details,
                         csv_warnings, columnar_warnings) {
    rows[[length(rows) + 1L]] <<- data.frame(
      precision = settings$precision, tz = settings$tz,
      datetime_parser = settings$datetime_parser,
      csv_parser = settings$csv_parser,
      column = column, sql_type = sql_type, status = status,
      details = details,
      csv_warnings = paste(unique(csv_warnings), collapse = " | "),
      columnar_warnings = paste(unique(columnar_warnings), collapse = " | "),
      stringsAsFactors = FALSE)
  }
  fetch <- function(settings, result_format) {
    caught <- character()
    value <- tryCatch(
      withCallingHandlers(
        sql_edge_types_test(
          result_format = result_format, precision = settings$precision,
          datetime_parser = settings$datetime_parser,
          parser = settings$csv_parser, bigint = "character", tz = settings$tz,
          batch_rows = batch_rows, transport = "rcpp"),
        warning = function(w) {
          caught <<- c(caught, conditionMessage(w))
          invokeRestart("muffleWarning")
        }),
      error = function(e) e)
    list(value = value, warnings = caught)
  }
  for (i in seq_len(nrow(combinations))) {
    settings <- combinations[i, , drop = FALSE]
    csv <- fetch(settings, "csv")
    col <- fetch(settings, "columnar")
    if (inherits(csv$value, "error") || inherits(col$value, "error")) {
      failures <- c(
        if (inherits(csv$value, "error"))
          paste0("CSV: ", conditionMessage(csv$value)),
        if (inherits(col$value, "error"))
          paste0("columnar: ", conditionMessage(col$value)))
      append_row(settings, "<query>", "", "ERROR",
                 paste(failures, collapse = " | "),
                 csv$warnings, col$warnings)
      next
    }
    a <- csv$value
    b <- col$value
    schema_a <- sql_schema(a)
    schema_b <- sql_schema(b)
    if (!identical(schema_a, schema_b)) {
      append_row(settings, "<schema>", "", "FAIL",
                 paste(all.equal(schema_a, schema_b), collapse = "; "),
                 csv$warnings, col$warnings)
    }
    all_columns <- union(names(a), names(b))
    for (name in all_columns) {
      if (!name %in% names(a) || !name %in% names(b)) {
        append_row(settings, name, "", "FAIL", "Column missing in one format",
                   csv$warnings, col$warnings)
        next
      }
      ai <- a[[name]]
      bi <- b[[name]]
      typ <- schema_a$sql_type[match(name, schema_a$name)]
      exact <- is.character(ai) || is.list(ai) || is.integer(ai) ||
               is.logical(ai) || inherits(ai, "integer64")
      same_class <- identical(class(ai), class(bi))
      same_na <- identical(is.na(ai), is.na(bi))
      same_value <- if (exact) identical(ai, bi) else
        isTRUE(all.equal(ai, bi, tolerance = tolerance, check.attributes = TRUE))
      errors <- character()
      if (!same_class) errors <- c(errors, "R classes differ")
      if (!same_na) errors <- c(errors, "NA positions differ")
      if (!same_value) {
        errors <- c(errors, paste0("values: ",
          paste(utils::head(all.equal(ai, bi, tolerance = tolerance,
                                check.attributes = TRUE), 4L), collapse = "; ")))
      }
      # Independent oracle checks: comparing two broken formats alone is not
      # enough, as both could corrupt the same fixture value.
      if (identical(name, "sql_bigint") &&
          !identical(bi[1:2], c("-9223372036854775808", "9223372036854775807")))
        errors <- c(errors, "bigint limits not preserved")
      if (identical(name, "sql_int") &&
          !isTRUE(identical(as.numeric(bi[1]), -2147483648)))
        errors <- c(errors, "INT32_MIN not preserved")
      if (identical(name, "nullable_unicode") &&
          (!identical(bi[1:2], c("NA", "")) || !is.na(bi[3])))
        errors <- c(errors, "empty/NULL/text NA conflated")
      if (identical(name, "unicode") &&
          !identical(bi[1], "\u00e6\u00f8\u00e5 \u6f22\u5b57 \U0001F600 cafe\u0301\r\n\"citat\", tab\t"))
        errors <- c(errors, "Unicode fixture incorrect")
      if (identical(name, "precise") && identical(settings$precision, "full") &&
          !identical(bi[1:2], c("9999999999999999999999999999.1234567890",
                                  "-9999999999999999999999999999.1234567890")))
        errors <- c(errors, "38-digit decimal values not preserved")
      if (identical(name, "near_zero") && identical(settings$precision, "full") &&
          !identical(bi[1], "0.00000000000000000000000000000000000001"))
        errors <- c(errors, "decimal near zero not preserved")
      if (identical(name, "sql_date") &&
          !(inherits(bi, "Date") &&
            identical(as.character(bi[1]), "1900-01-01") &&
            identical(as.character(bi[2]), "2100-12-31")))
        errors <- c(errors, "SQL Date endpoints incorrect")
      if (identical(name, "clock_time") && identical(settings$precision, "native") &&
          !(inherits(bi, "difftime") && is.na(bi[4]) &&
            abs(as.numeric(bi[2])) < 1e-7))
        errors <- c(errors, "SQL time endpoints incorrect")
      if (identical(name, "binary_bytes") &&
          !(is.list(bi) &&
            identical(bi[[1]], as.raw(c(0, 1, 34, 44, 10, 13, 255))) &&
            identical(bi[[2]], raw(0))))
        errors <- c(errors, "binary bytes or empty binary incorrect")
      if (identical(name, "null_binary") &&
          !(is.list(bi) && is.null(bi[[1]]) &&
            identical(bi[[2]], raw(0))))
        errors <- c(errors, "binary NULL and empty binary conflated")
      if (identical(name, "bit_flag") &&
          !(is.logical(bi) && identical(bi[1:2], c(TRUE, FALSE)) && is.na(bi[4])))
        errors <- c(errors, "SQL bit/NULL incorrect")
      if (identical(name, "absolute_clock") &&
          identical(settings$precision, "native")) {
        utc <- function(s) as.numeric(as.POSIXct(s, tz = "UTC"))
        if (!(inherits(bi, "POSIXct") &&
              identical(attr(bi, "tzone"), "UTC") &&
              abs(as.numeric(bi[3]) - utc("1999-12-31 22:00:00")) < 1e-6 &&
              abs(as.numeric(bi[5]) - utc("2000-01-02 00:00:00")) < 1e-6))
          errors <- c(errors, "datetimeoffset legal extreme offsets incorrect")
      }
      if (identical(name, "wall_clock") && identical(settings$precision, "native") &&
          identical(settings$tz, "Europe/Copenhagen")) {
        if (!is.na(bi[1]))
          errors <- c(errors, "Nonexistent DST-gap local timestamp must be NA")
        if (!is.na(bi[2]))
          errors <- c(errors, "Ambiguous DST-fall local timestamp must be NA")
        if (!any(grepl("Row(s): 1", csv$warnings, fixed=TRUE)) ||
            !any(grepl("Row(s): 1", col$warnings, fixed=TRUE)))
          errors <- c(errors, "DST-gap warning lacks result row 1")
        if (!any(grepl("Row(s): 2", csv$warnings, fixed=TRUE)) ||
            !any(grepl("Row(s): 2", col$warnings, fixed=TRUE)))
          errors <- c(errors, "DST-fold warning lacks result row 2")
      }
      if (identical(name, "wall_clock") && identical(settings$precision, "full") &&
          !grepl("2026-03-29T02:30:00", bi[1], fixed = TRUE))
        errors <- c(errors, "Original DST-gap wall time not preserved as text")
      append_row(settings, name, if (length(typ)) typ else "",
                 if (length(errors)) "FAIL" else "PASS",
                 paste(errors, collapse = " | "), csv$warnings, col$warnings)
    }
    if (!identical(names(sql_timings(a)), names(sql_timings(b))))
      append_row(settings, "<timings>", "", "FAIL",
                 "CSV/columnar timing metadata names differ",
                 csv$warnings, col$warnings)
  }
  do.call(rbind, rows)
}
