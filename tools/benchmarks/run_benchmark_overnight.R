# Run with Rscript or source() in an existing R session.
# Requires benchmark_dbi.R in the same directory.
#
# 1) Existing connections (in RStudio):
#      benchmark_connections <- list(RSqlClientNET = con_RSqlClientNET, odbc = con_odbc)
#      source("tools/benchmarks/run_benchmark_overnight.R")
#
# 2) Fresh Rscript session (recommended for overnight runs): set two distinct
#    environment variables in your OS / PowerShell session first:
#      RSQLCLIENTNET_CONNECTION_STRING  Microsoft.Data.SqlClient format
#      ODBC_CONNECTION_STRING        ODBC format incl. Driver={ODBC Driver 18 for SQL Server}
#    Then run Rscript tools/benchmarks/run_benchmark_overnight.R.
#    Connection strings may contain credentials; NEVER paste them into logs.
#
# Settings can be changed here or through BENCH_* environment variables.
# Results are checkpointed after EACH write/read run in details.csv.
# By default, both permanent (normal database) and session-local temporary
# SQL Server tables are benchmarked using identical generated data. All test
# tables are named uniquely and dropped when their scenario finishes.

.bench_source_dir <- local({
  # During source(), one of the stack frames contains an `ofile` field.
  frames <- sys.frames()
  paths <- vapply(frames, function(f) {
    if (is.null(f$ofile)) "" else as.character(f$ofile)[1L]
  }, character(1L))
  paths <- paths[nzchar(paths)]
  own_script <- grep("run_benchmark_overnight[.]R$", paths, value = TRUE)
  if (length(own_script)) paths <- own_script
  if (!length(paths)) {
    args <- commandArgs(trailingOnly = FALSE)
    arg <- grep("^--file=", args, value = TRUE)
    if (length(arg)) paths <- sub("^--file=", "", arg[1L])
  }
  if (!length(paths)) getwd() else
    dirname(normalizePath(tail(paths, 1L), winslash = "/", mustWork = TRUE))
})

benchmark_run_overnight <- function() {
  base_dir <- .bench_source_dir
  source(file.path(base_dir, "benchmark_dbi.R"), local = environment())

  env <- function(name, default) {
    x <- Sys.getenv(name, unset = "")
    if (nzchar(x)) x else default
  }
  num_vec <- function(x) as.numeric(trimws(strsplit(x, ",", fixed = TRUE)[[1L]]))
  char_vec <- function(x) trimws(strsplit(x, ",", fixed = TRUE)[[1L]])
  # BENCH_APPEND_MODES=none disables the additional pre-created-table writes.
  append_vec <- function(x) if (tolower(trimws(x)) %in% c("none", ""))
    character() else char_vec(x)
  get_bool <- function(name, default = FALSE) {
    value <- tolower(env(name, if (default) "true" else "false"))
    if (!value %in% c("true", "false", "1", "0", "yes", "no"))
      stop("Expected boolean in ", name)
    value %in% c("true", "1", "yes")
  }

  mode <- tolower(env("BENCH_MODE", "overnight"))
  if (!mode %in% c("smoke", "overnight")) stop("BENCH_MODE must be smoke or overnight")
  smoke <- identical(mode, "smoke")

  all_types <- c(
    "integer", "numeric", "logical", "character", "date", "datetime",
    "mixed", "bigint", "binary", "decimal", "time", "guid", "unicode",
    "text_long", "datetimeoffset", "real", "smallint", "tinyint",
    "money", "varchar", "xml", "wide_mixed"
  )
  types <- char_vec(env("BENCH_TYPES", if (smoke)
    "integer,date,datetime,character,bigint,binary,decimal,mixed,wide_mixed" else
    paste(all_types, collapse = ",")))

  cfg <- list(
    repetitions_write = as.integer(env("BENCH_REPETITIONS_WRITE",
       env("BENCH_REPETITIONS", if (smoke) "1" else "5"))),
    repetitions_read = as.integer(env("BENCH_REPETITIONS_READ",
       env("BENCH_REPETITIONS", if (smoke) "1" else "5"))),
    n_write = num_vec(env("BENCH_N_WRITE",
       env("BENCH_N", if (smoke) "10000" else "1000000"))),
    n_read = num_vec(env("BENCH_N_READ",
       env("BENCH_N", if (smoke) "10000" else "1000000"))),
    types = types,
    text_lengths = num_vec(env("BENCH_TEXT_LENGTHS", if (smoke) "10,100" else "10,100,1000,10000")),
    null_fractions = num_vec(env("BENCH_NULL_FRACTIONS", if (smoke) "0,0.1" else "0,0.1,0.5")),
    binary_sizes = num_vec(env("BENCH_BINARY_SIZES", if (smoke) "16" else "16,1024,8192")),
    wide_columns = num_vec(env("BENCH_WIDE_COLUMNS", if (smoke) "25" else "25,100")),
    max_case_payload_mb = as.numeric(env("BENCH_MAX_PAYLOAD_MB", if (smoke) "64" else "512")),
    max_wide_rows = as.integer(env("BENCH_MAX_WIDE_ROWS", if (smoke) "10000" else "100000")),
    max_hours = as.numeric(env("BENCH_MAX_HOURS", if (smoke) "1" else "12")),
    seed = as.integer(env("BENCH_SEED", "17")),
    fail_fast = get_bool("BENCH_FAIL_FAST", FALSE),
    append_modes = append_vec(env("BENCH_APPEND_MODES", "append,append_table_lock")),
    table_types = char_vec(env("BENCH_TABLE_TYPES", "temporary,permanent")),
    verbose = TRUE
  )

  # Use already open connections if sourced in an interactive session.
  existing <- get0("benchmark_connections", envir = .GlobalEnv, inherits = FALSE)
  owned_connections <- is.null(existing)
  if (!owned_connections) {
    connections <- existing
    reconnect_callbacks <- get0("benchmark_reconnect", envir = .GlobalEnv,
                                inherits = FALSE)
    cat("Using existing benchmark_connections; they will remain open.\n")
  } else {
    cs_sql <- Sys.getenv("RSQLCLIENTNET_CONNECTION_STRING", unset = "")
    if (!nzchar(cs_sql)) cs_sql <- Sys.getenv("RSQLCLIENTNET_TEST_CONNECTION_STRING", unset = "")
    cs_odbc <- Sys.getenv("ODBC_CONNECTION_STRING", unset = "")
    if (!nzchar(cs_sql) || !nzchar(cs_odbc)) {
      stop("Set RSQLCLIENTNET_CONNECTION_STRING (or RSQLCLIENTNET_TEST_CONNECTION_STRING) ",
           "and ODBC_CONNECTION_STRING, or create global benchmark_connections.")
    }
    for (pkg in c("DBI", "RSqlClientNET", "odbc")) {
      if (!requireNamespace(pkg, quietly = TRUE)) stop("Install package: ", pkg)
    }
    connections <- list()
    created_connections <- list()
    on.exit({
      # Includes fresh sessions opened by recovery callbacks. Never close a
      # connection owned by the user when called from source()/RStudio.
      for (cx in created_connections) {
        if (isTRUE(tryCatch(DBI::dbIsValid(cx), error = function(e) FALSE)))
          try(DBI::dbDisconnect(cx), silent = TRUE)
      }
    }, add = TRUE)
    track <- function(cx) {
      created_connections[[length(created_connections) + 1L]] <<- cx
      cx
    }
    reconnect_callbacks <- list(
      RSqlClientNET = function() track(DBI::dbConnect(
        RSqlClientNET::SqlClientNET(), connection_string = cs_sql)),
      odbc = function() track(DBI::dbConnect(
        odbc::odbc(), .connection_string = cs_odbc))
    )
    connections$RSqlClientNET <- reconnect_callbacks$RSqlClientNET()
    connections$odbc <- reconnect_callbacks$odbc()
  }

  # Fresh output location by default; previous temporary-only checkpoint plans
  # are incompatible with the extra table_type dimension.
  target <- env("BENCH_OUTPUT", file.path(base_dir, "benchmark_results",
       paste0(mode, "_", paste(cfg$table_types, collapse = "_"))))
  cfg$run_dir <- target
  cfg$reconnect <- reconnect_callbacks
  cfg$resume <- get_bool("BENCH_RESUME", TRUE) &&
    file.exists(file.path(target, "plan.rds"))

  dir.create(target, recursive = TRUE, showWarnings = FALSE)
  # Keep a reproducibility record without connection strings or passwords.
  if (!file.exists(file.path(target, "session_info.txt")))
    writeLines(capture.output(utils::sessionInfo()),
               file.path(target, "session_info.txt"))

  cat("Mode: ", mode, "\n", sep = "")
  cat("Output: ", normalizePath(target, winslash = "/"), "\n", sep = "")
  cat("Drivers: ", paste(names(connections), collapse = ", "), "\n", sep = "")
  cat("Requested rows: write=", paste(cfg$n_write, collapse = ","),
      ", read=", paste(cfg$n_read, collapse = ","), "; repetitions: write=",
      cfg$repetitions_write, ", read=", cfg$repetitions_read,
      "; tables=", paste(cfg$table_types, collapse = ","),
      "; additional writes=", if (length(cfg$append_modes))
        paste(cfg$append_modes, collapse = ",") else "none", "\n", sep = "")

  result <- do.call(benchmark_dbi, c(list(con = connections), cfg))
  saveRDS(result, file.path(target, "result.rds"))
  cat("\nResults: summary.csv, details.csv, failures.csv, result.rds\n")
  cat("Completed ", result$completed, "/", result$total, " tasks\n", sep = "")
  invisible(result)
}

benchmark_result <- benchmark_run_overnight()
