# Build an installable Windows binary ZIP from a CLEAN source bundle.
# Run from the RSqlClientNET package root with Rtools and .NET SDK installed.
# The bundle step applies .Rbuildignore, excluding any old dotnet/publish,
# bin and obj folders in a synced source checkout (e.g., Dropbox).
if (.Platform$OS.type != "windows")
  stop("This script builds Windows binary packages; run under Windows R.")
source("tools/ensure-deps.R", local = TRUE)
rscn_ensure_dev_deps("binary")
if (!requireNamespace("roxygen2", quietly = TRUE))
  stop("Install roxygen2 first: install.packages('roxygen2')")
if (!requireNamespace("pkgbuild", quietly = TRUE))
  stop("Install pkgbuild first: install.packages('pkgbuild')")
if (!requireNamespace("callr", quietly = TRUE))
  stop("Install callr first: install.packages('callr')")

# roxygen2 uses pkgload::load_all() to inspect code. If run in the user's
# R session, it can leave the *source* tree registered as a pseudo-installation.
# This causes ??RSqlClientNET / help.search() to warn about missing Meta/package.rds
# and S3 methods even when the Windows binary is intact. Isolate that operation.
# Prefer the authoritative, unmodified SNI license from the local NuGet cache
# when present. Keep the checked-in full text as a fallback for source builds.
# The previous Windows binary already restored this package to the build host.
nuget_base <- Sys.getenv("NUGET_PACKAGES", unset =
                         file.path(path.expand("~"), ".nuget", "packages"))
sni_upstream <- file.path(nuget_base, "microsoft.data.sqlclient.sni.runtime",
                          "7.1.0", "LICENSE.txt")
sni_target <- file.path("inst", "LICENSES",
                        "Microsoft.Data.SqlClient.SNI.runtime-7.1.0.txt")
if (file.exists(sni_upstream)) {
  upstream_text <- readLines(sni_upstream, warn = FALSE)
  if (!any(grepl("MICROSOFT.DATA.SQLCLIENT.SNI LIBRARY", upstream_text, fixed = TRUE)))
    stop("Unexpected SNI license text in local NuGet cache: ", sni_upstream)
  if (!file.copy(sni_upstream, sni_target, overwrite = TRUE))
    stop("Could not copy upstream SNI license into package source")
  message("Using SNI license from NuGet cache: ", sni_upstream)
} else {
  message("NuGet SNI license not cached; using the bundled license text.")
}

pkg_root <- normalizePath(".", mustWork = TRUE)
callr::r(
  function(path) {
    roxygen2::roxygenise(path)
    invisible(TRUE)
  },
  args = list(pkg_root),
  wd = pkg_root,
  show = TRUE
)

tmp <- tempfile("RSqlClientNET-binary-build-")
dir.create(tmp, recursive = TRUE)
# R's tempfile() directory can be removed after the script finishes or the
# current R session exits. Keep it until the binary is verified.
source_archive <- pkgbuild::build(".", binary = FALSE, dest_path = tmp)
source_dir <- file.path(tmp, "source")
dir.create(source_dir)
utils::untar(source_archive, exdir = source_dir)

package_dir <- file.path(source_dir, "RSqlClientNET")
if (!file.exists(file.path(package_dir, "DESCRIPTION")))
  stop("Cannot locate clean RSqlClientNET source tree after unpacking ", source_archive)
if (dir.exists(file.path(package_dir, "inst", "dotnet", "publish")))
  stop("Source archive unexpectedly contains generated dotnet/publish artifacts.")

archive <- pkgbuild::build(
  package_dir,
  binary = TRUE,
  dest_path = normalizePath("..", mustWork = TRUE),
  args = "--html"
)
paths <- gsub("\\", "/", utils::unzip(archive, list = TRUE)$Name, fixed = TRUE)
required <- c(
  "RSqlClientNET/dotnet/publish/SqlClientBridge.dll",
  "RSqlClientNET/dotnet/publish/SqlClientBridge.runtimeconfig.json",
  "RSqlClientNET/dotnet/publish/SqlClientBridge.deps.json",
  "RSqlClientNET/html/00Index.html",
  "RSqlClientNET/help/AnIndex",
  "RSqlClientNET/Meta/package.rds",
  "RSqlClientNET/Meta/nsInfo.rds",
  "RSqlClientNET/R/RSqlClientNET.rdb",
  "RSqlClientNET/LICENSES/RSqlClientNET-MIT.txt",
  "RSqlClientNET/LICENSES/Microsoft.Data.SqlClient-MIT.txt",
  "RSqlClientNET/LICENSES/Microsoft.Data.SqlClient.SNI.runtime-7.1.0.txt",
  "RSqlClientNET/LICENSES/THIRD-PARTY-NOTICES.md"
)
missing <- required[!required %in% paths]
if (length(missing)) {
  stop("Binary archive is incomplete: missing ", paste(missing, collapse = ", "), ".\n",
       "Inspect the output of R CMD INSTALL --html and the archive contents.",
       call. = FALSE)
}
# Check the actual published .NET dependency graph, not just the csproj.
# These guardrails prevent accidentally releasing the previously audited
# Windows binary with the vulnerable PKCS 8.0.1 asset set.
manifest_path <- "RSqlClientNET/dotnet/publish/SqlClientBridge.deps.json"
utils::unzip(archive, files = manifest_path, exdir = tmp)
manifest <- jsonlite::fromJSON(file.path(tmp, manifest_path), simplifyVector = FALSE)
package_keys <- names(manifest$libraries)
if (!"System.Security.Cryptography.Pkcs/8.0.2" %in% package_keys)
  stop("Windows binary does not contain expected patched System.Security.Cryptography.Pkcs/8.0.2. Found: ",
       paste(grep("^System.Security.Cryptography.Pkcs/", package_keys, value = TRUE), collapse = ", "))
if (!"Microsoft.Data.SqlClient.SNI.runtime/7.1.0" %in% package_keys)
  stop("Windows binary has an unexpected native SNI version; review/update license files")
if (!"Microsoft.Data.SqlClient/7.1.1" %in% package_keys)
  stop("Windows binary has an unexpected SqlClient version; review/update license files")
message("Verified published NuGet graph: SqlClient 7.1.1, SNI 7.1.0, PKCS 8.0.2.")
# A full smoke test must use a fresh R process with the *binary* installed
# into a private library; loading the current source tree would mask issues.
callr::r(
  function(binary_zip) {
    lib <- tempfile("RSqlClientNET-binary-check-lib-")
    dir.create(lib, recursive = TRUE)
    on.exit(unlink(lib, recursive = TRUE, force = TRUE), add = TRUE)
    utils::install.packages(binary_zip, repos = NULL, type = "win.binary",
                            lib = lib, quiet = TRUE)
    installed <- file.path(lib, "RSqlClientNET")
    for (license in c("RSqlClientNET-MIT.txt", "Microsoft.Data.SqlClient-MIT.txt",
                      "Microsoft.Data.SqlClient.SNI.runtime-7.1.0.txt", "THIRD-PARTY-NOTICES.md")) {
      if (!file.exists(file.path(installed, "LICENSES", license)))
        stop("Installed package missing required license file: ", license)
    }
    if (!file.exists(file.path(installed, "Meta", "package.rds")))
      stop("Installed binary is missing Meta/package.rds")
    ns <- loadNamespace("RSqlClientNET", lib.loc = lib)
    methods <- c("print.sqlclient_connection",
                 "sql_dialect.SqlClientNETConnection",
                 "dbplyr_edition.SqlClientNETConnection")
    missing_methods <- methods[!vapply(methods, exists, logical(1),
                                       envir = ns, inherits = FALSE)]
    if (length(missing_methods))
      stop("Missing S3 methods in installed namespace: ",
           paste(missing_methods, collapse = ", "))
    # Check every documented DBI help topic as well as common method aliases.
    for (topic in c("RSqlClientNET-db-connect", "dbConnect",
                    "RSqlClientNET-db-query", "dbGetQuery",
                    "RSqlClientNET-db-result", "dbFetch",
                    "RSqlClientNET-db-write", "dbWriteTable",
                    "RSqlClientNET-db-read", "dbReadTable",
                    "RSqlClientNET-db-transaction", "dbBegin")) {
      help_match <- utils::help(topic, package = "RSqlClientNET", lib.loc = lib)
      if (length(help_match) != 1L)
        stop("Help topic is missing in installed binary: ", topic)
    }
    # Surface namespace/metadata warnings as failures during release builds.
    withCallingHandlers(
      utils::help.search("RSqlClientNET", package = "RSqlClientNET", lib.loc = lib),
      warning = function(w) stop("help.search() warning: ", conditionMessage(w))
    )
    invisible(TRUE)
  },
  args = list(normalizePath(archive, mustWork = TRUE)),
  show = TRUE
)
message("Verified: .NET helper, R help index, package metadata and S3 methods.")
message("Windows binary: ", normalizePath(archive, mustWork = TRUE))
message("Install this ZIP in a NEW R session, then run ??RSqlClientNET.")
