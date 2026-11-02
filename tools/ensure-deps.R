# Bootstrap R dependencies for development scripts only.
# Never sourced by the installed package or during R CMD INSTALL/check.
# To require a pre-provisioned library (e.g. in CI), set:
#   RSQLCLIENTNET_AUTO_INSTALL_DEPS=0
rscn_ensure_dev_deps <- function(mode = c("document", "binary", "release"),
                                 root = ".") {
  mode <- match.arg(mode)
  root <- normalizePath(root, mustWork = TRUE)
  if (!file.exists(file.path(root, "DESCRIPTION"))) {
    stop("Run this script from the RSqlClientNET source directory.",
         call. = FALSE)
  }

  if (tolower(Sys.getenv("RSQLCLIENTNET_AUTO_INSTALL_DEPS", "1")) %in%
      c("0", "false", "no", "off")) {
    message("RSqlClientNET: automatic R dependency installation disabled.")
    return(invisible(FALSE))
  }

  # Do not replace a CRAN mirror the user has already selected.
  repos <- getOption("repos")
  if (is.null(repos) || length(repos) == 0L ||
      any(is.na(repos) | repos == "@CRAN@" | !nzchar(repos))) {
    old_repos <- repos
    on.exit(options(repos = old_repos), add = TRUE)
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  }

  if (!requireNamespace("pak", quietly = TRUE)) {
    message("RSqlClientNET: installing 'pak' for development dependency management...")
    utils::install.packages("pak", repos = getOption("repos"))
  }
  if (!requireNamespace("pak", quietly = TRUE)) {
    stop("Could not install 'pak'; check CRAN access and library permissions.",
         call. = FALSE)
  }

  # pak resolves both transitive dependencies and version constraints in
  # DESCRIPTION, without upgrading unrelated installed packages.
  if (mode == "release") {
    message("RSqlClientNET: resolving development/test dependencies...")
    pak::local_install_dev_deps(root = root, upgrade = FALSE, ask = FALSE)
  } else {
    message("RSqlClientNET: resolving required package dependencies...")
    pak::local_install_deps(root = root, upgrade = FALSE, ask = FALSE)
  }

  # These tools are used by scripts, not by the installed R package itself.
  extra <- switch(mode,
    document = c("roxygen2", "callr"),
    binary = c("roxygen2", "callr", "pkgbuild"),
    release = c("devtools", "DBItest")
  )
  missing <- extra[!vapply(extra, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    message("RSqlClientNET: installing build/test tools: ",
            paste(missing, collapse = ", "))
    pak::pkg_install(missing, upgrade = FALSE, ask = FALSE)
  }
  missing <- extra[!vapply(extra, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing development tools after installation: ",
         paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}
