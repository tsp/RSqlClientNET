#!/bin/sh
# Local macOS verification against the RC source tree.
# Run from the RSqlClientNET package root: sh tools/test-macos.sh
# Optional: RSQLCLIENTNET_TEST_CONNECTION_STRING (disposable test database)
# Optional: RSQLCLIENTNET_MAC_CHECK=0 (skip CRAN-style package check)
# Optional: RSQLCLIENTNET_MAC_INSTALL_DEPS=0 (use preinstalled dependencies)
set -eu

if [ "$(uname -s)" != "Darwin" ]; then
  echo "This script is intended for macOS (Darwin)." >&2
  exit 1
fi
if [ ! -f DESCRIPTION ] || ! grep -q '^Package: RSqlClientNET$' DESCRIPTION; then
  echo "Run from the RSqlClientNET source directory." >&2
  exit 1
fi
for exe in R Rscript dotnet xcrun; do
  if ! command -v "$exe" >/dev/null 2>&1; then
    echo "Missing macOS prerequisite: $exe" >&2
    exit 1
  fi
done
if ! xcrun --find clang++ >/dev/null 2>&1; then
  echo "Apple Command Line Tools are missing. Run: xcode-select --install" >&2
  exit 1
fi

# R CMD INSTALL invokes configure and cleanup directly on POSIX systems.
# Some ZIP extraction tools discard executable bits. Repair only these
# package-owned installation hooks before invoking R CMD INSTALL/devtools.
if [ ! -x configure ] || [ ! -x cleanup ]; then
  echo "Restoring executable permissions on configure and cleanup..."
  chmod +x configure cleanup
fi

# macOS's framework installation may not have a writable default R library.
# Keep local test installations isolated and discoverable by all R subprocesses.
: "${R_LIBS_USER:=$HOME/R/RSqlClientNET-library}"
export R_LIBS_USER
mkdir -p "$R_LIBS_USER"

# Avoid .NET diagnostic pipes in R CMD check's monitored temporary directory.
: "${DOTNET_EnableDiagnostics_IPC:=0}"
export DOTNET_EnableDiagnostics_IPC

echo "RSqlClientNET macOS preflight"
echo "  macOS: $(sw_vers -productVersion) ($(uname -m))"
echo "  R: $(Rscript -e 'cat(R.version.string, " / ", R.version$arch, sep="")')"
echo "  .NET SDK: $(dotnet --version)"
dotnet --list-runtimes

# The project's runtimeconfig.json sets RollForward=LatestMajor, so any
# installed Microsoft.NETCore.App major >= 8 is an acceptable runtime.
if ! dotnet --list-runtimes | awk '$1 == "Microsoft.NETCore.App" {split($2, v, "."); if (v[1] + 0 >= 8) ok=1} END {exit !ok}'; then
  echo "RSqlClientNET requires a Microsoft.NETCore.App runtime version 8 or newer." >&2
  exit 1
fi

if [ "${RSQLCLIENTNET_MAC_INSTALL_DEPS:-1}" != "0" ]; then
  echo "[1/4] Installing missing R development/test dependencies"
  Rscript -e 'source("tools/ensure-deps.R"); rscn_ensure_dev_deps("release")'
else
  echo "[1/4] Using existing R dependencies"
fi

echo "[2/4] Building/installing source package with native macOS Clang and .NET"
R CMD INSTALL -l "$R_LIBS_USER" .

echo "[3/4] Running installed helper and validating DBI driver"
Rscript -e '
  library(RSqlClientNET)
  p <- sql_ping()
  print(p)
  stopifnot(isTRUE(p$ok), identical(p$reply, "pong"))
  stopifnot(methods::is(SqlClientNET(), "SqlClientNETDriver"))
'

if [ -n "${RSQLCLIENTNET_TEST_CONNECTION_STRING:-}" ]; then
  echo "Running SQL Server regression and official DBItest suites"
  Rscript -e 'source("tools/run-release-tests.R")'
else
  echo "No RSQLCLIENTNET_TEST_CONNECTION_STRING set: SQL integration/DBItest skipped."
fi

if [ "${RSQLCLIENTNET_MAC_CHECK:-1}" = "0" ]; then
  echo "[4/4] R CMD check skipped (RSQLCLIENTNET_MAC_CHECK=0)"
else
  echo "[4/4] Running CRAN-style check"
  echo "Checking native POSIX spawn, Rcpp library and Rd documentation on macOS."
  Rscript -e 'devtools::check(args = "--as-cran")'
fi
