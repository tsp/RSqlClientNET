#!/bin/sh
# Compile the managed helper without modifying the package source tree.
# R_PACKAGE_DIR is the *staged* installation directory supplied by R CMD INSTALL.
# In particular, do not remove or overwrite inst/dotnet/publish: Dropbox and
# antivirus scanners can lock it, and R CMD INSTALL may run repeatedly.
set -eu

if [ "${RSQLCLIENTNET_SKIP_DOTNET_BUILD:-0}" = "1" ]; then
  echo "RSqlClientNET: skipping .NET helper compilation (RSQLCLIENTNET_SKIP_DOTNET_BUILD=1)"
  exit 0
fi

if [ -n "${RSQLCLIENTNET_DOTNET:-}" ]; then
  DOTNET="$RSQLCLIENTNET_DOTNET"
else
  DOTNET="$(command -v dotnet 2>/dev/null || command -v dotnet.exe 2>/dev/null || true)"
fi
if [ -z "$DOTNET" ]; then
  echo "RSqlClientNET: .NET SDK was not found in PATH." >&2
  echo "Install .NET SDK 8+ before installing RSqlClientNET from source." >&2
  echo "To intentionally defer compilation, set RSQLCLIENTNET_SKIP_DOTNET_BUILD=1." >&2
  exit 1
fi

project="inst/dotnet/SqlClientBridge.csproj"
if [ ! -f "$project" ]; then
  echo "RSqlClientNET: missing $project" >&2
  exit 1
fi
if [ -z "${R_PACKAGE_DIR:-}" ]; then
  echo "RSqlClientNET: R_PACKAGE_DIR is missing. This script must run from R CMD INSTALL." >&2
  exit 1
fi

# R_PACKAGE_DIR may be a Windows drive-letter path. Convert only when
# necessary; dotnet itself receives a temporary build directory path.
stage="$R_PACKAGE_DIR"
case "$stage" in
  [A-Za-z]:/*|[A-Za-z]:\\*)
    if command -v cygpath >/dev/null 2>&1; then
      stage="$(cygpath -u "$stage")"
    fi
    ;;
esac

work="$(mktemp -d "${TMPDIR:-/tmp}/RSqlClientNET-dotnet.XXXXXXXX")"
cleanup_work() { rm -rf "$work" 2>/dev/null || :; }
trap cleanup_work EXIT
trap 'exit 1' HUP INT TERM
cp "$project" "$work/"
for source in inst/dotnet/*.cs; do
  [ -f "$source" ] || continue
  cp "$source" "$work/"
done

# Keep SDK/MSBuild scratch files inside this build's private workspace.
# These would otherwise be created in R CMD check's TMPDIR and reported as
# detritus. NuGet scratch must be SHARED across builds using the same package
# cache because it coordinates cross-process locks (do not use $work here).
nuget_scratch="${NUGET_SCRATCH:-${XDG_CACHE_HOME:-${HOME:-/tmp}/.cache}/RSqlClientNET/NuGetScratch}"
mkdir -p "$work/tmp" "$nuget_scratch"
out="$work/publish"
echo "RSqlClientNET: publishing .NET 8 helper in an isolated temporary directory..."
TMPDIR="$work/tmp" TMP="$work/tmp" TEMP="$work/tmp" \
  NUGET_SCRATCH="$nuget_scratch" \
  DOTNET_CLI_TELEMETRY_OPTOUT=1 DOTNET_NOLOGO=1 \
  DOTNET_CLI_DO_NOT_USE_MSBUILD_SERVER=1 MSBUILDDISABLENODEREUSE=1 \
  DOTNET_EnableDiagnostics_IPC=0 \
  "$DOTNET" publish "$work/SqlClientBridge.csproj" \
  -c Release -f net8.0 --self-contained false -o "$out"

for required in SqlClientBridge.dll SqlClientBridge.runtimeconfig.json SqlClientBridge.deps.json; do
  if [ ! -s "$out/$required" ]; then
    echo "RSqlClientNET: .NET publish did not create $required." >&2
    exit 1
  fi
done

# Copy only to this installation's staged directory; never to inst/ in
# the source checkout. The installed binary ZIP will include these files.
target="$stage/dotnet/publish"
mkdir -p "$target"
cp -R "$out/." "$target/"
for required in SqlClientBridge.dll SqlClientBridge.runtimeconfig.json SqlClientBridge.deps.json; do
  if [ ! -s "$target/$required" ]; then
    echo "RSqlClientNET: published helper missing from package stage: $required" >&2
    exit 1
  fi
done
echo "RSqlClientNET: helper installed in the R package staging directory."
