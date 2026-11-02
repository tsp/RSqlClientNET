# Source provenance review — 2026-10-25

**Maintainer:** Tonny Studsgaard Petersen (`tsp@person.dk`).

**Development disclosure:** This project was vibe-coded with **GPT-6 SOL – high** (AI-assisted development). The human maintainer directed the work and tested it using R, .NET and a SQL Server test database on Windows, Linux and macOS.

## What was checked

The source archive was reviewed for embedded third-party copyright headers, license notices, vendored source directories, linked external projects and files resembling copies from established packages. This included `R/`, `src/`, `inst/dotnet/`, `tools/`, `tests/` and the package build metadata. Several distinctive project-specific test identifiers and implementation phrases were searched in public GitHub code search. The searches did not identify a direct copied source file.

The code uses standard platform APIs and the published API contracts of DBI, Rcpp, dbplyr and Microsoft.Data.SqlClient. This is **not** evidence that code from those projects was copied into the package. `dbplyr` SQL Server translation is called as a dependency, not copied into this project.

## Limits of this review

No source-creation audit trail, complete history back to prototype `0.0.1`, exhaustive code-similarity scan, or formal intellectual-property clearance was available. AI-generated material can resemble pre-existing code, and this static inspection **does not prove independent creation** of every fragment. Please report suspected copied code with a link to its source so licensing and attribution can be reviewed.

The dependency license review is separate; see [THIRD_PARTY.md](THIRD_PARTY.md). Before distributing binaries, review actual dependency versions, licenses, and any bundled redistributable notices. The project MIT license covers the project's own source only; it does not relicense Microsoft or R dependencies.
