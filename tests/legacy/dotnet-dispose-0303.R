# Offline guard against CS1061 in Microsoft.Data.SqlClient's explicitly implemented IDisposable.
source_path <- file.path("inst", "dotnet", "Program.cs")
if (!file.exists(source_path)) {
  source_path <- system.file("dotnet", "Program.cs", package = "RSqlClientNET")
}
stopifnot(nzchar(source_path), file.exists(source_path))
src <- paste(readLines(source_path, warn = FALSE), collapse = "\n")
stopifnot(grepl("((IDisposable)Copier).Dispose();", src, fixed = TRUE),
          !grepl("Copier.Dispose();", src, fixed = TRUE))
message("RSqlClientNET 0.0.30.3 SqlBulkCopy explicit IDisposable source check passed")
