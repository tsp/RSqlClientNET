# Third-party licenses and redistribution (RSqlClientNET 0.1.0)

The RSqlClientNET project source is distributed under MIT (`LICENSE.md`);
see `inst/LICENSES/RSqlClientNET-MIT.txt` in the installed package.
A separate MIT license for Microsoft.Data.SqlClient 7.1.1 is included.
NuGet dependencies are **not** relicensed under RSqlClientNET's MIT license.

## Native Windows SNI

Microsoft.Data.SqlClient 7.1.1 brings in `Microsoft.Data.SqlClient.SNI.runtime 7.1.0`
for Windows x86/x64/ARM64. Microsoft's **Distributable Code** license for this
native library is separate from MIT. The *full* license text is shipped in
`inst/LICENSES/Microsoft.Data.SqlClient.SNI.runtime-7.1.0.txt`.
Official license: <https://www.nuget.org/packages/Microsoft.Data.SqlClient.SNI.runtime/7.1.0/License>.

As a project release decision, RSqlClientNET keeps native SNI and includes its
license text and a consolidated third-party notice with the source and binary.
Its terms (including conditions for distributors and end users) remain applicable
to that component; this notice does not change Microsoft's terms.

## Other NuGet dependencies

`inst/LICENSES/THIRD-PARTY-NOTICES.md` lists the 24 NuGet packages found in the
previously tested Windows 0.1.0 binary, updated with the patched PKCS dependency.
The bundled `Microsoft.Data.SqlClient` and other transitive managed packages are
individually MIT licensed according to their respective NuGet metadata, except SNI.
The framework-dependent helper does **not** bundle the entire .NET runtime.
R library dependencies are installed independently and do not ship within the
RSqlClientNET Windows package.

## Security update

The 0.1.0 source explicitly references `System.Security.Cryptography.Pkcs 8.0.2`
(as opposed to 8.0.1 in the previous binary) to address security advisories
CVE-2026-84008 and CVE-2026-70162. This requires a fresh .NET publish and a new
Windows R package; the previous ZIP must not be released as the patched build.

## Final binary verification

`tools/build-binary.R` checks for the included notice files and the resolved
PKCS 8.0.2 dependency in the new Windows ZIP. Verify the final manifest and
package-specific `ThirdPartyNotices` against the **actual** restored NuGet
packages if the dependency graph changes. The project's MIT license does not
replace any third-party copyright or license notices.
