# Third-party notices for RSqlClientNET 0.1.0

RSqlClientNET source is MIT licensed (see `RSqlClientNET-MIT.txt`). Its bundled
.NET helper depends on packages distributed under separate licenses.
The native Microsoft SQL Server networking library, `Microsoft.Data.SqlClient.SNI.runtime`,
**is not covered by the RSqlClientNET or SqlClient MIT licenses**.

The following inventory is based on the 24 dependencies in the *previously audited*
Windows 0.1.0 package, with `System.Security.Cryptography.Pkcs` now pinned to 8.0.2.
The final build must be checked against its own `SqlClientBridge.deps.json`, as
NuGet can resolve versions differently. Every package retains its own license.

| NuGet component | Version in expected release | License |
|---|---:|---|
| [Microsoft.Bcl.Cryptography](https://www.nuget.org/packages/Microsoft.Bcl.Cryptography/8.0.0) | 8.0.0 | MIT |
| [Microsoft.Data.SqlClient](https://www.nuget.org/packages/Microsoft.Data.SqlClient/7.1.1) | 7.1.1 | MIT |
| [Microsoft.Data.SqlClient.Extensions.Abstractions](https://www.nuget.org/packages/Microsoft.Data.SqlClient.Extensions.Abstractions/7.1.1) | 7.1.1 | MIT |
| [Microsoft.Data.SqlClient.Internal.Logging](https://www.nuget.org/packages/Microsoft.Data.SqlClient.Internal.Logging/7.1.1) | 7.1.1 | MIT |
| [Microsoft.Data.SqlClient.SNI.runtime](https://www.nuget.org/packages/Microsoft.Data.SqlClient.SNI.runtime/7.1.0) | 7.1.0 | Microsoft Software License Terms (Distributable Code; NOT MIT) |
| [Microsoft.Extensions.Caching.Abstractions](https://www.nuget.org/packages/Microsoft.Extensions.Caching.Abstractions/8.0.0) | 8.0.0 | MIT |
| [Microsoft.Extensions.Caching.Memory](https://www.nuget.org/packages/Microsoft.Extensions.Caching.Memory/8.0.1) | 8.0.1 | MIT |
| [Microsoft.Extensions.DependencyInjection.Abstractions](https://www.nuget.org/packages/Microsoft.Extensions.DependencyInjection.Abstractions/8.0.2) | 8.0.2 | MIT |
| [Microsoft.Extensions.Logging.Abstractions](https://www.nuget.org/packages/Microsoft.Extensions.Logging.Abstractions/8.0.2) | 8.0.2 | MIT |
| [Microsoft.Extensions.Options](https://www.nuget.org/packages/Microsoft.Extensions.Options/8.0.2) | 8.0.2 | MIT |
| [Microsoft.Extensions.Primitives](https://www.nuget.org/packages/Microsoft.Extensions.Primitives/8.0.0) | 8.0.0 | MIT |
| [Microsoft.IdentityModel.Abstractions](https://www.nuget.org/packages/Microsoft.IdentityModel.Abstractions/8.16.0) | 8.16.0 | MIT |
| [Microsoft.IdentityModel.JsonWebTokens](https://www.nuget.org/packages/Microsoft.IdentityModel.JsonWebTokens/8.16.0) | 8.16.0 | MIT |
| [Microsoft.IdentityModel.Logging](https://www.nuget.org/packages/Microsoft.IdentityModel.Logging/8.16.0) | 8.16.0 | MIT |
| [Microsoft.IdentityModel.Protocols](https://www.nuget.org/packages/Microsoft.IdentityModel.Protocols/8.16.0) | 8.16.0 | MIT |
| [Microsoft.IdentityModel.Protocols.OpenIdConnect](https://www.nuget.org/packages/Microsoft.IdentityModel.Protocols.OpenIdConnect/8.16.0) | 8.16.0 | MIT |
| [Microsoft.IdentityModel.Tokens](https://www.nuget.org/packages/Microsoft.IdentityModel.Tokens/8.16.0) | 8.16.0 | MIT |
| [Microsoft.SqlServer.Server](https://www.nuget.org/packages/Microsoft.SqlServer.Server/1.0.0) | 1.0.0 | MIT |
| [System.Configuration.ConfigurationManager](https://www.nuget.org/packages/System.Configuration.ConfigurationManager/8.0.1) | 8.0.1 | MIT |
| [System.Diagnostics.EventLog](https://www.nuget.org/packages/System.Diagnostics.EventLog/8.0.1) | 8.0.1 | MIT |
| [System.IdentityModel.Tokens.Jwt](https://www.nuget.org/packages/System.IdentityModel.Tokens.Jwt/8.16.0) | 8.16.0 | MIT |
| [System.Security.Cryptography.Pkcs](https://www.nuget.org/packages/System.Security.Cryptography.Pkcs/8.0.2) | 8.0.2 | MIT |
| [System.Security.Cryptography.ProtectedData](https://www.nuget.org/packages/System.Security.Cryptography.ProtectedData/8.0.0) | 8.0.0 | MIT |
| [System.Threading.RateLimiting](https://www.nuget.org/packages/System.Threading.RateLimiting/8.0.0) | 8.0.0 | MIT |

## License texts and copyrights

- `RSqlClientNET-MIT.txt`: full license for this package.
- `Microsoft.Data.SqlClient-MIT.txt`: upstream SqlClient MIT license and copyright notice.
- `Microsoft.Data.SqlClient.SNI.runtime-7.1.0.txt`: full, separate Microsoft Software
  License Terms for the native Windows SNI networking DLLs (x86, x64 and ARM64).
- The other NuGet libraries identified above are under MIT, and their copyright
  notices/license texts remain those of their respective NuGet packages.
  The full common MIT terms are supplied in the above MIT license file;
  consult the linked package versions for their individual copyright notices
  and any additional `ThirdPartyNotices`.

The SNI DLLs are redistributed only as part of RSqlClientNET; Microsoft owns
the SNI code and its license governs that component separately.
Reference: <https://www.nuget.org/packages/Microsoft.Data.SqlClient.SNI.runtime/7.1.0/License>.

## Release verification

After rebuilding the Windows binary, verify that the embedded dependency manifest
contains `System.Security.Cryptography.Pkcs/8.0.2` instead of 8.0.1.
Inspect the full resolved NuGet graph and retain any package-specific upstream
copyright and third-party notices applicable to the shipped assets.
