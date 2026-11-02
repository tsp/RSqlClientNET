using System;
using System.Buffers;
using System.Buffers.Binary;
using System.Data;
using System.Data.SqlTypes;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Text;
using System.Text.Json;
using Microsoft.Data.SqlClient;

// Control requests: newline-delimited JSON over stdin. Interactive stdout
// always uses tagged frames: 1 byte ('J' JSON or 'D' raw data), 4-byte
// little-endian signed length, then payload. SQL results never touch disk.
internal static class Program
{
    private static SqlConnection? connection;
    private static SqlTransaction? transaction;
    private static CursorState? cursor;
    private static long nextCursorId;
    // Reset for each command. After a successful stream-start message has
    // been emitted, a subsequent SQL error is *not* a complete terminal
    // reply: the R side must discard the helper rather than reuse the pipe.
    private static bool CurrentResponseStarted;
    private static readonly Stream RawStdout = Console.OpenStandardOutput();
    // Read control-line JSON and raw upload bodies from the SAME buffered byte
    // stream. Mixing Console.In.ReadLine() with raw reads would lose prefetched
    // bytes from TextReader's private buffer on Windows/POSIX.
    private static readonly BufferedStream RawStdin = new(Console.OpenStandardInput(), 65536);
    private static string? ReadControlLine()
    {
        using var line = new MemoryStream();
        while (true)
        {
            int ch = RawStdin.ReadByte();
            if (ch < 0) return line.Length == 0 ? null : throw new EndOfStreamException("Truncated control request.");
            if (ch == 10) return new UTF8Encoding(false, true).GetString(line.ToArray());
            if (line.Length >= 1024L * 1024L * 128L)
                throw new InvalidDataException("Oversized JSON command.");
            line.WriteByte((byte)ch);
        }
    }


    private static void WriteFrame(byte kind, ReadOnlySpan<byte> payload)
    {
        Span<byte> header = stackalloc byte[5];
        header[0] = kind;
        BinaryPrimitives.WriteInt32LittleEndian(header.Slice(1), payload.Length);
        RawStdout.Write(header);
        // Keep individual OS writes bounded, even for a large logical D frame.
        // The reader uses the frame length and read_exact() so fragmented pipe
        // writes cannot change the SCB2 protocol or truncate a valid payload.
        const int MaxPipeWrite = 256 * 1024;
        for (int offset = 0; offset < payload.Length; )
        {
            int count = Math.Min(MaxPipeWrite, payload.Length - offset);
            RawStdout.Write(payload.Slice(offset, count));
            offset += count;
        }
        RawStdout.Flush();
    }

    private static readonly JsonSerializerOptions JsonOpts = new() { PropertyNamingPolicy = JsonNamingPolicy.CamelCase };

    private static void Reply(object value)
    {
        WriteFrame((byte)'J', JsonSerializer.SerializeToUtf8Bytes(value, JsonOpts));
    }

    private static void Ping(string echo)
    {
        // Handshake only: does not create or open any SQL Server connection.
        Reply(new {
            ok = true,
            reply = "pong",
            echo,
            helperVersion = "0.0.30.4",
            transport = "binary",
            processId = Environment.ProcessId,
            runtimeVersion = System.Runtime.InteropServices.RuntimeInformation.FrameworkDescription,
            connectionState = connection?.State.ToString() ?? "Closed"
        });
    }

    private static string GetString(JsonElement root, string name, string fallback = "") =>
        root.TryGetProperty(name, out var x) && x.ValueKind != JsonValueKind.Null ? x.GetString() ?? fallback : fallback;

    private static bool GetBool(JsonElement root, string name, bool fallback) =>
        root.TryGetProperty(name, out var x) && x.ValueKind != JsonValueKind.Null ? x.GetBoolean() : fallback;

    // jsonlite may encode a numeric scalar as either a JSON number or a
    // decimal-text scalar (e.g. through a user-supplied scalar wrapper).
    // Accept both without rounding or coercing fractional values to integers.
    private static int ParseJsonInt32(JsonElement x)
    {
        if (x.ValueKind == JsonValueKind.Number && x.TryGetInt32(out int number))
            return number;
        if (x.ValueKind == JsonValueKind.String &&
            int.TryParse(x.GetString(), NumberStyles.AllowLeadingSign,
                         CultureInfo.InvariantCulture, out int textNumber))
            return textNumber;
        throw new FormatException("Expected a 32-bit integer encoded as a JSON number or integer string; got " + x.ValueKind + ".");
    }
    private static int GetInt(JsonElement root, string name, int fallback) =>
        root.TryGetProperty(name, out var x) && x.ValueKind != JsonValueKind.Null ? ParseJsonInt32(x) : fallback;

    private static void RequireOpen()
    {
        if (connection == null || connection.State != ConnectionState.Open)
            throw new InvalidOperationException("SQL Server connection is not open.");
    }

    private static void Connect(JsonElement root)
    {
        if (cursor != null) throw new InvalidOperationException("Clear active DBI result first.");
        BulkAbortSilently();
        transaction?.Dispose();
        transaction = null;
        connection?.Dispose();
        connection = null;
        var explicitString = GetString(root, "connectionString");
        string connString;

        if (!string.IsNullOrEmpty(explicitString))
        {
            connString = explicitString;
        }
        else
        {
            var builder = new SqlConnectionStringBuilder
            {
                DataSource = GetString(root, "server"),
                InitialCatalog = GetString(root, "database"),
                IntegratedSecurity = GetBool(root, "integratedSecurity", true),
                TrustServerCertificate = GetBool(root, "trustServerCertificate", false),
                ApplicationName = "RSqlClientNET"
            };
            builder["Encrypt"] = GetBool(root, "encrypt", true) ? "True" : "False";
            if (!builder.IntegratedSecurity)
            {
                builder.UserID = GetString(root, "username");
                builder.Password = GetString(root, "password");
            }
            connString = builder.ConnectionString;
        }

        connection = new SqlConnection(connString);
        connection.Open();
        Reply(new { ok = true, state = "open", serverVersion = connection.ServerVersion });
    }

    private static object? ParameterValue(JsonElement value) => value.ValueKind switch
    {
        JsonValueKind.Null => DBNull.Value,
        JsonValueKind.True => true,
        JsonValueKind.False => false,
        JsonValueKind.String => value.GetString(),
        JsonValueKind.Number => value.TryGetInt32(out var i) ? i :
            value.TryGetInt64(out var l) ? l : value.GetDouble(),
        _ => throw new ArgumentException("Only scalar string/number/bool/null SQL parameters are supported.")
    };

    // Explicitly typed parameters avoid AddWithValue's inference for decimal,
    // bigint and temporal values. SQL parameters remain bound, never concatenated.
    private static SqlParameter TypedParameter(string name, JsonElement entry)
    {
        string typeName = GetString(entry, "$sqlclient_type");
        if (typeName.Equals("numeric", StringComparison.OrdinalIgnoreCase)) typeName = "Decimal";
        if (!Enum.TryParse<SqlDbType>(typeName, true, out var sqlType))
            throw new ArgumentException("Unsupported SQL parameter type: " + typeName);
        var p = new SqlParameter(name, sqlType);
        if (entry.TryGetProperty("precision", out var precision) &&
            precision.ValueKind != JsonValueKind.Null)
            p.Precision = checked((byte)precision.GetInt32());
        if (entry.TryGetProperty("scale", out var scale) &&
            scale.ValueKind != JsonValueKind.Null)
            p.Scale = checked((byte)scale.GetInt32());
        if (!entry.TryGetProperty("value", out var val) || val.ValueKind == JsonValueKind.Null)
        {
            p.Value = DBNull.Value;
            return p;
        }
        string v = val.ValueKind == JsonValueKind.String ? val.GetString()! : val.ToString();
        p.Value = sqlType switch
        {
            SqlDbType.BigInt => long.Parse(v, CultureInfo.InvariantCulture),
            SqlDbType.Int => int.Parse(v, CultureInfo.InvariantCulture),
            SqlDbType.SmallInt => short.Parse(v, CultureInfo.InvariantCulture),
            SqlDbType.TinyInt => byte.Parse(v, CultureInfo.InvariantCulture),
            SqlDbType.Decimal => SqlDecimal.Parse(v),
            SqlDbType.Money or SqlDbType.SmallMoney => decimal.Parse(v, CultureInfo.InvariantCulture),
            SqlDbType.Bit => bool.Parse(v),
            SqlDbType.Float => double.Parse(v, CultureInfo.InvariantCulture),
            SqlDbType.Real => float.Parse(v, CultureInfo.InvariantCulture),
            SqlDbType.Date or SqlDbType.DateTime or SqlDbType.DateTime2 or SqlDbType.SmallDateTime =>
                DateTime.Parse(v, CultureInfo.InvariantCulture, DateTimeStyles.None),
            SqlDbType.DateTimeOffset => DateTimeOffset.Parse(v, CultureInfo.InvariantCulture),
            SqlDbType.Time => TimeSpan.Parse(v, CultureInfo.InvariantCulture),
            SqlDbType.UniqueIdentifier => Guid.Parse(v),
            SqlDbType.Binary or SqlDbType.VarBinary => Convert.FromBase64String(v),
            SqlDbType.Char or SqlDbType.NChar or SqlDbType.VarChar or SqlDbType.NVarChar or SqlDbType.Xml => v,
            _ => throw new ArgumentException("SQL parameter type is not supported: " + sqlType)
        };
        return p;
    }

    private static SqlCommand CreateCommand(JsonElement root)
    {
        RequireOpen();
        var cmd = connection!.CreateCommand();
        if (transaction != null) cmd.Transaction = transaction;
        cmd.CommandText = GetString(root, "sql");
        cmd.CommandTimeout = GetInt(root, "timeout", 600);

        if (root.TryGetProperty("params", out var parameters) && parameters.ValueKind == JsonValueKind.Object)
        {
            foreach (var entry in parameters.EnumerateObject())
            {
                var name = entry.Name.StartsWith('@') ? entry.Name : "@" + entry.Name;
                if (entry.Value.ValueKind == JsonValueKind.Object &&
                    entry.Value.TryGetProperty("$sqlclient_type", out _))
                    cmd.Parameters.Add(TypedParameter(name, entry.Value));
                else
                    cmd.Parameters.AddWithValue(name, ParameterValue(entry.Value) ?? DBNull.Value);
            }
        }
        return cmd;
    }

    // This protocol distinguishes SQL NULL from *every* possible value,
    // including an empty string or literal text "NA":
    // N = SQL NULL, quoted V<value> = non-NULL.
    // No SQL result files are created.
    private static string PlainValue(object v, string sqlType = "")
    {
        string t = sqlType.ToLowerInvariant();
        return t switch
        {
            "date" when v is DateTime d => d.ToString("yyyy-MM-dd", CultureInfo.InvariantCulture),
            "datetime" or "datetime2" or "smalldatetime" when v is DateTime d =>
                d.ToString("yyyy-MM-ddTHH:mm:ss.fffffff", CultureInfo.InvariantCulture),
            "datetimeoffset" when v is DateTimeOffset dto => dto.ToString("O", CultureInfo.InvariantCulture),
            "time" when v is TimeSpan span => span.ToString("c", CultureInfo.InvariantCulture),
            "real" when v is float f => f.ToString("R", CultureInfo.InvariantCulture),
            "float" when v is double f => f.ToString("R", CultureInfo.InvariantCulture),
            _ => v switch
            {
                byte[] bytes => Convert.ToBase64String(bytes),
                DateTime d => d.ToString("O", CultureInfo.InvariantCulture),
                DateTimeOffset dto => dto.ToString("O", CultureInfo.InvariantCulture),
                TimeSpan span => span.ToString("c", CultureInfo.InvariantCulture),
                bool b => b ? "TRUE" : "FALSE",
                SqlDecimal sqlDecimal => sqlDecimal.ToString(),
                IFormattable formattable => formattable.ToString(null, CultureInfo.InvariantCulture) ?? "",
                _ => Convert.ToString(v, CultureInfo.InvariantCulture) ?? ""
            }
        };
    }

    private static string CsvValue(object? v, string sqlType = "")
    {
        if (v == null || v is DBNull) return "N";
        return "\"V" + PlainValue(v, sqlType).Replace("\"", "\"\"") + "\"";
    }

    private static string CsvHeader(string columnName) =>
        "\"" + columnName.Replace("\"", "\"\"") + "\"";

    // Receives UTF-8 CSV bytes and writes bounded raw D frames to stdout.
    // The receiver concatenates the *decoded bytes* before converting to UTF-8,
    // because a multi-byte character may cross a chunk boundary.
    private sealed class ChunkedOutputStream : Stream
    {
        private readonly byte[] chunk;
        private int used;
        public int ChunksSent { get; private set; }
        public long BytesSent { get; private set; }
        public ChunkedOutputStream(int chunkSizeBytes)
        {
            chunk = new byte[chunkSizeBytes];
        }

        public override bool CanRead => false;
        public override bool CanSeek => false;
        public override bool CanWrite => true;
        public override long Length => throw new NotSupportedException();
        public override long Position
        {
            get => throw new NotSupportedException();
            set => throw new NotSupportedException();
        }
        public override void SetLength(long value) => throw new NotSupportedException();
        public override long Seek(long offset, SeekOrigin origin) => throw new NotSupportedException();
        public override int Read(byte[] buffer, int offset, int count) => throw new NotSupportedException();

        public override void Write(byte[] buffer, int offset, int count) => Write(buffer.AsSpan(offset, count));
        public override void Write(ReadOnlySpan<byte> bytes)
        {
            while (!bytes.IsEmpty)
            {
                int n = Math.Min(bytes.Length, chunk.Length - used);
                bytes.Slice(0, n).CopyTo(chunk.AsSpan(used));
                used += n;
                bytes = bytes.Slice(n);
                if (used == chunk.Length) FlushChunk();
            }
        }

        private void FlushChunk()
        {
            if (used == 0) return;
            WriteFrame((byte)'D', chunk.AsSpan(0, used));
            ChunksSent++;
            BytesSent += used;
            used = 0;
        }

        public override void Flush() => FlushChunk();
        protected override void Dispose(bool disposing)
        {
            if (disposing) FlushChunk();
            base.Dispose(disposing);
        }
    }

    private static void WriteCsvHeaderAndRows(TextWriter writer, string[] columns, string[] types,
                                              SqlDataReader? reader, out long rows)
    {
        writer.WriteLine(string.Join(",", Array.ConvertAll(columns, c => CsvHeader(c))));
        rows = 0;
        if (reader == null) return;
        while (reader.Read())
        {
            for (int j = 0; j < reader.FieldCount; j++)
            {
                if (j != 0) writer.Write(',');
                if (reader.IsDBNull(j)) writer.Write("N");
                else {
                    // SqlDecimal supports SQL decimal(38,s); CLR decimal is limited to 28-29 digits.
                    object v = (types[j].Equals("decimal", StringComparison.OrdinalIgnoreCase) ||
                                types[j].Equals("numeric", StringComparison.OrdinalIgnoreCase))
                        ? reader.GetSqlDecimal(j) : reader.GetValue(j);
                    writer.Write(CsvValue(v, types[j]));
                }
            }
            writer.WriteLine();
            rows++;
        }
    }

    // Columnar-v2: 4-byte SCB2 marker; UInt32 row count and column count;
    // then per column: one wire-kind byte, ceil(rows/8) validity bytes
    // (bit 1=present), and row-ordered typed payload.  I=int32, L=int64,
    // D=double, B=boolean byte, S=UTF-8 length-prefixed, Y=raw length-prefixed.
    // Strings and exact SQL decimals/timestamps remain textual VALUES, not CSV.
    // No tempfile and no Base64.  Frames retain the previous outer J/D framing.
    private static bool ColumnarRequested(JsonElement root) =>
        GetString(root, "resultFormat", "csv") == "columnar";

    private static int ColumnarBatchRows(JsonElement root)
    {
        int n = GetInt(root, "batchRows", 8192);
        if (n < 1 || n > 65536)
            throw new ArgumentOutOfRangeException(nameof(n), "Expected batchRows 1..65536.");
        return n;
    }

    // Results are negotiated independently for each query. Full precision
    // SQL decimals and timestamps remain lossless text. For native mode we
    // send time ticks, UTC ticks (datetimeoffset), or native doubles instead.
    // Non-UTC naive datetimes travel as binary wall-clock ticks ('W'), with
    // timezone/DST resolution performed in vectorized R batches. This avoids
    // per-row UTF-8 formatting and preserves the existing DST policy.
    private static byte WireKind(string sqlType, bool native, bool utcWallClock,
                                 bool nativeTimestamp)
    {
        return sqlType.ToLowerInvariant() switch
        {
            "bit" => (byte)'B',
            "tinyint" or "smallint" or "int" => (byte)'I',
            "bigint" => (byte)'L',
            "real" or "float" => (byte)'D',
            "decimal" or "numeric" or "money" or "smallmoney" when native => (byte)'N',
            "date" => (byte)'A',
            "time" when native => (byte)'H',
            "datetime" or "datetime2" or "smalldatetime" when nativeTimestamp =>
                utcWallClock ? (byte)'T' : (byte)'W',
            "datetimeoffset" when native => (byte)'O',
            "binary" or "varbinary" or "image" or "timestamp" or "rowversion" => (byte)'Y',
            _ => (byte)'S'
        };
    }

    private static byte[] ColumnarKinds(JsonElement root, string[] types)
    {
        bool native = GetString(root, "columnarPrecision", "full") == "native";
        bool nativeTimestamp = GetString(root, "columnarDatetime", "character") == "POSIXct";
        string tz = GetString(root, "columnarTz", "");
        bool utcWallClock = tz == "UTC" || tz == "GMT";
        var kinds = new byte[types.Length];
        for (int j=0; j<types.Length; j++)
            kinds[j] = WireKind(types[j], native, utcWallClock, nativeTimestamp);
        return kinds;
    }

    private static string[] KindLabels(byte[] kinds) =>
        Array.ConvertAll(kinds, k => ((char)k).ToString());

    private static readonly long EpochTicks = new DateTime(1970, 1, 1).Ticks;
    private static int DateDays(object v) =>
        checked((int)((((DateTime)v).Date.Ticks - EpochTicks) / TimeSpan.TicksPerDay));
    private static long WallTicks(object v) => ((DateTime)v).Ticks;
    private static long UtcTicks(object v) => ((DateTimeOffset)v).UtcDateTime.Ticks;
    private static long TimeTicks(object v) => ((TimeSpan)v).Ticks;

    // UTF-8 field encoding reuses one pooled buffer per SCB2 batch instead of
    // allocating a new byte[] for every XML/NVARCHAR value. Both the length
    // and the encoded bytes use the same UTF-8 encoder replacement fallback.
    private static void WriteUtf8ColumnarField(BinaryWriter writer, string value,
                                              ref byte[] scratch, int knownUtf8Bytes = -1)
    {
        int length = knownUtf8Bytes >= 0 ? knownUtf8Bytes : Encoding.UTF8.GetByteCount(value);
        writer.Write(checked((uint)length));
        if (length == 0) return;
        if (length > scratch.Length)
        {
            byte[] larger = ArrayPool<byte>.Shared.Rent(length);
            ArrayPool<byte>.Shared.Return(scratch);
            scratch = larger;
        }
        int encoded = Encoding.UTF8.GetBytes(value.AsSpan(), scratch.AsSpan(0, length));
        if (encoded != length)
            throw new InvalidOperationException("UTF-8 byte count mismatch in SCB2 field.");
        writer.Write(scratch, 0, length);
    }

    // A D frame has a hard 128 MiB limit in the Rcpp receiver. Keep the
    // *encoded* SCB2 payload within that limit, even for VARCHAR(MAX), XML,
    // VARBINARY(MAX), and wide result sets. The SCB2 header is 12 bytes;
    // each column contributes one kind byte and ceil(rows/8) validity bytes.
    // Fixed-width fields are written even when SQL NULL; strings and blobs
    // always have a 4-byte length prefix (including NULL and empty values).
    private const long MaxColumnarFrameBytes = 128L * 1024L * 1024L;

    private static long ColumnarValueBytes(byte kind, string sqlType, object? value)
    {
        return kind switch
        {
            (byte)'B' => 1L,
            (byte)'I' or (byte)'A' => 4L,
            (byte)'L' or (byte)'D' or (byte)'N' or (byte)'H' or
                (byte)'T' or (byte)'W' or (byte)'O' => 8L,
            (byte)'Y' => 4L + (value is null || value is DBNull
                ? 0L : ((byte[])value).LongLength),
            (byte)'S' => 4L + (value is null || value is DBNull
                ? 0L : Encoding.UTF8.GetByteCount(PlainValue(value, sqlType))),
            _ => throw new InvalidOperationException("Unsupported columnar kind.")
        };
    }

    // Compute all fixed widths (including the 4-byte S/Y length prefixes)
    // once per SQL result schema. Only variable-sized values need inspection
    // for each row. In a 100-column result with 25 variable columns this
    // avoids 75 redundant kind switches per row (7.5 million per 100k rows).
    // Keep exact UTF-8 byte counting for non-ASCII / XML values: the result
    // is used to enforce the 128 MiB SCB2 frame limit before serialization.
    private sealed class ColumnarRowSizer
    {
        private readonly long fixedBytesPerRow;
        private readonly int[] variableIndices;
        private readonly bool[] isText;
        private readonly string[] variableSqlTypes;
        private readonly int columnCount;

        public ColumnarRowSizer(byte[] kinds, string[] types)
        {
            if (kinds.Length != types.Length)
                throw new InvalidOperationException("Columnar schema mismatch.");
            columnCount = kinds.Length;
            var indices = new System.Collections.Generic.List<int>();
            var textFlags = new System.Collections.Generic.List<bool>();
            var sqlTypes = new System.Collections.Generic.List<string>();
            long constant = 0;
            for (int j = 0; j < kinds.Length; ++j)
            {
                switch (kinds[j])
                {
                    case (byte)'B': constant = checked(constant + 1L); break;
                    case (byte)'I':
                    case (byte)'A': constant = checked(constant + 4L); break;
                    case (byte)'L':
                    case (byte)'D':
                    case (byte)'N':
                    case (byte)'H':
                    case (byte)'T':
                    case (byte)'W':
                    case (byte)'O': constant = checked(constant + 8L); break;
                    case (byte)'S':
                    case (byte)'Y':
                        constant = checked(constant + 4L);
                        indices.Add(j);
                        textFlags.Add(kinds[j] == (byte)'S');
                        sqlTypes.Add(types[j]);
                        break;
                    default: throw new InvalidOperationException("Unsupported columnar kind.");
                }
            }
            fixedBytesPerRow = constant;
            variableIndices = indices.ToArray();
            isText = textFlags.ToArray();
            variableSqlTypes = sqlTypes.ToArray();
        }

        public long RowBytes(object?[] row)
        {
            if (row.Length != columnCount)
                throw new InvalidOperationException("Column count mismatch.");
            long bytes = fixedBytesPerRow;
            for (int k = 0; k < variableIndices.Length; ++k)
            {
                object? value = row[variableIndices[k]];
                if (value is null || value is DBNull) continue;
                bytes = checked(bytes + (isText[k]
                    ? Encoding.UTF8.GetByteCount(PlainValue(value, variableSqlTypes[k]))
                    : ((byte[])value).LongLength));
            }
            return bytes;
        }
    }

    private sealed class ColumnarBatchBudget
    {
        private readonly int columns;
        private long valueBytes;
        public int Rows { get; private set; }
        public ColumnarBatchBudget(int columns) { this.columns = columns; }
        private long SizeWith(long nextRowBytes) => checked(
            12L + columns +
            (long)columns * ((Rows + 1L + 7L) / 8L) +
            valueBytes + nextRowBytes);
        public long FrameBytes => checked(
            12L + columns +
            (long)columns * ((Rows + 7L) / 8L) + valueBytes);
        public bool Fits(long nextRowBytes) => SizeWith(nextRowBytes) <= MaxColumnarFrameBytes;
        public void Add(long nextRowBytes)
        {
            if (!Fits(nextRowBytes))
                throw new InvalidOperationException(
                    "Columnar batch exceeds 128 MiB: one SQL row cannot fit in a frame.");
            valueBytes = checked(valueBytes + nextRowBytes);
            ++Rows;
        }
        public void Clear() { Rows = 0; valueBytes = 0; }
    }

    private static void WriteColumnarBatch(byte[] kinds, string[] types,
                                           System.Collections.Generic.List<object?[]> rows,
                                           ref long batches, ref long bytes,
                                           long expectedFrameBytes)
    {
        // The budgeting pass knows the *exact* SCB2 size. Reserve it once
        // instead of repeated MemoryStream growth and large array copies.
        if (expectedFrameBytes < 0 || expectedFrameBytes > MaxColumnarFrameBytes)
            throw new InvalidOperationException("Columnar frame size exceeds 128 MiB.");
        using var ms = new MemoryStream(checked((int)expectedFrameBytes));
        // The scratch buffer is reused across ALL text/XML rows in the batch.
        byte[] utf8Scratch = ArrayPool<byte>.Shared.Rent(4096);
        try
        {
            using (var writer = new BinaryWriter(ms, new UTF8Encoding(false), leaveOpen: true))
            {
                writer.Write(new byte[] { (byte)'S', (byte)'C', (byte)'B', (byte)'2' });
                writer.Write((uint)rows.Count);
                writer.Write((uint)types.Length);
                for (int j = 0; j < types.Length; j++)
                {
                    byte kind = kinds[j];
                    writer.Write(kind);
                    byte[] bitmap = new byte[(rows.Count + 7) / 8];
                    for (int i = 0; i < rows.Count; i++)
                        if (rows[i][j] is not null && rows[i][j] is not DBNull)
                            bitmap[i >> 3] |= (byte)(1 << (i & 7));
                    writer.Write(bitmap);
                    for (int i = 0; i < rows.Count; i++)
                    {
                        object? v = rows[i][j];
                        bool present = (bitmap[i >> 3] & (1 << (i & 7))) != 0;
                        switch (kind)
                        {
                            case (byte)'I':
                                writer.Write(present ? Convert.ToInt32(v, CultureInfo.InvariantCulture) : 0);
                                break;
                            case (byte)'L':
                                writer.Write(present ? Convert.ToInt64(v, CultureInfo.InvariantCulture) : 0L);
                                break;
                            case (byte)'D':
                            {
                                // SQL Server FLOAT is CLR double; REAL is CLR float.
                                // Avoid Convert.ToDouble(object, provider) and its
                                // IConvertible interface dispatch on every cell.
                                // This also makes unexpected provider types fail
                                // with a managed exception identifying the column.
                                if (!present) writer.Write(0.0);
                                else if (v is double number) writer.Write(number);
                                else if (v is float single) writer.Write((double)single);
                                else throw new InvalidDataException(
                                    $"Unexpected CLR value for SQL {types[j]} in column {j + 1}: " +
                                    (v?.GetType().FullName ?? "null"));
                                break;
                            }
                            case (byte)'N':
                            {
                                // SqlDecimal can exceed CLR decimal precision. A lossless
                                // invariant string is parsed to double only in native mode.
                                double value = present
                                    ? double.Parse(PlainValue(v!, types[j]), NumberStyles.Float,
                                                   CultureInfo.InvariantCulture) : 0.0;
                                if (!double.IsFinite(value))
                                    throw new InvalidOperationException("Non-finite SQL decimal conversion.");
                                writer.Write(value);
                                break;
                            }
                            case (byte)'B':
                                writer.Write((byte)(present && Convert.ToBoolean(v, CultureInfo.InvariantCulture) ? 1 : 0));
                                break;
                            case (byte)'A':
                                writer.Write(present ? DateDays(v!) : 0);
                                break;
                            case (byte)'H':
                                writer.Write(present ? TimeTicks(v!) : 0L);
                                break;
                            case (byte)'T':
                            case (byte)'W':
                                writer.Write(present ? WallTicks(v!) : 0L);
                                break;
                            case (byte)'O':
                                writer.Write(present ? UtcTicks(v!) : 0L);
                                break;
                            case (byte)'S':
                            {
                                string value = present ? PlainValue(v!, types[j]) : "";
                                WriteUtf8ColumnarField(writer, value, ref utf8Scratch);
                                break;
                            }
                            case (byte)'Y':
                            {
                                byte[] blob = present ? (byte[])v! : Array.Empty<byte>();
                                writer.Write(checked((uint)blob.Length));
                                writer.Write(blob);
                                break;
                            }
                            default: throw new InvalidOperationException("Unsupported columnar kind.");
                        }
                    }
                }
            }
        }
        finally
        {
            ArrayPool<byte>.Shared.Return(utf8Scratch);
        }
        // Fail closed if size planning ever diverges from actual SCB2 encoding.
        // No partial/incorrect data frame can be sent to R in that case.
        if (ms.Length != expectedFrameBytes)
            throw new InvalidOperationException(
                $"SCB2 frame size mismatch: expected {expectedFrameBytes}, encoded {ms.Length}.");
        WriteFrame((byte)'D', ms.GetBuffer().AsSpan(0, checked((int)ms.Length)));
        ++batches;
        bytes += ms.Length;
    }

    // DBI single-column VARBINARY/XML/plain-text fast path. Avoid allocating
    // an object?[] per row and keep opt-in allocation diagnostics available
    // when comparing XML with CONVERT(NVARCHAR(MAX), xml_col).
    // Other 'S' kinds (dates/decimals/unsupported) are intentionally excluded.
    private static bool SingleColumnBinaryOrXml(CursorState state) =>
        state.Types.Length == 1 &&
        (state.Kinds[0] == (byte)'Y' ||
         (state.Kinds[0] == (byte)'S' &&
          (state.Types[0].Equals("xml", StringComparison.OrdinalIgnoreCase) ||
           state.Types[0].Equals("varchar", StringComparison.OrdinalIgnoreCase) ||
           state.Types[0].Equals("nvarchar", StringComparison.OrdinalIgnoreCase) ||
           state.Types[0].Equals("char", StringComparison.OrdinalIgnoreCase) ||
           state.Types[0].Equals("nchar", StringComparison.OrdinalIgnoreCase) ||
           state.Types[0].Equals("text", StringComparison.OrdinalIgnoreCase) ||
           state.Types[0].Equals("ntext", StringComparison.OrdinalIgnoreCase))));

    private static void WriteSingleColumnBatch(byte kind, string sqlType,
                                               System.Collections.Generic.List<object?> values,
                                               ref long batches, ref long bytes,
                                               long expectedFrameBytes,
                                               System.Collections.Generic.List<int>? utf8Lengths = null)
    {
        // The budgeting pass knows the *exact* SCB2 size. Reserve it once
        // instead of repeated MemoryStream growth and large array copies.
        if (expectedFrameBytes < 0 || expectedFrameBytes > MaxColumnarFrameBytes)
            throw new InvalidOperationException("Columnar frame size exceeds 128 MiB.");
        using var ms = new MemoryStream(checked((int)expectedFrameBytes));
        byte[] utf8Scratch = ArrayPool<byte>.Shared.Rent(4096);
        try
        {
            using var writer = new BinaryWriter(ms, new UTF8Encoding(false), leaveOpen: true);
            writer.Write((byte)'S'); writer.Write((byte)'C');
            writer.Write((byte)'B'); writer.Write((byte)'2');
            writer.Write(checked((uint)values.Count));
            writer.Write(1u);
            writer.Write(kind);
            var bitmap = new byte[(values.Count + 7) / 8];
            for (int i = 0; i < values.Count; ++i)
                if (values[i] is not null && values[i] is not DBNull)
                    bitmap[i >> 3] |= (byte)(1 << (i & 7));
            writer.Write(bitmap);
            for (int i = 0; i < values.Count; ++i)
            {
                object? v = values[i];
                if (kind == (byte)'Y')
                {
                    byte[] raw = v is null || v is DBNull
                        ? Array.Empty<byte>() : (byte[])v;
                    writer.Write(checked((uint)raw.Length));
                    writer.Write(raw);
                }
                else
                {
                    string value = v is null || v is DBNull ? "" : PlainValue(v, sqlType);
                    WriteUtf8ColumnarField(writer, value, ref utf8Scratch,
                        utf8Lengths is null ? -1 : utf8Lengths[i]);
                }
            }
        }
        finally
        {
            ArrayPool<byte>.Shared.Return(utf8Scratch);
        }
        // Fail closed if size planning ever diverges from actual SCB2 encoding.
        // No partial/incorrect data frame can be sent to R in that case.
        if (ms.Length != expectedFrameBytes)
            throw new InvalidOperationException(
                $"SCB2 frame size mismatch: expected {expectedFrameBytes}, encoded {ms.Length}.");
        WriteFrame((byte)'D', ms.GetBuffer().AsSpan(0, checked((int)ms.Length)));
        ++batches;
        bytes += ms.Length;
    }

    private static object?[] CurrentRow(SqlDataReader reader, string[] types)
    {
        var row = new object?[reader.FieldCount];
        for (int j = 0; j < reader.FieldCount; j++)
        {
            if (reader.IsDBNull(j)) { row[j] = null; continue; }
            // Read FLOAT/REAL through the provider's typed CLR accessors,
            // rather than GetValue() followed by IConvertible dispatch.
            // Preserve SqlDecimal for values beyond CLR decimal precision.
            if (types[j].Equals("float", StringComparison.OrdinalIgnoreCase))
                row[j] = reader.GetDouble(j);
            else if (types[j].Equals("real", StringComparison.OrdinalIgnoreCase))
                row[j] = reader.GetFloat(j);
            else if (types[j].Equals("decimal", StringComparison.OrdinalIgnoreCase) ||
                     types[j].Equals("numeric", StringComparison.OrdinalIgnoreCase))
                row[j] = reader.GetSqlDecimal(j);
            else
                row[j] = reader.GetValue(j);
        }
        return row;
    }

    private static System.Collections.Generic.IEnumerable<object?[]> ReaderRows(SqlDataReader reader,
                                                                                string[] types)
    {
        while (reader.Read()) yield return CurrentRow(reader, types);
    }

    private static void EmitColumnar(byte[] kinds, string[] types,
                                    System.Collections.Generic.IEnumerable<object?[]> source,
                                    int batchRows, out long rows, out long batches, out long bytes)
    {
        rows = 0;
        batches = 0;
        bytes = 0;
        var batch = new System.Collections.Generic.List<object?[]>(batchRows);
        var budget = new ColumnarBatchBudget(types.Length);
        var rowSizer = new ColumnarRowSizer(kinds, types);
        foreach (object?[] item in source)
        {
            if (item.Length != types.Length) throw new InvalidOperationException("Column count mismatch.");
            long valueBytes = rowSizer.RowBytes(item);
            if (batch.Count > 0 && !budget.Fits(valueBytes))
            {
                WriteColumnarBatch(kinds, types, batch, ref batches, ref bytes, budget.FrameBytes);
                batch.Clear();
                budget.Clear();
            }
            budget.Add(valueBytes); // rejects a single row larger than 128 MiB
            batch.Add(item);
            ++rows;
            if (batch.Count == batchRows)
            {
                WriteColumnarBatch(kinds, types, batch, ref batches, ref bytes, budget.FrameBytes);
                batch.Clear();
                budget.Clear();
            }
        }
        // Emit at least one batch, including for an empty SQL result.
        if (batch.Count > 0 || batches == 0)
            WriteColumnarBatch(kinds, types, batch, ref batches, ref bytes, budget.FrameBytes);
    }

    // Samples this helper's actual OS working set during ONE query. These are
    // sampled peaks (not a guaranteed instantaneous peak or lifetime maxima).
    private sealed class QueryMemorySampler : IDisposable
    {
        private readonly Process process = Process.GetCurrentProcess();
        private readonly System.Threading.Timer timer;
        private readonly object mutex = new();
        private long peakWorkingSet;
        private long peakPrivate;
        public long StartWorkingSet { get; private set; }

        public QueryMemorySampler()
        {
            TrySample();
            StartWorkingSet = peakWorkingSet;
            timer = new System.Threading.Timer(_ => TrySample(), null, 25, 25);
        }

        private void Sample()
        {
            lock (mutex)
            {
                process.Refresh();
                peakWorkingSet = Math.Max(peakWorkingSet, process.WorkingSet64);
                peakPrivate = Math.Max(peakPrivate, process.PrivateMemorySize64);
            }
        }

        private void TrySample()
        {
            try { Sample(); } catch { /* diagnostic sampling must never fail a query */ }
        }
        public (long PeakWorkingSet, long PeakPrivate) Snapshot()
        {
            TrySample();
            lock (mutex) return (peakWorkingSet, peakPrivate);
        }
        public void Dispose()
        {
            timer.Dispose();
            process.Dispose();
        }
    }

    private static void Query(JsonElement root)
    {
        using var memory = new QueryMemorySampler();
        using var cmd = CreateCommand(root);
        var sw = Stopwatch.StartNew();
        using var reader = cmd.ExecuteReader(CommandBehavior.SequentialAccess);
        double executeSeconds = sw.Elapsed.TotalSeconds;
        string[] columns = new string[reader.FieldCount];
        string[] types = new string[reader.FieldCount];
        for (int j = 0; j < reader.FieldCount; j++)
        {
            columns[j] = reader.GetName(j);
            types[j] = reader.GetDataTypeName(j);
        }
        
        if (ColumnarRequested(root))
        {
            int batchRows = ColumnarBatchRows(root);
            // Guard SQL data-reader path until the fatal 65536-row batch issue
            // has been isolated on Windows. The memory_test diagnostic may
            // still exercise 65536 rows without SQL Server / native SNI.
            if (batchRows > 32768)
                throw new ArgumentOutOfRangeException(nameof(batchRows),
                    "SQL columnar batches over 32768 rows are temporarily disabled in 0.0.27; " +
                    "use sql_memory_test() to isolate the serializer independently of SqlClient.");
            byte[] kinds = ColumnarKinds(root, types);
            CurrentResponseStarted = true;
            Reply(new { ok = true, kind = "start", columns, types, wireKinds = KindLabels(kinds),
                        valueEncoding = "columnar-v2", batchRows });
            EmitColumnar(kinds, types, ReaderRows(reader, types), batchRows,
                         out long count, out long batches, out long payloadBytes);
            var mem = memory.Snapshot();
            Reply(new { ok = true, kind = "end", rows = count,
                        executeSeconds, fetchAndStreamSeconds = sw.Elapsed.TotalSeconds - executeSeconds,
                        chunksSent = batches, bytesSent = payloadBytes,
                        helperPeakWorkingSetBytes = Process.GetCurrentProcess().PeakWorkingSet64,
                        helperStartWorkingSetBytes = memory.StartWorkingSet,
                        helperQueryPeakWorkingSetBytes = mem.PeakWorkingSet,
                        helperQueryPeakPrivateBytes = mem.PeakPrivate });
            return;
        }
        int chunkSizeKiB = GetInt(root, "chunkSizeKiB", 256);
        if (chunkSizeKiB < 8 || chunkSizeKiB > 1024)
            throw new ArgumentOutOfRangeException(nameof(chunkSizeKiB), "Expected 8..1024 KiB.");
        int chunkSizeBytes = checked(chunkSizeKiB * 1024);
        CurrentResponseStarted = true;
        Reply(new { ok = true, kind = "start", columns, types, valueEncoding = "tagged-v1", chunkSizeBytes });

        // Buffered writing; only the chunk bytes pass through stdout.
        // Neither StreamWriter nor data.table::fread(text=scalar) needs a data file.
        long rows;
        var stream = new ChunkedOutputStream(chunkSizeBytes);
        using (stream)
        using (var writer = new StreamWriter(stream, new UTF8Encoding(false), 16 * 1024,
                                              leaveOpen: false) { NewLine = "\n" })
        {
            WriteCsvHeaderAndRows(writer, columns, types, reader, out rows);
        }
        var csvMem = memory.Snapshot();
        Reply(new { ok = true, kind = "end", rows, executeSeconds,
                    fetchAndStreamSeconds = sw.Elapsed.TotalSeconds - executeSeconds,
                    chunksSent = stream.ChunksSent, bytesSent = stream.BytesSent,
                    helperPeakWorkingSetBytes = Process.GetCurrentProcess().PeakWorkingSet64,
                    helperStartWorkingSetBytes = memory.StartWorkingSet,
                    helperQueryPeakWorkingSetBytes = csvMem.PeakWorkingSet,
                    helperQueryPeakPrivateBytes = csvMem.PeakPrivate });
    }

    // DBI cursor lifecycle. One SqlDataReader per helper process, with an
    // optional one-row lookahead so dbHasCompleted() is correct even when
    // dbFetch(n) ends exactly on the last row. Fetch never re-runs SQL.
    private sealed class CursorState : IDisposable
    {
        public long Id;
        public SqlCommand Command;
        public SqlDataReader Reader;
        public string[] Columns;
        public string[] Types;
        public byte[] Kinds;
        public ColumnarRowSizer RowSizer;
        public object?[]? NextRow;
        public bool Complete;
        public int BatchRows;
        public double ExecuteSeconds;

        public CursorState(long id, SqlCommand command, SqlDataReader reader,
                           string[] columns, string[] types, byte[] kinds,
                           int batchRows, double executeSeconds)
        {
            Id = id; Command = command; Reader = reader;
            Columns = columns; Types = types; Kinds = kinds;
            RowSizer = new ColumnarRowSizer(kinds, types);
            BatchRows = batchRows; ExecuteSeconds = executeSeconds;
        }
        public void Dispose()
        {
            Reader.Dispose();
            Command.Dispose();
        }
    }

    private static void CloseCursor()
    {
        var old = cursor;
        cursor = null;
        old?.Dispose();
    }

    private static void CursorOpen(JsonElement root)
    {
        RequireOpen();
        if (cursor != null)
            throw new InvalidOperationException("An active result must be cleared before opening another.");
        int batchRows = ColumnarBatchRows(root);
        if (batchRows > 32768)
            throw new ArgumentOutOfRangeException(nameof(batchRows), "DBI batch_rows must be <= 32768.");
        var cmd = CreateCommand(root);
        SqlDataReader? reader = null;
        try
        {
            var sw = Stopwatch.StartNew();
            reader = cmd.ExecuteReader(CommandBehavior.SequentialAccess);
            double seconds = sw.Elapsed.TotalSeconds;
            var columns = new string[reader.FieldCount];
            var types = new string[reader.FieldCount];
            for (int j = 0; j < types.Length; j++)
            {
                columns[j] = reader.GetName(j);
                types[j] = reader.GetDataTypeName(j);
            }
            var kinds = ColumnarKinds(root, types);
            var state = new CursorState(++nextCursorId, cmd, reader,
                                        columns, types, kinds, batchRows, seconds);
            if (reader.Read()) state.NextRow =
                types.Length == 1 && (types[0].Equals("xml", StringComparison.OrdinalIgnoreCase) ||
                                      types[0].Equals("varbinary", StringComparison.OrdinalIgnoreCase))
                    ? new object?[] { SingleColumnValue(reader) }
                    : CurrentRow(reader, types);
            else state.Complete = true;
            cursor = state;
            Reply(new { ok = true, kind = "cursor_open", cursorId = state.Id,
                        columns, types, wireKinds = KindLabels(kinds),
                        valueEncoding = "columnar-v2", batchRows,
                        hasMore = !state.Complete });
        }
        catch
        {
            // If cursor ownership was transferred, CloseCursor() disposes both.
            if (cursor != null) CloseCursor();
            else { reader?.Dispose(); cmd.Dispose(); }
            throw;
        }
    }

    // Single-column VARBINARY/XML: one accessor also represents SQL NULL.
    // GetValue() returns DBNull.Value for nulls, byte[] for VARBINARY and
    // string for XML. Avoid a redundant IsDBNull() call per row.
    private static object? SingleColumnValue(SqlDataReader reader)
    {
        object value = reader.GetValue(0);
        return value is DBNull ? null : value;
    }

    private static void CursorFetch(JsonElement root)
    {
        var state = cursor ?? throw new InvalidOperationException("No active DBI cursor.");
        long id = root.GetProperty("cursorId").GetInt64();
        if (id != state.Id) throw new InvalidOperationException("DBI cursor identifier mismatch.");
        int requested = GetInt(root, "n", -1);
        if (requested < -1) throw new ArgumentOutOfRangeException(nameof(requested));
        var sw = Stopwatch.StartNew();
        double readAndBufferSeconds = 0, encodeAndPipeSeconds = 0;
        var stage = Stopwatch.StartNew();
        CurrentResponseStarted = true;
        Reply(new { ok = true, kind = "start", valueEncoding = "columnar-v2", cursorId = id });
        long rows = 0, chunks = 0, bytes = 0;
        object? singleColumnAllocations = null;
        try
        {
            if (SingleColumnBinaryOrXml(state))
            {
                // Binary and XML often have millions of one-column rows. The
                // standard path allocates one object?[] per row then traverses
                // that array again. Keep just the value itself per row.
                var values = new System.Collections.Generic.List<object?>(
                    Math.Min(state.BatchRows, 32768));
                // Reuse UTF-8 byte lengths computed by adaptive frame budgeting.
                // A parallel int list (~128 KiB for 32768 rows) avoids a second
                // GetByteCount() over every XML string while emitting SCB2.
                bool isXml = state.Types[0].Equals("xml", StringComparison.OrdinalIgnoreCase);
                bool isText = state.Kinds[0] == (byte)'S';
                var utf8Lengths = isText ? new System.Collections.Generic.List<int>(
                    Math.Min(state.BatchRows, 32768)) : null;
                bool allocProfile = GetBool(root, "allocationProfile", false);
                long allocBefore = allocProfile ? GC.GetAllocatedBytesForCurrentThread() : 0;
                long totalBefore = allocProfile ? GC.GetTotalAllocatedBytes(false) : 0;
                int[] gcBefore = allocProfile ? new[] { GC.CollectionCount(0), GC.CollectionCount(1), GC.CollectionCount(2) } : Array.Empty<int>();
                long valueReadBytes = 0, lengthBytes = 0, encodeBytes = 0;
                long valueReadTicks = 0, lengthTicks = 0, encodeTicks = 0;
                long materialized = 0;
                var budget = new ColumnarBatchBudget(1);
                while (!state.Complete && (requested < 0 || rows < requested))
                {
                    object? value;
                    if (state.NextRow is not null)
                    {
                        value = state.NextRow[0];
                        state.NextRow = null;
                    }
                    else
                    {
                        if (!state.Reader.Read()) { state.Complete = true; break; }
                        if (allocProfile)
                        {
                            long a = GC.GetAllocatedBytesForCurrentThread();
                            long t = Stopwatch.GetTimestamp();
                            value = SingleColumnValue(state.Reader);
                            valueReadTicks += Stopwatch.GetTimestamp() - t;
                            valueReadBytes += GC.GetAllocatedBytesForCurrentThread() - a;
                        }
                        else value = SingleColumnValue(state.Reader);
                    }
                    long valueBytes;
                    if (allocProfile)
                    {
                        long a = GC.GetAllocatedBytesForCurrentThread();
                        long t = Stopwatch.GetTimestamp();
                        valueBytes = ColumnarValueBytes(state.Kinds[0], state.Types[0], value);
                        lengthTicks += Stopwatch.GetTimestamp() - t;
                        lengthBytes += GC.GetAllocatedBytesForCurrentThread() - a;
                    }
                    else valueBytes = ColumnarValueBytes(state.Kinds[0], state.Types[0], value);
                    if (value is not null && value is not DBNull) materialized++;
                    if (values.Count > 0 && !budget.Fits(valueBytes))
                    {
                        readAndBufferSeconds += stage.Elapsed.TotalSeconds;
                        stage.Restart();
                        long encodeBefore = allocProfile ? GC.GetAllocatedBytesForCurrentThread() : 0;
                        long encodeStart = allocProfile ? Stopwatch.GetTimestamp() : 0;
                        WriteSingleColumnBatch(state.Kinds[0], state.Types[0],
                                               values, ref chunks, ref bytes, budget.FrameBytes, utf8Lengths);
                        if (allocProfile)
                        {
                            encodeTicks += Stopwatch.GetTimestamp() - encodeStart;
                            encodeBytes += GC.GetAllocatedBytesForCurrentThread() - encodeBefore;
                        }
                        encodeAndPipeSeconds += stage.Elapsed.TotalSeconds;
                        stage.Restart();
                        values.Clear();
                        utf8Lengths?.Clear();
                        budget.Clear();
                    }
                    budget.Add(valueBytes);
                    values.Add(value);
                    utf8Lengths?.Add(checked((int)(valueBytes - 4L)));
                    rows++;
                    if (values.Count == state.BatchRows)
                    {
                        readAndBufferSeconds += stage.Elapsed.TotalSeconds;
                        stage.Restart();
                        long encodeBefore = allocProfile ? GC.GetAllocatedBytesForCurrentThread() : 0;
                        long encodeStart = allocProfile ? Stopwatch.GetTimestamp() : 0;
                        WriteSingleColumnBatch(state.Kinds[0], state.Types[0],
                                               values, ref chunks, ref bytes, budget.FrameBytes, utf8Lengths);
                        if (allocProfile)
                        {
                            encodeTicks += Stopwatch.GetTimestamp() - encodeStart;
                            encodeBytes += GC.GetAllocatedBytesForCurrentThread() - encodeBefore;
                        }
                        encodeAndPipeSeconds += stage.Elapsed.TotalSeconds;
                        stage.Restart();
                        values.Clear();
                        utf8Lengths?.Clear();
                        budget.Clear();
                    }
                }
                if (!state.Complete && state.NextRow is null)
                {
                    if (state.Reader.Read())
                        state.NextRow = new object?[] {
                            SingleColumnValue(state.Reader)
                        };
                    else state.Complete = true;
                }
                readAndBufferSeconds += stage.Elapsed.TotalSeconds;
                stage.Restart();
                if (values.Count != 0 || chunks == 0)
                {
                    long encodeBefore = allocProfile ? GC.GetAllocatedBytesForCurrentThread() : 0;
                    long encodeStart = allocProfile ? Stopwatch.GetTimestamp() : 0;
                    WriteSingleColumnBatch(state.Kinds[0], state.Types[0],
                                           values, ref chunks, ref bytes, budget.FrameBytes, utf8Lengths);
                    if (allocProfile)
                    {
                        encodeTicks += Stopwatch.GetTimestamp() - encodeStart;
                        encodeBytes += GC.GetAllocatedBytesForCurrentThread() - encodeBefore;
                    }
                }
                encodeAndPipeSeconds += stage.Elapsed.TotalSeconds;
                if (allocProfile)
                {
                    singleColumnAllocations = new {
                        xmlReadMode = isXml ? "getvalue" : "not-xml",
                        threadBytes = GC.GetAllocatedBytesForCurrentThread() - allocBefore,
                        processBytes = GC.GetTotalAllocatedBytes(false) - totalBefore,
                        valueReadBytes, lengthBytes, encodeBytes, materialized,
                        getValueMilliseconds = valueReadTicks * 1000.0 / Stopwatch.Frequency,
                        lengthMilliseconds = lengthTicks * 1000.0 / Stopwatch.Frequency,
                        encodeMilliseconds = encodeTicks * 1000.0 / Stopwatch.Frequency,
                        gen0 = GC.CollectionCount(0) - gcBefore[0],
                        gen1 = GC.CollectionCount(1) - gcBefore[1],
                        gen2 = GC.CollectionCount(2) - gcBefore[2]
                    };
                }
            }
            else
            {
                var batch = new System.Collections.Generic.List<object?[]>(
                    Math.Min(state.BatchRows, 32768));
                var budget = new ColumnarBatchBudget(state.Types.Length);
                while (!state.Complete && (requested < 0 || rows < requested))
                {
                    object?[]? row = state.NextRow;
                    state.NextRow = null;
                    if (row == null)
                    {
                        if (!state.Reader.Read()) { state.Complete = true; break; }
                        row = CurrentRow(state.Reader, state.Types);
                    }
                    long valueBytes = state.RowSizer.RowBytes(row);
                    if (batch.Count > 0 && !budget.Fits(valueBytes))
                    {
                        readAndBufferSeconds += stage.Elapsed.TotalSeconds;
                        stage.Restart();
                        WriteColumnarBatch(state.Kinds, state.Types, batch, ref chunks, ref bytes, budget.FrameBytes);
                        encodeAndPipeSeconds += stage.Elapsed.TotalSeconds;
                        stage.Restart();
                        batch.Clear();
                        budget.Clear();
                    }
                    budget.Add(valueBytes);
                    batch.Add(row);
                    rows++;
                    if (batch.Count == state.BatchRows)
                    {
                        readAndBufferSeconds += stage.Elapsed.TotalSeconds;
                        stage.Restart();
                        WriteColumnarBatch(state.Kinds, state.Types, batch, ref chunks, ref bytes, budget.FrameBytes);
                        encodeAndPipeSeconds += stage.Elapsed.TotalSeconds;
                        stage.Restart();
                        batch.Clear();
                        budget.Clear();
                    }
                }
                if (!state.Complete && state.NextRow == null)
                {
                    if (state.Reader.Read()) state.NextRow = CurrentRow(state.Reader, state.Types);
                    else state.Complete = true;
                }
                readAndBufferSeconds += stage.Elapsed.TotalSeconds;
                stage.Restart();
                if (batch.Count != 0 || chunks == 0)
                    WriteColumnarBatch(state.Kinds, state.Types, batch, ref chunks, ref bytes, budget.FrameBytes);
                encodeAndPipeSeconds += stage.Elapsed.TotalSeconds;
            }
            Reply(new { ok = true, kind = "end", cursorId = id, rows,
                        chunksSent = chunks, bytesSent = bytes,
                        hasMore = !state.Complete,
                        executeSeconds = state.ExecuteSeconds,
                        fetchAndStreamSeconds = sw.Elapsed.TotalSeconds,
                        readAndBufferSeconds, encodeAndPipeSeconds,
                        singleColumnFastPath = SingleColumnBinaryOrXml(state),
                        allocations = singleColumnAllocations });
        }
        catch
        {
            CloseCursor();
            throw;
        }
    }

    private static void CursorClose(JsonElement root)
    {
        var state = cursor ?? throw new InvalidOperationException("No active DBI cursor.");
        if (root.GetProperty("cursorId").GetInt64() != state.Id)
            throw new InvalidOperationException("DBI cursor identifier mismatch.");
        CloseCursor();
        Reply(new { ok = true, state = "cursor_closed" });
    }

    // Deterministic regression: emit query start and a data frame, then stall.
    // No SQL connection is required; lets R verify it kills a partial stream
    // rather than accidentally accepting its leftover frame on a later query.
    private static void StreamStallTest(JsonElement root)
    {
        int chunkKiB = GetInt(root, "chunkSizeKiB", 256);
        if (chunkKiB < 8 || chunkKiB > 1024)
            throw new ArgumentOutOfRangeException(nameof(chunkKiB));
        byte[] payload = Encoding.UTF8.GetBytes("\"id\"\n\"V1\"\n");
        Reply(new { ok = true, kind = "start",
                    columns = new[] { "id" }, types = new[] { "int" },
                    valueEncoding = "tagged-v1", chunkSizeBytes = chunkKiB * 1024 });
        WriteFrame((byte)'D', payload);
        int ms = Math.Clamp(GetInt(root, "milliseconds", 2000), 0, 30000);
        System.Threading.Thread.Sleep(ms);
        Reply(new { ok = true, kind = "end", rows = 1,
                    executeSeconds = 0.0, fetchAndStreamSeconds = ms / 1000.0,
                    chunksSent = 1, bytesSent = payload.Length });
    }

    // Simulates failure or hang AFTER a valid typed batch. Tests R-side
    // stream invalidation and stale-frame protection without SQL Server.
    private static void ColumnarFaultTest(JsonElement root)
    {
        string mode = GetString(root, "mode", "error");
        string[] columns = { "id" };
        string[] types = { "int" };
        int batchRows = ColumnarBatchRows(root);
        byte[] kinds = ColumnarKinds(root, types);
        Reply(new { ok = true, kind = "start", columns, types,
                    wireKinds = KindLabels(kinds), valueEncoding = "columnar-v2", batchRows });
        System.Collections.Generic.IEnumerable<object?[]> TestRows() {
            yield return new object?[] { 1 };
            yield return new object?[] { 2 };
        }
        EmitColumnar(kinds, types, TestRows(), batchRows,
                     out long rows, out long batches, out long bytes);
        if (mode == "error")
            throw new InvalidOperationException("Synthetic columnar mid-stream failure");
        if (mode == "stall") {
            System.Threading.Thread.Sleep(Math.Clamp(GetInt(root, "milliseconds", 2000), 0, 30000));
            Reply(new { ok = true, kind = "end", rows,
                        executeSeconds = 0.0, fetchAndStreamSeconds = 0.0,
                        chunksSent = batches, bytesSent = bytes });
            return;
        }
        throw new ArgumentException("Unknown columnar fault mode");
    }

    // Test the entire file-free query transport, without making a SQL connection.
    private static void MemoryTest(JsonElement root)
    {
        var sw = Stopwatch.StartNew();
        int chunkSizeKiB = GetInt(root, "chunkSizeKiB", 256);
        int nRows = GetInt(root, "nRows", 2);
        if (chunkSizeKiB < 8 || chunkSizeKiB > 1024)
            throw new ArgumentOutOfRangeException(nameof(chunkSizeKiB), "Expected 8..1024 KiB.");
        if (nRows < 2 || nRows > 100000)
            throw new ArgumentOutOfRangeException(nameof(nRows), "Expected 2..100000 rows.");
        int chunkSizeBytes = checked(chunkSizeKiB * 1024);
        string[] columns = { "id", "label", "nullable" };
        string[] types = { "int", "nvarchar", "nvarchar" };
        
        if (ColumnarRequested(root))
        {
            int batchRows = ColumnarBatchRows(root);
            byte[] kinds = ColumnarKinds(root, types);
            Reply(new { ok = true, kind = "start", columns, types, wireKinds = KindLabels(kinds),
                        valueEncoding = "columnar-v2", batchRows });
            string integrityMode = GetString(root, "integrityMode", "normal");
            if (integrityMode != "normal" && integrityMode != "duplicate" &&
                integrityMode != "out_of_range" && integrityMode != "reordered")
                throw new ArgumentException("Unknown integrityMode");
            System.Collections.Generic.IEnumerable<object?[]> TestRows()
            {
                yield return new object?[] { 1, "æøå café", "ok" };
                yield return new object?[] {
                    integrityMode == "reordered" ? 3 : 2,
                    "Line 1\nLine 2, \"citat\"", null };
                for (int i = 3; i <= nRows; i++) {
                    int id = i == 3 ? (integrityMode switch {
                        "duplicate" => 2,
                        "out_of_range" => nRows + 1,
                        "reordered" => 2,
                        _ => i
                    }) : i;
                    yield return new object?[] { id, "æøå, \"syntetisk\" " + new string('x', 120), "ok" };
                }
            }
            EmitColumnar(kinds, types, TestRows(), batchRows,
                         out long r, out long n, out long bytes);
            Reply(new { ok = true, kind = "end", rows = r, executeSeconds = 0.0,
                        fetchAndStreamSeconds = sw.Elapsed.TotalSeconds,
                        chunksSent = n, bytesSent = bytes });
            return;
        }
        Reply(new { ok = true, kind = "start", columns, types, valueEncoding = "tagged-v1", chunkSizeBytes });
        var stream = new ChunkedOutputStream(chunkSizeBytes);
        using (stream)
        using (var writer = new StreamWriter(stream, new UTF8Encoding(false), 16 * 1024,
                                              leaveOpen: false) { NewLine = "\n" })
        {
            writer.WriteLine(string.Join(",", Array.ConvertAll(columns, c => CsvHeader(c))));
            writer.WriteLine(CsvValue(1) + "," + CsvValue("æøå café") + "," + CsvValue("ok"));
            writer.WriteLine(CsvValue(2) + "," + CsvValue("Line 1\nLine 2, \"citat\"") + ",N");
            for (int i = 3; i <= nRows; i++)
                writer.WriteLine(CsvValue(i) + "," +
                                 CsvValue("æøå, \"syntetisk\" " + new string('x', 120)) + "," +
                                 CsvValue("ok"));
        }
        Reply(new { ok = true, kind = "end", rows = nRows,
                    executeSeconds = 0.0, fetchAndStreamSeconds = sw.Elapsed.TotalSeconds,
                    chunksSent = stream.ChunksSent, bytesSent = stream.BytesSent });
    }

    // A typed fixture to exercise all conversions without connecting to SQL Server.
    private static void TypesTest(JsonElement root)
    {
        var sw = Stopwatch.StartNew();
        int chunkSizeKiB = GetInt(root, "chunkSizeKiB", 256);
        if (chunkSizeKiB < 8 || chunkSizeKiB > 1024)
            throw new ArgumentOutOfRangeException(nameof(chunkSizeKiB));
        int chunkSizeBytes = checked(chunkSizeKiB * 1024);
        string[] columns = { "id", "flag", "tiny", "big", "exact_decimal", "cash",
            "dbl", "day", "stamp", "local_dt", "offset_dt", "clock", "guid",
            "code", "empty_text", "nullable_text", "blob", "empty_blob",
            "nullable_blob", "xml_text", "single" };
        string[] types = { "int", "bit", "tinyint", "bigint", "decimal", "money",
            "float", "date", "datetime2", "datetime", "datetimeoffset", "time",
            "uniqueidentifier", "nvarchar", "nvarchar", "nvarchar", "varbinary",
            "varbinary", "varbinary", "xml", "real" };

        object?[][] rows = {
            new object?[] { 1, true, (byte)255, long.MaxValue,
                SqlDecimal.Parse("1234567890123456789012345678.1234567890"), 12.3400m,
                1.2345678901234567, new DateTime(2026, 10, 16),
                new DateTime(2026, 10, 16, 12, 34, 56).AddTicks(1234567),
                new DateTime(2026, 10, 16, 12, 34, 56),
                new DateTimeOffset(2026, 10, 16, 12, 34, 56, TimeSpan.FromHours(2)).AddTicks(1234567),
                new TimeSpan(0, 12, 34, 56, 0).Add(TimeSpan.FromTicks(1234567)),
                new Guid("00112233-4455-6677-8899-aabbccddeeff"),
                "000123", "", "NA", new byte[] { 0, 1, 255 }, Array.Empty<byte>(),
                null, "<a x=\"1\">æøå</a>", 1.2345679f },
            new object?[] { 2, false, (byte)0, 9007199254740993L,
                SqlDecimal.Parse("-0.0000000000000000000000000001"), 0.0001m,
                0.0, new DateTime(2000, 1, 1),
                new DateTime(2000, 1, 1), new DateTime(2000, 1, 1),
                new DateTimeOffset(2000, 1, 1, 0, 0, 0, TimeSpan.FromHours(-5)),
                TimeSpan.Zero, Guid.Empty, "\"citat\"", "", "", new byte[] { 34, 44, 10 },
                Array.Empty<byte>(), null, "<x/>", -2.5f },
            new object?[] { 3, null, null, null, null, null, null, null, null, null,
                null, null, null, null, null, null, null, null, null, null, null }
        };
        if (ColumnarRequested(root))
        {
            int batchRows = ColumnarBatchRows(root);
            byte[] kinds = ColumnarKinds(root, types);
            Reply(new { ok = true, kind = "start", columns, types, wireKinds = KindLabels(kinds),
                        valueEncoding = "columnar-v2", batchRows });
            EmitColumnar(kinds, types, rows, batchRows, out long r, out long n, out long bytes);
            Reply(new { ok = true, kind = "end", rows = r, executeSeconds = 0.0,
                        fetchAndStreamSeconds = sw.Elapsed.TotalSeconds,
                        chunksSent = n, bytesSent = bytes });
            return;
        }
        Reply(new { ok = true, kind = "start", columns, types,
                    valueEncoding = "tagged-v1", chunkSizeBytes });
        long count = 0;
        var stream = new ChunkedOutputStream(chunkSizeBytes);
        using (stream)
        using (var writer = new StreamWriter(stream, new UTF8Encoding(false), 16 * 1024,
                                              leaveOpen: false) { NewLine = "\n" })
        {
            writer.WriteLine(string.Join(",", Array.ConvertAll(columns, c => CsvHeader(c))));
            foreach (object?[] row in rows)
            {
                for (int j = 0; j < row.Length; j++)
                {
                    if (j > 0) writer.Write(',');
                    writer.Write(CsvValue(row[j], types[j]));
                }
                writer.WriteLine();
                count++;
            }
        }
        Reply(new { ok = true, kind = "end", rows = count, executeSeconds = 0.0,
                    fetchAndStreamSeconds = sw.Elapsed.TotalSeconds,
                    chunksSent = stream.ChunksSent, bytesSent = stream.BytesSent });
    }

    // No SQL Server required. Exercises extremes and wire/NULL semantics
    // using the SAME columnar and CSV serializers as real queries.
    private static void EdgeTypesTest(JsonElement root)
    {
        var sw = Stopwatch.StartNew();
        int chunkSizeKiB = GetInt(root, "chunkSizeKiB", 256);
        if (chunkSizeKiB < 8 || chunkSizeKiB > 1024)
            throw new ArgumentOutOfRangeException(nameof(chunkSizeKiB));
        int chunkSizeBytes = checked(chunkSizeKiB * 1024);
        string[] columns = { "sql_int", "sql_bigint", "precise", "near_zero",
            "wall_clock", "absolute_clock", "sql_date", "clock_time",
            "unicode", "nullable_unicode", "binary_bytes", "null_binary",
            "floating", "bit_flag" };
        string[] types = { "int", "bigint", "decimal", "numeric", "datetime2",
            "datetimeoffset", "date", "time", "nvarchar", "nvarchar",
            "varbinary", "varbinary", "float", "bit" };
        // Local DST gap (Mar 29, 2026) and overlap (Oct 25, 2026) in Europe/Copenhagen.
        // DateTimeOffset with +14 and -12 tests extreme legal SQL offsets.
        // SqlDecimal values contain at most 38 significant digits.
        object?[][] rows = {
            new object?[] { int.MinValue, long.MinValue,
                SqlDecimal.Parse("9999999999999999999999999999.1234567890"),
                SqlDecimal.Parse("0.00000000000000000000000000000000000001"),
                new DateTime(2026, 3, 29, 2, 30, 0).AddTicks(1234567),
                new DateTimeOffset(2026, 3, 29, 3, 30, 0, TimeSpan.FromHours(2)).AddTicks(1234567),
                new DateTime(1900, 1, 1), TimeSpan.FromTicks(863999999999),
                "æøå 漢字 😀 cafe\u0301\r\n\"citat\", tab\t", "NA",
                new byte[] { 0, 1, 34, 44, 10, 13, 255 }, null,
                1.2345678901234567, true },
            new object?[] { int.MaxValue, long.MaxValue,
                SqlDecimal.Parse("-9999999999999999999999999999.1234567890"),
                SqlDecimal.Parse("-0.00000000000000000000000000000000000001"),
                new DateTime(2026, 10, 25, 2, 30, 0).AddTicks(7654321),
                new DateTimeOffset(2026, 10, 25, 2, 30, 0, TimeSpan.FromHours(1)).AddTicks(7654321),
                new DateTime(2100, 12, 31), TimeSpan.Zero,
                "", "", Array.Empty<byte>(), Array.Empty<byte>(),
                -1.2345678901234567, false },
            new object?[] { 0, 9007199254740993L,
                SqlDecimal.Parse("0.0000000000"), SqlDecimal.Parse("0.0000000000"),
                new DateTime(2000, 1, 1, 0, 0, 0),
                new DateTimeOffset(2000, 1, 1, 12, 0, 0, TimeSpan.FromHours(14)),
                new DateTime(2000, 1, 1), TimeSpan.FromTicks(1),
                "literal NULL, NA, comma, \"quoted\"", null, new byte[] { 0, 255 }, null,
                0.0, false },
            new object?[] { null, null, null, null, null, null, null, null,
                null, null, null, null, null, null },
            new object?[] { -42, -9007199254740993L,
                SqlDecimal.Parse("123.45678901234567890123456789012345678"),
                SqlDecimal.Parse("0.00000000000000000000000000000000000001"),
                new DateTime(2026, 10, 25, 1, 59, 59).AddTicks(1),
                new DateTimeOffset(2000, 1, 1, 12, 0, 0, TimeSpan.FromHours(-12)),
                new DateTime(2026, 10, 18), TimeSpan.FromTicks(1),
                "emoji 🌈 and NUL in binary", "NULL", new byte[] { 0, 0, 255 },
                Array.Empty<byte>(), 1e-300, true }
        };
        if (ColumnarRequested(root))
        {
            int batchRows = ColumnarBatchRows(root);
            byte[] kinds = ColumnarKinds(root, types);
            Reply(new { ok = true, kind = "start", columns, types, wireKinds = KindLabels(kinds),
                        valueEncoding = "columnar-v2", batchRows });
            EmitColumnar(kinds, types, rows, batchRows, out long r, out long n, out long bytes);
            Reply(new { ok = true, kind = "end", rows = r, executeSeconds = 0.0,
                        fetchAndStreamSeconds = sw.Elapsed.TotalSeconds,
                        chunksSent = n, bytesSent = bytes });
            return;
        }
        Reply(new { ok = true, kind = "start", columns, types,
                    valueEncoding = "tagged-v1", chunkSizeBytes });
        var stream = new ChunkedOutputStream(chunkSizeBytes);
        using (stream)
        using (var writer = new StreamWriter(stream, new UTF8Encoding(false), 16 * 1024,
                                              leaveOpen: false) { NewLine = "\n" })
        {
            writer.WriteLine(string.Join(",", Array.ConvertAll(columns, c => CsvHeader(c))));
            foreach (object?[] row in rows)
            {
                for (int j = 0; j < row.Length; j++)
                {
                    if (j > 0) writer.Write(',');
                    writer.Write(CsvValue(row[j], types[j]));
                }
                writer.WriteLine();
            }
        }
        Reply(new { ok = true, kind = "end", rows = rows.Length,
                    executeSeconds = 0.0, fetchAndStreamSeconds = sw.Elapsed.TotalSeconds,
                    chunksSent = stream.ChunksSent, bytesSent = stream.BytesSent });
    }

    // Synthetic errors, with a deliberately partial stream.  R must discard
    // the helper process after either failure, not reuse queued bytes.
    private static void FaultTest(JsonElement root)
    {
        string mode = GetString(root, "mode", "error");
        if (mode == "error")
            throw new InvalidOperationException("Synthetic helper failure for connection-state regression");
        if (mode == "partial")
        {
            int chunkSizeKiB = GetInt(root, "chunkSizeKiB", 256);
            Reply(new { ok = true, kind = "start", columns = new [] { "id" },
                        types = new [] { "int" }, valueEncoding = "tagged-v1",
                        chunkSizeBytes = chunkSizeKiB * 1024 });
            byte[] payload = Encoding.UTF8.GetBytes("\"id\"\n\"V1\"\n");
            WriteFrame((byte)'D', payload);
            throw new InvalidOperationException("Synthetic partial-stream failure");
        }
        throw new ArgumentException("Unknown fault-test mode");
    }

    // Explicit, connection-scoped transactions; never transparently retry
    // after a communication error because COMMIT outcome may be unknown.
    private static void TransactionCommand(JsonElement root, string op)
    {
        RequireOpen();
        if (op == "begin")
        {
            if (transaction != null)
                throw new InvalidOperationException("A SQL transaction is already active.");
            string name = GetString(root, "isolation", "ReadCommitted");
            if (!Enum.TryParse<IsolationLevel>(name, ignoreCase: true, out var level) ||
                level == IsolationLevel.Unspecified || level == IsolationLevel.Chaos)
                throw new ArgumentException("Unsupported SQL transaction isolation level.");
            transaction = connection!.BeginTransaction(level);
            Reply(new { ok = true, state = "active", isolation = level.ToString() });
            return;
        }
        if (transaction == null)
            throw new InvalidOperationException("No SQL transaction is active.");
        var current = transaction;
        try
        {
            if (op == "commit") current.Commit();
            else if (op == "rollback") current.Rollback();
            else throw new ArgumentException("Unknown transaction operation.");
            Reply(new { ok = true, state = op == "commit" ? "committed" : "rolled_back" });
        }
        finally
        {
            transaction = null;
            current.Dispose();
        }
    }


    // DBI table uploads: SBIN binary columns stream through IDataReader.
    // The JSON control header contains schema only; no table rows.
    // SqlBulkCopy deliberately shares exactly the active SqlConnection.
    private sealed class BulkUpload : IDisposable
    {
        public SqlBulkCopy Copier;
        public SqlTransaction? OwnedTransaction;
        public string[] Names;
        public string[] Kinds;
        public string[] SqlTypes;
        public long Rows;
        public BulkUpload(SqlBulkCopy copier, SqlTransaction? owned, string[] names,
                          string[] kinds, string[] sqlTypes)
        { Copier=copier; OwnedTransaction=owned; Names=names; Kinds=kinds;
          SqlTypes=sqlTypes; Rows=0; }
        public void Dispose() { ((IDisposable)Copier).Dispose(); OwnedTransaction?.Dispose(); }
    }
    private static BulkUpload? bulk;
    private static string[] JsonStrings(JsonElement x, string field)
    {
        if (!x.TryGetProperty(field, out var prop) || prop.ValueKind != JsonValueKind.Array)
            throw new ArgumentException("Expected array: " + field);
        return prop.EnumerateArray().Select(e => e.GetString() ??
                throw new ArgumentException("Missing item in " + field)).ToArray();
    }
    private static void BulkAbortSilently()
    {
        var b=bulk; bulk=null;
        if (b==null) return;
        try { b.OwnedTransaction?.Rollback(); } catch { }
        b.Dispose();
    }
    private static void BulkBegin(JsonElement root)
    {
        RequireOpen();
        if (bulk!=null) throw new InvalidOperationException("Another bulk operation is active.");
        string destination=GetString(root,"destination");
        if (string.IsNullOrWhiteSpace(destination)) throw new ArgumentException("Missing destination.");
        var names=JsonStrings(root,"columns");
        var kinds=JsonStrings(root,"kinds");
        var types=JsonStrings(root,"sqlTypes");
        if (names.Length==0 || names.Length>4096 || names.Length!=kinds.Length ||
            kinds.Length!=types.Length || names.Distinct(StringComparer.OrdinalIgnoreCase).Count()!=names.Length)
            throw new ArgumentException("Invalid bulk column schema.");
        int batchSize=GetInt(root,"batchRows",4096);
        // SQL columnar *reads* remain capped at 32768; SBIN writes are
        // independently bounded by a 128-MiB payload and 131072 rows.
        if (batchSize<1 || batchSize>131072) throw new ArgumentException("Invalid bulk batch size.");
        SqlTransaction? owned=null;
        try
        {
            if (transaction==null) owned=connection!.BeginTransaction();
            var active=transaction??owned;
            string dropSql=GetString(root,"dropSql");
            string createSql=GetString(root,"createSql");
            foreach (string sql in new[] {dropSql,createSql})
            {
                if (!string.IsNullOrWhiteSpace(sql))
                {
                    using var cmd=connection!.CreateCommand();
                    cmd.Transaction=active;
                    cmd.CommandTimeout=GetInt(root,"timeout",600);
                    cmd.CommandText=sql;
                    cmd.ExecuteNonQuery();
                }
            }
            // Schema lookup uses the same transaction and session, including #temp.
            // In particular, preserve 38-digit DECIMAL on append even when R
            // provided a character column rather than an explicit field.types.
            int[] destinationOrdinals=new int[names.Length];
            using (var schemaCmd=connection!.CreateCommand())
            {
                schemaCmd.Transaction=active;
                schemaCmd.CommandTimeout=GetInt(root,"timeout",600);
                schemaCmd.CommandText="SELECT TOP (0) * FROM " + destination;
                using var schema=schemaCmd.ExecuteReader(CommandBehavior.SchemaOnly);
                var dbTypes=new System.Collections.Generic.Dictionary<string,(string Type,int Ordinal)>(StringComparer.OrdinalIgnoreCase);
                for(int i=0;i<schema.FieldCount;i++)
                    dbTypes.Add(schema.GetName(i), (schema.GetDataTypeName(i).ToLowerInvariant(),i));
                for(int i=0;i<names.Length;i++)
                {
                    if (!dbTypes.TryGetValue(names[i],out var column))
                        throw new ArgumentException("Destination lacks upload column: " + names[i]);
                    destinationOrdinals[i]=column.Ordinal;
                    // MONEY and SMALLMONEY must be materialised as CLR
                    // decimal, not SqlDecimal. SQL DECIMAL/NUMERIC still
                    // use SqlDecimal to support the full 38-digit precision.
                    // Source values are SBIN decimal (6), or text (5) when
                    // appending preformatted character vectors.
                    if ((kinds[i]=="decimal" || kinds[i]=="text" || kinds[i]=="double") &&
                        (column.Type=="money" || column.Type=="smallmoney"))
                        kinds[i]="money";
                    else if ((kinds[i]=="text" || kinds[i]=="double") &&
                             (column.Type=="decimal" || column.Type=="numeric"))
                        kinds[i]="decimal";
                    // SqlBulkCopy does not reliably coerce a string source
                    // into UNIQUEIDENTIFIER or DATETIMEOFFSET. The SBIN
                    // source remains UTF-8 text; the IDataReader must
                    // expose Guid / DateTimeOffset CLR values instead.
                    else if (kinds[i]=="text" && column.Type=="uniqueidentifier")
                        kinds[i]="guid";
                    else if (kinds[i]=="text" && column.Type=="datetimeoffset")
                        kinds[i]="datetimeoffset_text";
                }
            }
            bool tableLock = GetBool(root, "tableLock", false);
            SqlBulkCopyOptions copyOptions = SqlBulkCopyOptions.KeepNulls;
            if (tableLock) copyOptions |= SqlBulkCopyOptions.TableLock;
            var copier=new SqlBulkCopy(connection!,copyOptions,active)
            {
                DestinationTableName=destination,
                BatchSize=batchSize,
                BulkCopyTimeout=GetInt(root,"timeout",600),
                EnableStreaming=false
            };
            // Map validated source ordinals to destination ordinals; this
            // supports reordered or subset columns without DataTable copies.
            for(int i=0;i<names.Length;i++) copier.ColumnMappings.Add(i,destinationOrdinals[i]);
            bulk=new BulkUpload(copier,owned,names,kinds,types);
            Reply(new {ok=true,state="bulk_ready"});
        }
        catch { try { owned?.Rollback(); } catch { } owned?.Dispose(); throw; }
    }
    // Raw input body uses the same stdin stream as the JSON control lines.
    // Invalid/cancelled batches abort the upload transaction and invalidate
    // the request.
    private static void BulkBinaryBatch(JsonElement root)
    {
        var b = bulk ?? throw new InvalidOperationException("No active bulk operation.");
        int length = GetInt(root, "bytes", -1);
        int n = GetInt(root, "rows", -1);
        if (length < 16 || length > 128 * 1024 * 1024 || n < 0 || n > 131072)
            throw new ArgumentException("Invalid binary bulk batch size.");
        bool allocationProfile = GetBool(root, "allocationProfile", false);
        long allocatedBeforeReceive = allocationProfile ? GC.GetAllocatedBytesForCurrentThread() : 0;
        long totalBeforeReceive = allocationProfile ? GC.GetTotalAllocatedBytes(false) : 0;
        int[] gcBefore = allocationProfile ? new[] { GC.CollectionCount(0), GC.CollectionCount(1), GC.CollectionCount(2) } : Array.Empty<int>();
        var receiveWatch = Stopwatch.StartNew();
        byte[] buffer = new byte[length];
        RawStdin.ReadExactly(buffer.AsSpan(0, length));
        double receiveMs = receiveWatch.Elapsed.TotalMilliseconds;
        long allocatedAfterReceive = allocationProfile ? GC.GetAllocatedBytesForCurrentThread() : 0;
        var stopwatch = Stopwatch.StartNew();
        using var reader = new BulkBinaryReader(buffer, b.Names, b.Kinds,
                                                allocationProfile, length);
        if (reader.RowCount != n)
            throw new ArgumentException("Binary bulk batch row count mismatch.");
        double prepareMs = stopwatch.Elapsed.TotalMilliseconds;
        long allocatedAfterPrepare = allocationProfile ? GC.GetAllocatedBytesForCurrentThread() : 0;
        double copyMs = 0;
        if (n > 0)
        {
            var copyWatch = Stopwatch.StartNew();
            b.Copier.WriteToServer((IDataReader)reader);
            copyMs = copyWatch.Elapsed.TotalMilliseconds;
        }
        long allocatedAfterCopy = allocationProfile ? GC.GetAllocatedBytesForCurrentThread() : 0;
        long totalAfterCopy = allocationProfile ? GC.GetTotalAllocatedBytes(false) : 0;
        b.Rows += n;
        Reply(new {ok=true, rows=n, totalRows=b.Rows, receiveMs, prepareMs, copyMs,
                   allocations = allocationProfile ? new {
                       receiveThreadBytes = allocatedAfterReceive - allocatedBeforeReceive,
                       prepareThreadBytes = allocatedAfterPrepare - allocatedAfterReceive,
                       sourceThreadBytes = 0L,
                       copyThreadBytes = allocatedAfterCopy - allocatedAfterPrepare,
                       processBytes = totalAfterCopy - totalBeforeReceive,
                       gen0 = GC.CollectionCount(0) - gcBefore[0],
                       gen1 = GC.CollectionCount(1) - gcBefore[1],
                       gen2 = GC.CollectionCount(2) - gcBefore[2],
                       readerCounters = reader.AllocationCounters
                   } : null});
    }

    private static void BulkFinish()
    {
        var b=bulk??throw new InvalidOperationException("No active bulk upload.");
        // A failed COMMIT has indeterminate outcome: never retry automatically.
        if (b.OwnedTransaction!=null) b.OwnedTransaction.Commit();
        long rows=b.Rows;
        bulk=null;
        b.Dispose();
        Reply(new {ok=true,rows});
    }
    private static void BulkAbort()
    {
        BulkAbortSilently();
        Reply(new {ok=true,state="bulk_aborted"});
    }

    private static void Execute(JsonElement root)
    {
        using var cmd = CreateCommand(root);
        int affected = cmd.ExecuteNonQuery();
        Reply(new { ok = true, affected });
    }

    public static void Main(string[] args)
    {
        // Diagnostic only. Windows SqlClient normally uses native SNI. This
        // official switch isolates native-networking failures when enabled
        // before the first SqlConnection is created. Managed networking may
        // not support certain local/non-domain Windows authentication modes.
        var managedSni = Environment.GetEnvironmentVariable("RSQLCLIENTNET_MANAGED_SNI");
        if (managedSni == "1" || string.Equals(managedSni, "true", StringComparison.OrdinalIgnoreCase))
            AppContext.SetSwitch("Switch.Microsoft.Data.SqlClient.UseManagedNetworkingOnWindows", true);
        Console.InputEncoding = Encoding.UTF8;
        Console.OutputEncoding = new UTF8Encoding(false);

        if (args.Length == 1 && args[0] == "--bulk-binary-self-test")
        {
            // Standalone decoder check, with no connection to SQL Server.
            using var bytes = new MemoryStream();
            using (var writer = new BinaryWriter(bytes, Encoding.UTF8, leaveOpen: true))
            {
                writer.Write(Encoding.ASCII.GetBytes("SBIN"));
                writer.Write(1); writer.Write(3); writer.Write(2);
                writer.Write((byte)1); writer.Write((byte)2); // int32, row 2 NULL
                writer.Write(42); writer.Write(0); writer.Write(-7);
                writer.Write((byte)5); writer.Write((byte)4); // UTF8, row 3 NULL
                var label = Encoding.UTF8.GetBytes("æøå 日本語");
                writer.Write(label.Length); writer.Write(label);
                writer.Write(0); writer.Write(0);
            }
            using var reader = new BulkBinaryReader(bytes.ToArray(),
                new[] {"id", "label"}, new[] {"integer", "text"});
            if (reader.RowCount != 3 || !reader.Read() ||
                (int)reader.GetValue(0) != 42 || (string)reader.GetValue(1) != "æøå 日本語" ||
                !reader.Read() || !reader.IsDBNull(0) || (string)reader.GetValue(1) != "" ||
                !reader.Read() || (int)reader.GetValue(0) != -7 || !reader.IsDBNull(1) ||
                reader.Read())
                throw new InvalidOperationException("SBIN binary bulk decoder self-test failed.");
            // SBIN v1 native POSIXct extension: code 12 is packed UTC
            // datetime and code 13 is packed UTC datetimeoffset. The
            // legacy UTF-8 variants (8 and 9) remain valid as well.
            using var temporalBytes = new MemoryStream();
            using (var temporalWriter = new BinaryWriter(temporalBytes, Encoding.UTF8, leaveOpen: true))
            {
                temporalWriter.Write(Encoding.ASCII.GetBytes("SBIN"));
                temporalWriter.Write(1); temporalWriter.Write(3); temporalWriter.Write(2);
                temporalWriter.Write((byte)12); temporalWriter.Write((byte)2);
                temporalWriter.Write(-0.25d); temporalWriter.Write(0d); temporalWriter.Write(1700000000.125d);
                temporalWriter.Write((byte)13); temporalWriter.Write((byte)2);
                temporalWriter.Write(-0.25d); temporalWriter.Write(0d); temporalWriter.Write(1700000000.125d);
            }
            using var tReader = new BulkBinaryReader(temporalBytes.ToArray(),
                new[] {"time", "offset"}, new[] {"datetime", "datetimeoffset"});
            if (!tReader.Read() ||
                (DateTime)tReader.GetValue(0) != new DateTime(1969, 12, 31, 23, 59, 59, 750) ||
                (DateTimeOffset)tReader.GetValue(1) != new DateTimeOffset(
                    1969, 12, 31, 23, 59, 59, 750, TimeSpan.Zero) ||
                !tReader.Read() || !tReader.IsDBNull(0) || !tReader.IsDBNull(1) ||
                !tReader.Read() ||
                ((DateTimeOffset)tReader.GetValue(1)).Offset != TimeSpan.Zero ||
                reader.Read() || tReader.Read())
                throw new InvalidOperationException("SBIN native POSIXct decoder self-test failed.");
            // MONEY/SMALLMONEY scale-4 regression. R's binary-double text
            // representation may be slightly outside the nominal SQL limit;
            // an exact CLR decimal of scale 4 must be returned to SqlBulkCopy.
            using var moneyBytes = new MemoryStream();
            using (var moneyWriter = new BinaryWriter(moneyBytes, Encoding.UTF8, leaveOpen: true))
            {
                moneyWriter.Write(Encoding.ASCII.GetBytes("SBIN"));
                moneyWriter.Write(1); moneyWriter.Write(4); moneyWriter.Write(1);
                moneyWriter.Write((byte)6); moneyWriter.Write((byte)4); // decimal; third row NULL
                foreach (var sample in new[] {
                    "214748.36470000001", "-214748.36480000001", "", "10.999950000001" })
                {
                    var raw = Encoding.UTF8.GetBytes(sample);
                    moneyWriter.Write(raw.Length); moneyWriter.Write(raw);
                }
            }
            using var moneyReader = new BulkBinaryReader(moneyBytes.ToArray(),
                new[] {"amount"}, new[] {"money"});
            if (moneyReader.GetFieldType(0) != typeof(decimal) ||
                !moneyReader.Read() || (decimal)moneyReader.GetValue(0) != 214748.3647m ||
                !moneyReader.Read() || (decimal)moneyReader.GetValue(0) != -214748.3648m ||
                !moneyReader.Read() || !moneyReader.IsDBNull(0) ||
                moneyReader.GetValue(0) != DBNull.Value ||
                !moneyReader.Read() || (decimal)moneyReader.GetValue(0) != 11.0000m ||
                moneyReader.Read())
                throw new InvalidOperationException("SBIN MONEY scale-4 decoder self-test failed.");
            // Native DATE/TIME/DECIMAL and MONEY: fixed 8-byte double values
            // with exactly the same NULL bitmap as all other SBIN columns.
            using var nativeBytes = new MemoryStream();
            using (var nativeWriter = new BinaryWriter(nativeBytes, Encoding.UTF8, leaveOpen: true))
            {
                nativeWriter.Write(Encoding.ASCII.GetBytes("SBIN"));
                nativeWriter.Write(1); nativeWriter.Write(3); nativeWriter.Write(4);
                nativeWriter.Write((byte)17); nativeWriter.Write((byte)2);
                nativeWriter.Write(0d); nativeWriter.Write(0d); nativeWriter.Write(2932896d);
                nativeWriter.Write((byte)18); nativeWriter.Write((byte)2);
                nativeWriter.Write(0.1234567d); nativeWriter.Write(0d); nativeWriter.Write(86399.9999999d);
                nativeWriter.Write((byte)19); nativeWriter.Write((byte)2);
                nativeWriter.Write(123.4567d); nativeWriter.Write(0d); nativeWriter.Write(-9.5d);
                nativeWriter.Write((byte)19); nativeWriter.Write((byte)2);
                nativeWriter.Write(214748.3647d); nativeWriter.Write(0d); nativeWriter.Write(-214748.3648d);
            }
            using var nativeReader = new BulkBinaryReader(nativeBytes.ToArray(),
                new[] { "date", "clock", "amount", "small" },
                new[] { "date", "time", "decimal", "money" });
            if (nativeReader.GetFieldType(0) != typeof(DateTime) ||
                nativeReader.GetFieldType(1) != typeof(TimeSpan) ||
                nativeReader.GetFieldType(2) != typeof(SqlDecimal) ||
                nativeReader.GetFieldType(3) != typeof(decimal) ||
                !nativeReader.Read() ||
                (DateTime)nativeReader.GetValue(0) != new DateTime(1970, 1, 1) ||
                (TimeSpan)nativeReader.GetValue(1) != new TimeSpan(1234567L) ||
                Math.Abs(((SqlDecimal)nativeReader.GetValue(2)).ToDouble() - 123.4567d) > 1e-10 ||
                (decimal)nativeReader.GetValue(3) != 214748.3647m ||
                !nativeReader.Read() || !nativeReader.IsDBNull(0) ||
                !nativeReader.IsDBNull(1) || !nativeReader.IsDBNull(2) ||
                !nativeReader.IsDBNull(3) ||
                !nativeReader.Read() ||
                (DateTime)nativeReader.GetValue(0) != new DateTime(9999, 12, 31) ||
                (TimeSpan)nativeReader.GetValue(1) != new TimeSpan(TimeSpan.TicksPerDay - 1) ||
                (decimal)nativeReader.GetValue(3) != -214748.3648m ||
                nativeReader.Read())
                throw new InvalidOperationException("SBIN native DATE/TIME/DECIMAL/MONEY decoder self-test failed.");
            Console.Out.WriteLine(JsonSerializer.Serialize(new { ok = true, test = "bulk-binary-decoder", rows = 3 }, JsonOpts));
            return;
        }

        // CLI diagnostic: `dotnet SqlClientBridge.dll --self-test`
        // Prints one JSON PONG and exits without touching SQL Server.
        if (args.Length == 1 && args[0] == "--self-test")
        {
            Console.Out.WriteLine(JsonSerializer.Serialize(new { ok = true, reply = "pong", transport = "binary" }, JsonOpts));
            return;
        }

        string? line;
        while ((line = ReadControlLine()) != null)
        {
            CurrentResponseStarted = false;
            string op = "";
            try
            {
                using var document = JsonDocument.Parse(line);
                var root = document.RootElement;
                op = GetString(root, "op");
                if (bulk != null && op != "bulk_finish" &&
                    op != "bulk_abort" && op != "bulk_binary_batch" && op != "disconnect")
                    throw new InvalidOperationException("Finish or abort active bulk upload first.");
                if (cursor != null && op != "cursor_fetch" && op != "cursor_close" &&
                    op != "disconnect")
                    throw new InvalidOperationException("An active DBI result must be cleared first.");
                switch (op)
                {
                    case "connect": Connect(root); break;
                    case "execute": Execute(root); break;
                    case "begin": TransactionCommand(root, "begin"); break;
                    case "commit": TransactionCommand(root, "commit"); break;
                    case "rollback": TransactionCommand(root, "rollback"); break;
                    case "query": Query(root); break;
                    case "bulk_begin": BulkBegin(root); break;
                    case "bulk_binary_batch": BulkBinaryBatch(root); break;
                    case "bulk_finish": BulkFinish(); break;
                    case "bulk_abort": BulkAbort(); break;
                    case "cursor_open": CursorOpen(root); break;
                    case "cursor_fetch": CursorFetch(root); break;
                    case "cursor_close": CursorClose(root); break;
                    case "stream_stall_test": StreamStallTest(root); break;
                    case "columnar_fault_test": ColumnarFaultTest(root); break;
                    case "memory_test": MemoryTest(root); break;
                    case "types_test": TypesTest(root); break;
                    case "edge_types_test": EdgeTypesTest(root); break;
                    case "fault_test": FaultTest(root); break;
                    case "ping": Ping(GetString(root, "echo")); break;
                    // Synthetic, bounded stall for cancellation testing only; no SQL access.
                    case "sleep_test":
                        int delayMs = Math.Clamp(GetInt(root, "milliseconds", 2000), 0, 30000);
                        System.Threading.Thread.Sleep(delayMs);
                        Reply(new { ok = true, sleptMs = delayMs });
                        break;
                    case "disconnect":
                        CloseCursor();
                        BulkAbortSilently();
                        if (transaction != null)
                        {
                            try { transaction.Rollback(); }
                            finally { transaction.Dispose(); transaction = null; }
                        }
                        connection?.Dispose();
                        connection = null;
                        Reply(new { ok = true, state = "closed" });
                        return;
                    default: throw new ArgumentException("Unknown operation: " + op);
                }
            }
            catch (Exception e)
            {
                // Only an SQL execution/compilation failure with a wholly
                // intact SQL session AND no stream frames sent is recoverable.
                // Do not mark bulk/COMMIT errors, protocol failures, or a
                // partially emitted query as recoverable. C# disposes failed
                // SqlDataReaders in CursorOpen before this catch executes.
                bool recoverableSqlError =
                    e is SqlException se && se.Class < 20 &&
                    connection?.State == ConnectionState.Open &&
                    !CurrentResponseStarted && cursor == null && bulk == null &&
                    (op == "query" || op == "cursor_open" || op == "execute");
                if (bulk != null) BulkAbortSilently();
                if (e is SqlException sqlError)
                    Reply(new { ok = false, error = sqlError.Message, exception = "SqlException",
                                sqlNumber = sqlError.Number, sqlState = sqlError.State,
                                severity = sqlError.Class, recoverable = recoverableSqlError });
                else
                    Reply(new { ok = false, error = e.Message, exception = e.GetType().Name,
                                recoverable = false });
            }
        }
        CloseCursor();
        BulkAbortSilently();
        transaction?.Dispose();
        transaction = null;
        connection?.Dispose();
    }
}
