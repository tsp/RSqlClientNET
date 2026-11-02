// RSqlClientNET SBIN v1: a column-oriented, length-checked in-memory IDataReader.
// SqlBulkCopy reads directly from this reader; no intermediate DataTable rows.
using System;
using System.Buffers.Binary;
using System.Data;
using System.Data.SqlTypes;
using System.IO;
using System.Globalization;
using System.Text;

internal sealed class BulkBinaryReader : IDataReader
{
    private readonly byte[] payload;
    private readonly int payloadLength;
    private readonly string[] names;
    private readonly string[] kinds;
    private readonly Column[] columns;
    private readonly int rows;
    private int row = -1;
    private bool closed;
    private readonly bool traceAllocations;
    private long valueCalls, charsCalls, bytesCalls, textObjects, textUtf16Bytes;
    private long blobObjects, blobBytes, streamBinaryBytes;
    public object? AllocationCounters => traceAllocations ? new {
        getValueCalls = valueCalls, getCharsCalls = charsCalls, getBytesCalls = bytesCalls,
        textObjects, textUtf16Bytes, blobObjects, blobBytes,
        streamBinaryBytes
    } : null;
    private static readonly UTF8Encoding StrictUtf8 = new(false, true);

    private sealed class Column
    {
        public int Kind;
        public int Bitmap;
        public int Data;
        public int Width;
        public int[]? Offsets;
        public int[]? Lengths;
    }

    private static int Code(string kind) => kind switch
    {
        "integer" => 1, "integer64" => 2, "double" => 3, "logical" => 4,
        "text" => 5, "decimal" => 6, "date" => 7, "datetime" => 8,
        "datetimeoffset" => 9, "time" => 10, "binary" => 11,
        // An internal destination-only kind: MONEY/SMALLMONEY must reach
        // SqlBulkCopy as a CLR decimal, not as a 38-digit SqlDecimal.
        // The R SBIN v1 encoder continues to send code 6 (or code 5 for
        // text); no change to the binary wire format is needed.
        "money" => 14,
        "guid" => 15, "datetimeoffset_text" => 16,
        "date_native" => 17, "time_native" => 18, "decimal_native" => 19,
        _ => throw new ArgumentException("Unsupported binary bulk column kind.")
    };
    private static Type ClrType(int code) => code switch
    {
        1 => typeof(int), 2 => typeof(long), 3 => typeof(double), 4 => typeof(bool),
        5 => typeof(string), 6 => typeof(SqlDecimal), 7 or 8 => typeof(DateTime),
        9 or 13 => typeof(DateTimeOffset), 10 => typeof(TimeSpan), 11 => typeof(byte[]),
        14 => typeof(decimal), 15 => typeof(Guid),
        16 => typeof(DateTimeOffset),
        12 or 17 => typeof(DateTime),
        18 => typeof(TimeSpan), 19 => typeof(SqlDecimal), 20 => typeof(decimal),
        _ => throw new ArgumentException("Invalid bulk column code.")
    };

    public BulkBinaryReader(byte[] buffer, string[] names, string[] kinds,
                            bool traceAllocations = false, int logicalLength = -1)
    {
        this.traceAllocations = traceAllocations;
        payload = buffer;
        payloadLength = logicalLength < 0 ? buffer.Length : logicalLength;
        if (payloadLength < 0 || payloadLength > buffer.Length)
            throw new ArgumentOutOfRangeException(nameof(logicalLength));
        this.names = names;
        this.kinds = kinds;
        if (names.Length == 0 || names.Length != kinds.Length || names.Length > 4096)
            throw new ArgumentException("Invalid binary batch schema.");
        int p = 0;
        if (payloadLength < 16 || buffer[0] != 'S' || buffer[1] != 'B' ||
            buffer[2] != 'I' || buffer[3] != 'N')
            throw new InvalidDataException("Bad SBIN bulk header.");
        p = 4;
        if (Read32(ref p) != 1) throw new InvalidDataException("Unsupported SBIN version.");
        rows = checked((int)Read32(ref p));
        int count = checked((int)Read32(ref p));
        if (rows < 0 || rows > 131072 || count != names.Length)
            throw new InvalidDataException("Invalid SBIN shape.");
        columns = new Column[count];
        for (int c = 0; c < count; c++)
        {
            Ensure(p, 1);
            int code = buffer[p++];
            int destinationCode = Code(kinds[c]);
            // Destination SQL type determines the CLR value returned by
            // IDataReader. SQL DECIMAL/NUMERIC use SqlDecimal for precision 38;
            // MONEY/SMALLMONEY instead need System.Decimal. Both arrive in
            // the existing SBIN UTF-8 decimal representation (code 6), or
            // in the text representation (code 5) for append operations.
            if (code != destinationCode && !(code == 5 && destinationCode == 6) &&
                !((code == 5 || code == 6) && destinationCode == 14) &&
                !(code == 5 && (destinationCode == 15 || destinationCode == 16)) &&
                !(code == 12 && destinationCode == 8) &&
                !(code == 13 && destinationCode == 9) &&
                !(code == 17 && destinationCode == 7) &&
                !(code == 18 && destinationCode == 10) &&
                !(code == 19 && (destinationCode == 6 || destinationCode == 14)) &&
                !(code == 3 && (destinationCode == 6 || destinationCode == 14)))
                throw new InvalidDataException("SBIN column type mismatch.");
            int bitmapLength = (rows + 7) / 8;
            Ensure(p, bitmapLength);
            // Reinterpret only the value conversion, not the SBIN data layout
            // (both money and decimal are length-prefixed UTF-8 strings).
            int effectiveCode = (code == 19 || code == 3) && destinationCode == 14 ? 20 :
                code == 3 && destinationCode == 6 ? 19 :
                destinationCode is 14 or 15 or 16 ? destinationCode :
                (code == 5 && destinationCode == 6 ? 6 : code);
            var col = new Column { Kind = effectiveCode, Bitmap = p };
            p += bitmapLength;
            col.Data = p;
            col.Width = code switch { 1 => 4, 2 or 3 or 12 or 13 or 17 or 18 or 19 => 8,
                                      4 => 1, _ => 0 };
            if (col.Width != 0)
            {
                Ensure(p, checked(rows * col.Width));
                p += rows * col.Width;
            }
            else
            {
                col.Offsets = new int[rows];
                col.Lengths = new int[rows];
                for (int r = 0; r < rows; r++)
                {
                    uint len = Read32(ref p);
                    if (len > int.MaxValue) throw new InvalidDataException("Oversized SBIN field.");
                    Ensure(p, (int)len);
                    col.Offsets[r] = p;
                    col.Lengths[r] = (int)len;
                    p += (int)len;
                }
            }
            columns[c] = col;
        }
        if (p != payloadLength) throw new InvalidDataException("Trailing SBIN data.");
    }
    private void Ensure(int pos, int length)
    {
        if (pos < 0 || length < 0 || pos > payloadLength || length > payloadLength - pos)
            throw new InvalidDataException("Truncated SBIN bulk data.");
    }
    private uint Read32(ref int pos)
    {
        Ensure(pos, 4);
        uint result = BinaryPrimitives.ReadUInt32LittleEndian(payload.AsSpan(pos, 4));
        pos += 4;
        return result;
    }
    private void CheckColumn(int i)
    {
        if (closed || row < 0 || row >= rows) throw new InvalidOperationException("No active bulk row.");
        if (i < 0 || i >= columns.Length) throw new IndexOutOfRangeException();
    }
    public bool Read()
    {
        if (closed) return false;
        if (row >= rows) return false;
        ++row;
        return row < rows;
    }
    public bool NextResult() => false;
    public void Close() { closed = true; }
    public void Dispose() => Close();
    public bool IsClosed => closed;
    public int RecordsAffected => -1;
    public int Depth => 0;
    public int FieldCount => columns.Length;
    public int RowCount => rows;
    public object this[int i] => GetValue(i);
    public object this[string name] => GetValue(GetOrdinal(name));
    public string GetName(int i) => names[i];
    public string GetDataTypeName(int i) => kinds[i];
    public Type GetFieldType(int i) => ClrType(columns[i].Kind);
    public int GetOrdinal(string name)
    {
        for (int i = 0; i < names.Length; ++i)
            if (string.Equals(names[i], name, StringComparison.OrdinalIgnoreCase)) return i;
        throw new IndexOutOfRangeException("Unknown binary bulk field.");
    }
    public DataTable GetSchemaTable()
    {
        var schema = new DataTable("SchemaTable");
        schema.Columns.Add("ColumnName", typeof(string));
        schema.Columns.Add("ColumnOrdinal", typeof(int));
        schema.Columns.Add("DataType", typeof(Type));
        schema.Columns.Add("AllowDBNull", typeof(bool));
        schema.Columns.Add("ColumnSize", typeof(int));
        for (int i = 0; i < names.Length; ++i)
            schema.Rows.Add(names[i], i, GetFieldType(i), true, -1);
        return schema;
    }
    public bool IsDBNull(int i)
    {
        CheckColumn(i);
        var col = columns[i];
        return (payload[col.Bitmap + row / 8] & (1 << (row % 8))) != 0;
    }
    // Unix-seconds packed timestamps avoid R format() and .NET DateTime.Parse().
    // Split whole seconds before multiplying the fractional remainder to
    // preserve sub-microsecond precision (the complete Unix tick count would
    // otherwise exceed the exact integer range of a double near 2026).
    private static DateTimeOffset UnixSecondsToOffset(double seconds)
    {
        if (double.IsNaN(seconds) || double.IsInfinity(seconds))
            throw new InvalidDataException("Invalid nonfinite native datetime.");
        double whole = Math.Floor(seconds);
        if (whole < -62135596800d || whole > 253402300799d)
            throw new InvalidDataException("Native datetime exceeds SQL datetime range.");
        long secondsPart = checked((long)whole);
        long ticksPart = checked((long)Math.Round(
            (seconds - whole) * TimeSpan.TicksPerSecond, MidpointRounding.AwayFromZero));
        return DateTimeOffset.FromUnixTimeSeconds(secondsPart).AddTicks(ticksPart);
    }
    // SQL MONEY and SMALLMONEY have a fixed scale of four decimal places.
    // R doubles are formatted with 17 significant digits for lossless transport;
    // a value that *means* SMALLMONEY 214748.3647 can consequently arrive as
    // "214748.36470000001". Passing this unrounded decimal to SqlBulkCopy
    // makes it overflow the upper bound before SQL Server can apply its
    // four-place scale. Round the parsed CLR decimal to the destination scale
    // first. Do not apply this to DECIMAL/NUMERIC, which require SqlDecimal
    // and can represent 38 digits with user-defined scale.
    private static decimal ParseMoneyValue(string value) =>
        decimal.Round(decimal.Parse(value, NumberStyles.Number,
                          CultureInfo.InvariantCulture), 4,
                      MidpointRounding.AwayFromZero);

    private static DateTime NativeSqlDate(double unixDays)
    {
        if (!double.IsFinite(unixDays) || unixDays != Math.Truncate(unixDays) ||
            unixDays < -719162d || unixDays > 2932896d)
            throw new InvalidDataException("Native DATE outside SQL Server calendar range.");
        return new DateTime(1970, 1, 1).AddDays(unixDays);
    }

    private static TimeSpan NativeSqlTime(double seconds)
    {
        if (!double.IsFinite(seconds) || seconds < 0 || seconds >= 86400d)
            throw new InvalidDataException("Native TIME must be within a calendar day.");
        long ticks = checked((long)Math.Round(seconds * TimeSpan.TicksPerSecond,
                              MidpointRounding.AwayFromZero));
        if (ticks < 0 || ticks >= TimeSpan.TicksPerDay)
            throw new InvalidDataException("Native TIME rounded outside calendar day.");
        return new TimeSpan(ticks);
    }

    // R numeric doubles have at most ~17 significant digits; .NET's
    // SqlDecimal(double) accepts this directly without building a character
    // vector in R. Character DECIMAL(38,s) still uses the precise UTF-8 path.
    private static SqlDecimal NativeSqlDecimal(double value)
    {
        if (!double.IsFinite(value)) throw new InvalidDataException("Invalid DECIMAL double.");
        return new SqlDecimal(value);
    }

    private static decimal NativeSqlMoney(double value)
    {
        if (!double.IsFinite(value)) throw new InvalidDataException("Invalid MONEY double.");
        return decimal.Round(Convert.ToDecimal(value), 4,
                             MidpointRounding.AwayFromZero);
    }

    public object GetValue(int i)
    {
        if (traceAllocations) valueCalls++;
        CheckColumn(i);
        if (IsDBNull(i)) return DBNull.Value;
        var col = columns[i];
        int pos = col.Width == 0 ? col.Offsets![row] : col.Data + row * col.Width;
        switch (col.Kind)
        {
            case 1: return BinaryPrimitives.ReadInt32LittleEndian(payload.AsSpan(pos, 4));
            case 2: return BinaryPrimitives.ReadInt64LittleEndian(payload.AsSpan(pos, 8));
            case 3: return BitConverter.Int64BitsToDouble(BinaryPrimitives.ReadInt64LittleEndian(payload.AsSpan(pos, 8)));
            case 4: return payload[pos] != 0;
            case 12:
                return DateTime.SpecifyKind(UnixSecondsToOffset(
                    BitConverter.Int64BitsToDouble(BinaryPrimitives.ReadInt64LittleEndian(
                        payload.AsSpan(pos, 8)))).UtcDateTime, DateTimeKind.Unspecified);
            case 13:
                return UnixSecondsToOffset(BitConverter.Int64BitsToDouble(
                    BinaryPrimitives.ReadInt64LittleEndian(payload.AsSpan(pos, 8))));
            case 17:
                return NativeSqlDate(BitConverter.Int64BitsToDouble(
                    BinaryPrimitives.ReadInt64LittleEndian(payload.AsSpan(pos, 8))));
            case 18:
                return NativeSqlTime(BitConverter.Int64BitsToDouble(
                    BinaryPrimitives.ReadInt64LittleEndian(payload.AsSpan(pos, 8))));
            case 19:
                return NativeSqlDecimal(BitConverter.Int64BitsToDouble(
                    BinaryPrimitives.ReadInt64LittleEndian(payload.AsSpan(pos, 8))));
            case 20:
                return NativeSqlMoney(BitConverter.Int64BitsToDouble(
                    BinaryPrimitives.ReadInt64LittleEndian(payload.AsSpan(pos, 8))));
            case 11:
            {
                int length = col.Lengths![row];
                if (traceAllocations) { blobObjects++; blobBytes += length; }
                return payload.AsSpan(pos, length).ToArray();
            }
            default:
                string s = StrictUtf8.GetString(payload, pos, col.Lengths![row]);
                if (traceAllocations) { textObjects++; textUtf16Bytes += s.Length * 2L; }
                return col.Kind switch
                {
                    5 => s,
                    6 => SqlDecimal.Parse(s),
                    // MONEY/SMALLMONEY need CLR decimal *rounded to four
                    // places* before SqlBulkCopy's destination-range check.
                    // SQL NULLs were already returned via the bitmap above.
                    14 => ParseMoneyValue(s),
                    15 => Guid.Parse(s),
                    16 => DateTimeOffset.Parse(s, CultureInfo.InvariantCulture,
                                                DateTimeStyles.None),
                    7 => DateTime.ParseExact(s, "yyyy-MM-dd", CultureInfo.InvariantCulture),
                    8 => DateTime.Parse(s, CultureInfo.InvariantCulture, DateTimeStyles.None),
                    9 => DateTimeOffset.Parse(s, CultureInfo.InvariantCulture),
                    10 => TimeSpan.Parse(s, CultureInfo.InvariantCulture),
                    _ => throw new InvalidDataException("Unknown SBIN bulk value kind.")
                };
        }
    }
    // IDataRecord.GetData(int) is required even when there are no nested
    // result sets. The SBIN format contains scalar columns only.
    public IDataReader GetData(int i) =>
        throw new NotSupportedException("Nested data readers are not supported by SBIN bulk columns.");

    public int GetValues(object[] values)
    {
        int count = Math.Min(values.Length, FieldCount);
        for(int i = 0; i < count; i++) values[i] = GetValue(i);
        return count;
    }
    public bool GetBoolean(int i) => (bool)GetValue(i);
    public byte GetByte(int i) => Convert.ToByte(GetValue(i), CultureInfo.InvariantCulture);
    public char GetChar(int i) => Convert.ToChar(GetValue(i), CultureInfo.InvariantCulture);
    public short GetInt16(int i) => Convert.ToInt16(GetValue(i), CultureInfo.InvariantCulture);
    public int GetInt32(int i) => Convert.ToInt32(GetValue(i), CultureInfo.InvariantCulture);
    public long GetInt64(int i) => Convert.ToInt64(GetValue(i), CultureInfo.InvariantCulture);
    public float GetFloat(int i) => Convert.ToSingle(GetValue(i), CultureInfo.InvariantCulture);
    public double GetDouble(int i) => Convert.ToDouble(GetValue(i), CultureInfo.InvariantCulture);
    public string GetString(int i) => (string)GetValue(i);
    public decimal GetDecimal(int i)
    {
        object v=GetValue(i);
        return v is SqlDecimal sql ? sql.Value : Convert.ToDecimal(v, CultureInfo.InvariantCulture);
    }
    public DateTime GetDateTime(int i) => (DateTime)GetValue(i);
    public Guid GetGuid(int i) => (Guid)GetValue(i);
    // Experimental DbDataReader data feed: expose a *view* of the existing
    // validated SBIN payload.  MemoryStream(byte[], offset, length, ...) does
    // not copy the LOB.  The bulk upload owns the payload until WriteToServer
    // completes; the returned streams must not outlive the batch.
    public Stream OpenBinaryStream(int i)
    {
        CheckColumn(i);
        var col = columns[i];
        if (col.Kind != 11) throw new InvalidCastException("Not an SBIN binary column.");
        if (IsDBNull(i)) throw new SqlNullValueException();
        int position = col.Offsets![row];
        int length = col.Lengths![row];
        return new MemoryStream(payload, position, length, writable: false, publiclyVisible: false);
    }

    public TextReader OpenTextReader(int i)
    {
        CheckColumn(i);
        var col = columns[i];
        if (col.Kind != 5) throw new InvalidCastException("Not an SBIN text column.");
        if (IsDBNull(i)) throw new SqlNullValueException();
        int position = col.Offsets![row];
        int length = col.Lengths![row];
        // Strict UTF-8 decoding is consistent with the existing GetValue path.
        // StreamReader's small buffers are bounded, unlike a full UTF-16 string.
        var stream = new MemoryStream(payload, position, length,
                                      writable: false, publiclyVisible: false);
        return new StreamReader(stream, StrictUtf8,
                                detectEncodingFromByteOrderMarks: false,
                                bufferSize: 1024, leaveOpen: false);
    }

    public long GetBytes(int i, long fieldOffset, byte[]? destination, int bufferOffset, int length)
    {
        if (traceAllocations) bytesCalls++;
        // Read directly from the validated SBIN column offsets. SqlBulkCopy
        // may ask for a binary slice repeatedly when streaming is enabled:
        // do not allocate/copy the whole VARBINARY value on every call.
        CheckColumn(i);
        var col = columns[i];
        if (col.Kind != 11) throw new InvalidCastException("Not an SBIN binary column.");
        if (IsDBNull(i)) throw new SqlNullValueException();
        int valueLength = col.Lengths![row];
        if (fieldOffset < 0 || fieldOffset > valueLength)
            throw new ArgumentOutOfRangeException(nameof(fieldOffset));
        if (destination == null) return valueLength;
        if (bufferOffset < 0 || length < 0 || bufferOffset > destination.Length ||
            length > destination.Length - bufferOffset)
            throw new ArgumentOutOfRangeException(nameof(bufferOffset));
        int n = Math.Min(length, valueLength - (int)fieldOffset);
        Buffer.BlockCopy(payload, col.Offsets![row] + (int)fieldOffset,
                         destination, bufferOffset, n);
        if (traceAllocations) streamBinaryBytes += n;
        return n;
    }
    public long GetChars(int i, long fieldOffset, char[]? destination, int bufferOffset, int length)
    {
        if (traceAllocations) charsCalls++;
        // GetString still decodes UTF-8 but String.CopyTo avoids a second,
        // full-size char[] allocation for each streaming slice. In particular
        // a length-only request no longer creates a throwaway char array.
        string value = GetString(i);
        if (fieldOffset < 0 || fieldOffset > value.Length)
            throw new ArgumentOutOfRangeException(nameof(fieldOffset));
        if (destination == null) return value.Length;
        if (bufferOffset < 0 || length < 0 || bufferOffset > destination.Length ||
            length > destination.Length - bufferOffset)
            throw new ArgumentOutOfRangeException(nameof(bufferOffset));
        int n = Math.Min(length, value.Length - (int)fieldOffset);
        value.CopyTo((int)fieldOffset, destination, bufferOffset, n);
        return n;
    }
}
