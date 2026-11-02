#include <Rcpp.h>
#include <Rinternals.h>
#include <algorithm>
#include <cstdint>
#include <cstring>
#include <limits>
#include <climits>
#include <stdexcept>
#include <string>
#include <vector>

// Decodes one complete SCB2 payload (outer J/D pipe frame already removed).
// Bounds-check every read before allocation; no file system or SQL access.
namespace {
class Reader {
  const unsigned char *p_;
  size_t n_, at_ = 0;
public:
  Reader(SEXP v): p_(RAW(v)), n_(XLENGTH(v)) {}
  size_t remaining() const { return n_ - at_; }
  const unsigned char* take(size_t n) {
    if (n > remaining()) throw std::runtime_error("Truncated SCB2 batch");
    const unsigned char* s = p_ + at_;
    at_ += n;
    return s;
  }
  uint8_t u8() { return *take(1); }
  uint32_t u32() {
    auto p = take(4);
    return uint32_t(p[0]) | (uint32_t(p[1]) << 8) |
           (uint32_t(p[2]) << 16) | (uint32_t(p[3]) << 24);
  }
  uint64_t u64() {
    auto p = take(8);
    uint64_t x=0;
    for (int i=0;i<8;i++) x |= (uint64_t(p[i]) << (8*i));
    return x;
  }
  int32_t i32() { return static_cast<int32_t>(u32()); }
  int64_t i64() { uint64_t b = u64(); int64_t v; std::memcpy(&v, &b, 8); return v; }
  double f64() { uint64_t b = u64(); double v; std::memcpy(&v, &b, 8); return v; }
};
inline bool present(const unsigned char* bitmap, size_t i) {
  return (bitmap[i >> 3] & (1u << (i & 7))) != 0;
}
}

// The second argument controls bit64 output for signed SQL bigint.
// SQL -2^63 is reserved as bit64::NA_integer64_: decode as NA and report it to R.
extern "C" SEXP sqlcpp_decode_batch(SEXP payload, SEXP bigint64, SEXP timezone) {
  BEGIN_RCPP
  if (TYPEOF(payload) != RAWSXP) throw std::runtime_error("SCB2 payload must be raw bytes");
  if (TYPEOF(bigint64) != LGLSXP || XLENGTH(bigint64) != 1 || LOGICAL(bigint64)[0] == NA_LOGICAL)
    throw std::runtime_error("bigint64 argument must be a single logical");
  bool native_bigint = LOGICAL(bigint64)[0] == TRUE;
  std::string tz = Rcpp::as<std::string>(timezone);
  Reader r(payload);
  auto magic = r.take(4);
  if (std::memcmp(magic, "SCB2", 4) != 0)
    throw std::runtime_error("Invalid columnar batch magic (expected SCB2)");
  uint32_t n = r.u32(), ncols = r.u32();
  // Also constrain allocations and avoid corrupt frame attacks.
  if (n > 65536 || ncols > 4096)
    throw std::runtime_error("Invalid SCB2 row/column count");
  Rcpp::List columns(ncols);
  Rcpp::CharacterVector kinds(ncols);
  // Bound warning metadata to at most 20 row numbers per column. The exact
  // total count is separate so very large batches do not inflate memory use.
  Rcpp::List bigint_min_rows(ncols);
  Rcpp::IntegerVector bigint_min_counts(ncols);
  for (uint32_t col = 0; col < ncols; col++) {
    char kind = static_cast<char>(r.u8());
    kinds[col] = std::string(1, kind);
    const unsigned char* valid = r.take((size_t(n) + 7) / 8);
    switch (kind) {
      case 'I': {
        // R's integer NA uses the bit pattern INT32_MIN. If an actual SQL int
        // contains -2147483648, promote this entire column to double; every
        // int32 is exactly representable by an IEEE double.
        std::vector<int32_t> values(n);
        bool has_r_na_sentinel = false;
        for (uint32_t i=0; i<n; i++) {
          values[i] = r.i32();
          if (present(valid, i) && values[i] == std::numeric_limits<int32_t>::min())
            has_r_na_sentinel = true;
        }
        if (has_r_na_sentinel) {
          Rcpp::NumericVector out(n);
          for (uint32_t i=0; i<n; i++)
            out[i] = present(valid,i) ? static_cast<double>(values[i]) : NA_REAL;
          columns[col] = out;
        } else {
          Rcpp::IntegerVector out(n);
          for (uint32_t i=0; i<n; i++)
            out[i] = present(valid,i) ? values[i] : NA_INTEGER;
          columns[col] = out;
        }
        break;
      }
      case 'L': {
        if (native_bigint) {
          Rcpp::NumericVector out(n);
          std::vector<int> min_rows;
          min_rows.reserve(20);
          int min_count = 0;
          for (uint32_t i=0; i<n; i++) {
            int64_t v = r.i64();
            if (!present(valid,i)) v = std::numeric_limits<int64_t>::min();
            else if (v == std::numeric_limits<int64_t>::min()) {
              // Keep SQL NULL and the minimum SQL BIGINT as NA_integer64_,
              // but report only the latter as a lossy conversion.
              ++min_count;
              if (min_rows.size() < 20) min_rows.push_back(static_cast<int>(i+1));
            }
            // bit64 stores raw signed 64-bit two's complement bits in REALSXP.
            std::memcpy(REAL(out) + i, &v, sizeof(v));
          }
          out.attr("class") = Rcpp::CharacterVector::create("integer64");
          columns[col] = out;
          if (min_count) {
            bigint_min_rows[col] = Rcpp::wrap(min_rows);
            bigint_min_counts[col] = min_count;
          }
        } else {
          Rcpp::CharacterVector out(n);
          for (uint32_t i=0; i<n; i++) {
            int64_t v = r.i64();
            if (present(valid, i)) out[i] = std::to_string(v);
            else out[i] = NA_STRING;
          }
          columns[col] = out;
        }
        break;
      }
      case 'N':
      case 'D': {
        Rcpp::NumericVector out(n);
        for (uint32_t i=0; i<n; i++) {
          double v = r.f64();
          out[i] = present(valid, i) ? v : NA_REAL;
        }
        columns[col] = out;
        break;
      }
      case 'A': {
        // SQL DATE as days since Unix epoch. Date is a double in R.
        Rcpp::NumericVector out(n);
        for (uint32_t i=0; i<n; i++) {
          int32_t day = r.i32();
          out[i] = present(valid, i) ? static_cast<double>(day) : NA_REAL;
        }
        out.attr("class") = "Date";
        columns[col] = out;
        break;
      }
      case 'T':
      case 'W':
      case 'O': {
        // Timestamp ticks: 100 ns since 0001-01-01.
        // T = UTC wall clock, W = local wall clock not yet timezone-adjusted,
        // O = already normalized to UTC by the .NET DateTimeOffset.
        const int64_t epoch_ticks = INT64_C(621355968000000000);
        if (kind == 'T' && tz != "UTC" && tz != "GMT")
          throw std::runtime_error("UTC wall-clock kind T requires UTC/GMT");
        if (kind == 'W' && (tz == "UTC" || tz == "GMT"))
          throw std::runtime_error("Local wall-clock kind W requires a non-UTC timezone");
        Rcpp::NumericVector out(n);
        for (uint32_t i=0; i<n; i++) {
          int64_t ticks = r.i64();
          if (present(valid, i)) {
            // Refuse corrupt frames before subtraction to avoid signed overflow.
            if (ticks < 0 || ticks > INT64_C(3155378975999999999))
              throw std::runtime_error("Out-of-range SCB2 timestamp ticks");
            out[i] = static_cast<double>(ticks - epoch_ticks) / 1.0e7;
          } else out[i] = NA_REAL;
        }
        if (kind != 'W') {
          out.attr("class") = Rcpp::CharacterVector::create("POSIXct", "POSIXt");
          out.attr("tzone") = kind == 'O' ? "UTC" : tz;
        }
        columns[col] = out;
        break;
      }
      case 'H': {
        Rcpp::NumericVector out(n);
        for (uint32_t i=0; i<n; i++) {
          int64_t ticks = r.i64();
          if (present(valid,i) && (ticks < 0 || ticks >= INT64_C(864000000000)))
            throw std::runtime_error("Invalid SQL time ticks");
          out[i] = present(valid,i) ? static_cast<double>(ticks) / 1.0e7 : NA_REAL;
        }
        out.attr("class") = "difftime";
        out.attr("units") = "secs";
        columns[col] = out;
        break;
      }
      case 'B': {
        Rcpp::LogicalVector out(n);
        for (uint32_t i=0; i<n; i++) {
          uint8_t v = r.u8();
          if (v > 1) throw std::runtime_error("Invalid SCB2 boolean byte");
          out[i] = present(valid, i) ? (v ? TRUE : FALSE) : NA_LOGICAL;
        }
        columns[col] = out;
        break;
      }
      case 'S': {
        Rcpp::CharacterVector out(n);
        for (uint32_t i=0; i<n; i++) {
          uint32_t len = r.u32();
          const unsigned char* s = r.take(len);
          if (present(valid,i)) {
            if (len > INT_MAX) throw std::runtime_error("Oversized UTF8 field");
            if (std::memchr(s, 0, len))
              throw std::runtime_error("Embedded NUL in SQL text is not representable by an R character string");
            SET_STRING_ELT(out, i, Rf_mkCharLenCE(reinterpret_cast<const char*>(s),
                                                  int(len), CE_UTF8));
          } else {
            if (len) throw std::runtime_error("Nonempty payload for SQL NULL text");
            SET_STRING_ELT(out, i, NA_STRING);
          }
        }
        columns[col] = out;
        break;
      }
      case 'Y': {
        // A direct VECSXP/RAWSXP path avoids Rcpp proxy allocations per cell.
        // Repeated adjacent blobs are common for BITMAP/flags/fixed tokens and
        // are especially expensive as 1e6 separate RAWSXP allocations. Share
        // the already-protected R object only if its contents are identical.
        // R's copy-on-modify semantics preserve independent list-element writes.
        SEXP out = PROTECT(Rf_allocVector(VECSXP, static_cast<R_xlen_t>(n)));
        SEXP last = R_NilValue;
        const unsigned char* last_bytes = nullptr;
        uint32_t last_len = 0;
        for (uint32_t i=0; i<n; i++) {
          uint32_t len = r.u32();
          const unsigned char* bytes = r.take(len);
          if (!present(valid, i)) {
            if (len) throw std::runtime_error("Nonempty payload for SQL NULL binary");
            SET_VECTOR_ELT(out, i, R_NilValue);
            continue;
          }
          if (last != R_NilValue && len == last_len &&
              (len == 0 || std::memcmp(bytes, last_bytes, len) == 0)) {
            SET_VECTOR_ELT(out, i, last);
            continue;
          }
          SEXP b = PROTECT(Rf_allocVector(RAWSXP, static_cast<R_xlen_t>(len)));
          if (len) std::memcpy(RAW(b), bytes, len);
          SET_VECTOR_ELT(out, i, b);
          UNPROTECT(1);
          last = b;                 // Rooted by out until this column finishes.
          last_bytes = bytes;       // Pointer into the still-live SCB2 frame.
          last_len = len;
        }
        columns[col] = out;
        UNPROTECT(1);
        break;
      }
      default: throw std::runtime_error("Unknown SCB2 wire kind");
    }
  }
  if (r.remaining() != 0) throw std::runtime_error("Trailing bytes in SCB2 batch");
  return Rcpp::List::create(Rcpp::Named("columns") = columns,
                            Rcpp::Named("kinds") = kinds,
                            Rcpp::Named("rows") = int(n),
                            Rcpp::Named("bigint_min_rows") = bigint_min_rows,
                            Rcpp::Named("bigint_min_counts") = bigint_min_counts);
  END_RCPP
}
