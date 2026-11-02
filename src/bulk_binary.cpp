// SBIN v1 input for SqlBulkCopy (native temporal code extension 12 and 13).
// The helper consumes this in-memory column
// format through IDataReader; SQL values never pass through JSON or a file.
#include <Rcpp.h>
#include <Rinternals.h>
#include <cmath>
#include <cstdint>
#include <cstring>
#include <limits>
#include <stdexcept>
#include <string>
#include <vector>

namespace {
constexpr size_t MAX_BATCH_BYTES = 128ULL * 1024 * 1024;
struct Writer {
  std::vector<unsigned char> bytes;
  void room(size_t n) {
    if (n > MAX_BATCH_BYTES || bytes.size() > MAX_BATCH_BYTES - n)
      throw std::runtime_error("Binary bulk batch exceeds 128 MiB; decrease batch_rows.");
  }
  void byte(uint8_t b) { room(1); bytes.push_back(b); }
  void u32(uint32_t v) { room(4); for(int k=0;k<4;k++) bytes.push_back(static_cast<uint8_t>(v>>(8*k))); }
  void u64(uint64_t v) { room(8); for(int k=0;k<8;k++) bytes.push_back(static_cast<uint8_t>(v>>(8*k))); }
  // Append one contiguous fixed-width block. Primitive R vectors already use
  // packed 32- and 64-bit values; on little-endian hosts a single memcpy
  // preserves the previous on-wire format without per-scalar push_back calls.
  void fixed_block(const void* ptr, size_t n, size_t width) {
    if (width && n > MAX_BATCH_BYTES / width)
      throw std::runtime_error("Binary bulk batch exceeds 128 MiB; decrease batch_rows.");
    const size_t bytes_to_copy = n * width;
    room(bytes_to_copy);
    const size_t at = bytes.size();
    bytes.resize(at + bytes_to_copy);
    if (bytes_to_copy) std::memcpy(bytes.data() + at, ptr, bytes_to_copy);
  }
  void block(const void* p, size_t n) {
    room(n);
    const auto* x=reinterpret_cast<const unsigned char*>(p);
    bytes.insert(bytes.end(),x,x+n);
  }
};
int kind_code(const std::string& s) {
  if (s=="integer") return 1;
  if (s=="integer64") return 2;
  if (s=="double") return 3;
  if (s=="logical") return 4;
  if (s=="text") return 5;
  if (s=="decimal") return 6;
  if (s=="date") return 7;
  if (s=="datetime") return 8;
  if (s=="datetimeoffset") return 9;
  if (s=="time") return 10;
  if (s=="binary") return 11;
  // Native POSIXct Unix seconds. Legacy UTF-8 datetime=8/offset=9 remain valid.
  if (s=="datetime_native") return 12;
  if (s=="datetimeoffset_native") return 13;
  // SBIN v1 backward-compatible native write extensions: 8-byte IEEE double
  // plus the existing NULL bitmap. The helper selects the SQL destination.
  if (s=="date_native") return 17;
  if (s=="time_native") return 18;
  if (s=="decimal_native") return 19;
  throw std::runtime_error("Unsupported binary bulk column kind: " + s);
}
}

extern "C" SEXP sqlcpp_bulk_encode(SEXP columnsSEXP, SEXP kindsSEXP) {
  BEGIN_RCPP
  Rcpp::List columns(columnsSEXP);
  Rcpp::CharacterVector kinds(kindsSEXP);
  if (columns.size()==0 || columns.size()!=kinds.size() || columns.size()>4096)
    throw std::runtime_error("Invalid binary bulk column count.");
  const R_xlen_t n=Rf_xlength(columns[0]);
  if (n<0 || n>131072) throw std::runtime_error("Binary bulk batch row count exceeds 131072.");
  Writer w;
  w.block("SBIN",4); w.u32(1); w.u32(static_cast<uint32_t>(n));
  w.u32(static_cast<uint32_t>(columns.size()));
  for (R_xlen_t c=0;c<columns.size();++c) {
    SEXP x=columns[c];
    if (Rf_xlength(x)!=n) throw std::runtime_error("Binary bulk column length mismatch.");
    const int kind=kind_code(Rcpp::as<std::string>(kinds[c]));
    const int type=TYPEOF(x);
    if ((kind==1 && type!=INTSXP) || (kind==2 && type!=REALSXP) ||
        (kind==3 && type!=REALSXP) || ((kind==12 || kind==13 || kind==17 || kind==18 || kind==19) && type!=REALSXP) ||
        (kind==4 && type!=LGLSXP) ||
        ((kind>=5 && kind<=10) && type!=STRSXP) ||
        (kind==11 && type!=VECSXP))
      throw std::runtime_error("Binary column R type does not match kind.");
    w.byte(static_cast<uint8_t>(kind));
    const size_t bitmap_at=w.bytes.size();
    const size_t bitmap_bytes=(static_cast<size_t>(n)+7)/8;
    w.room(bitmap_bytes);
    w.bytes.insert(w.bytes.end(),bitmap_bytes,0);
    // Fixed-size columns are already contiguous in the R SEXPs. Build the
    // NULL bitmap in one pass, memcpy the packed data once, and zero only the
    // missing slots (matching v1's exact zero-filled NULL representation).
    if ((kind >= 1 && kind <= 3) || kind==12 || kind==13 ||
        kind==17 || kind==18 || kind==19) {
      const size_t width = kind==1 ? 4 : 8;
      const void* ptr = kind==1
        ? static_cast<const void*>(INTEGER(x))
        : static_cast<const void*>(REAL(x));
      // Standard Windows/x86_64 and Linux/x86_64 hosts are little-endian.
      const uint16_t endian_probe = 1;
      const bool little_endian = *reinterpret_cast<const uint8_t*>(&endian_probe)==1;
      const size_t data_start = w.bytes.size();
      if (little_endian) w.fixed_block(ptr, static_cast<size_t>(n), width);
      else {
        for (R_xlen_t r=0; r<n; ++r) {
          if(kind==1) w.u32(static_cast<uint32_t>(INTEGER(x)[r]));
          else {
            uint64_t u=0;
            std::memcpy(&u, REAL(x)+r, 8);
            w.u64(u);
          }
        }
      }
      for (R_xlen_t r=0; r<n; ++r) {
        bool missing = false;
        if (kind==1) missing = INTEGER(x)[r]==NA_INTEGER;
        else if (kind==2) {
          int64_t i;
          std::memcpy(&i, REAL(x)+r, 8);
          missing = i==std::numeric_limits<int64_t>::min();
        } else {
          const double d=REAL(x)[r];
          if (ISNAN(d) && !R_IsNA(d))
            throw std::runtime_error("NaN is not supported in native SQL bulk doubles.");
          if (std::isinf(d))
            throw std::runtime_error("Infinite native SQL bulk doubles are unsupported.");
          missing = R_IsNA(d);
        }
        if (missing) {
          w.bytes[bitmap_at+static_cast<size_t>(r)/8] |= (1u << (r%8));
          std::memset(w.bytes.data()+data_start+static_cast<size_t>(r)*width,0,width);
        }
      }
      continue;
    }
    for(R_xlen_t r=0;r<n;++r) {
      const bool missing = kind==4 ? LOGICAL(x)[r]==NA_LOGICAL :
        kind==11 ? VECTOR_ELT(x,r)==R_NilValue : STRING_ELT(x,r)==NA_STRING;
      if (missing) w.bytes[bitmap_at+static_cast<size_t>(r)/8] |= (1u << (r%8));
      if (kind==4) w.byte(missing ? 0 : static_cast<uint8_t>(LOGICAL(x)[r]));
      else if (kind==11) {
        if (missing) w.u32(0);
        else {
          SEXP el=VECTOR_ELT(x,r);
          if (TYPEOF(el)!=RAWSXP) throw std::runtime_error("Binary list cell is not raw or NULL.");
          const R_xlen_t len=XLENGTH(el);
          if (len>std::numeric_limits<uint32_t>::max()) throw std::runtime_error("Binary SQL cell is too large.");
          w.u32(static_cast<uint32_t>(len));
          if(len) w.block(RAW(el),static_cast<size_t>(len));
        }
      } else {
        if (missing) w.u32(0);
        else {
          SEXP s=STRING_ELT(x,r);
          const char* u=Rf_translateCharUTF8(s);
          size_t len=std::strlen(u);
          if(len>std::numeric_limits<uint32_t>::max()) throw std::runtime_error("Text SQL cell is too large.");
          w.u32(static_cast<uint32_t>(len));
          if(len) w.block(u,len);
        }
      }
    }
  }
  Rcpp::RawVector ans(w.bytes.size());
  if (!w.bytes.empty()) std::memcpy(RAW(ans),w.bytes.data(),w.bytes.size());
  return ans;
  END_RCPP
}
