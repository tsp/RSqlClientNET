// v0.0.26: exact, order-independent streamed ID integrity checking.
// Only the bitmap, not result rows, is kept in memory.
#include <Rcpp.h>
#include <cstdint>
#include <cmath>
#include <limits>
#include <vector>
#include <string>

struct SqlIdBitmap {
  uint64_t expected;
  uint64_t examined = 0;
  uint64_t unique = 0;
  uint64_t duplicates = 0;
  uint64_t invalid = 0;
  std::vector<uint8_t> bits;
  std::vector<double> duplicate_examples;
  std::vector<double> invalid_examples;
  explicit SqlIdBitmap(uint64_t n) : expected(n), bits(static_cast<size_t>((n + 7) / 8), 0) {}
  void add(double value) {
    ++examined;
    if (!R_finite(value) || value < 1.0 || value > static_cast<double>(expected) ||
        std::floor(value) != value) {
      ++invalid;
      if (invalid_examples.size() < 5) invalid_examples.push_back(value);
      return;
    }
    const uint64_t index = static_cast<uint64_t>(value) - 1;
    uint8_t &byte = bits[static_cast<size_t>(index >> 3)];
    const uint8_t mask = static_cast<uint8_t>(1u << (index & 7));
    if (byte & mask) {
      ++duplicates;
      if (duplicate_examples.size() < 5) duplicate_examples.push_back(value);
    } else {
      byte |= mask;
      ++unique;
    }
  }
};

extern "C" SEXP sqlcpp_id_bitmap_new(SEXP size) {
  try {
    const double x = Rcpp::as<double>(size);
    // Explicit memory cap: 200 million IDs ~24 MiB bitmap.
    if (!R_finite(x) || x < 1 || x > 200000000 || std::floor(x) != x)
      Rcpp::stop("expected_rows must be a whole number between 1 and 200000000.");
    Rcpp::XPtr<SqlIdBitmap> p(new SqlIdBitmap(static_cast<uint64_t>(x)), true);
    return p;
  } catch (const std::exception& ex) { Rcpp::stop("id bitmap initialization: %s", ex.what()); }
  return R_NilValue;
}

extern "C" SEXP sqlcpp_id_bitmap_add(SEXP pointer, SEXP ids) {
  try {
    Rcpp::XPtr<SqlIdBitmap> p(pointer);
    if (TYPEOF(ids) == INTSXP) {
      Rcpp::IntegerVector input(ids);
      for (R_xlen_t i = 0; i < input.size(); ++i)
        p->add(input[i] == NA_INTEGER ? NA_REAL : static_cast<double>(input[i]));
    } else if (TYPEOF(ids) == REALSXP && !Rf_inherits(ids, "integer64")) {
      Rcpp::NumericVector input(ids);
      for (R_xlen_t i = 0; i < input.size(); ++i) p->add(input[i]);
    } else {
      Rcpp::stop("ID column must be integer or numeric (not integer64); cast bigint IDs to numeric only if exactly representable.");
    }
    return R_NilValue;
  } catch (const std::exception& ex) { Rcpp::stop("id bitmap update: %s", ex.what()); }
  return R_NilValue;
}

extern "C" SEXP sqlcpp_id_bitmap_report(SEXP pointer) {
  try {
    Rcpp::XPtr<SqlIdBitmap> p(pointer);
    Rcpp::NumericVector missing_examples;
    for (uint64_t id = 1; id <= p->expected && missing_examples.size() < 5; ++id) {
      uint64_t i = id - 1;
      if (!(p->bits[static_cast<size_t>(i >> 3)] & (1u << (i & 7))))
        missing_examples.push_back(static_cast<double>(id));
    }
    return Rcpp::List::create(
      Rcpp::Named("ok") = (p->unique == p->expected && p->duplicates == 0 && p->invalid == 0 && p->examined == p->expected),
      Rcpp::Named("expected_rows") = static_cast<double>(p->expected),
      Rcpp::Named("examined_ids") = static_cast<double>(p->examined),
      Rcpp::Named("unique_ids") = static_cast<double>(p->unique),
      Rcpp::Named("duplicate_ids") = static_cast<double>(p->duplicates),
      Rcpp::Named("invalid_ids") = static_cast<double>(p->invalid),
      Rcpp::Named("missing_ids") = static_cast<double>(p->expected - p->unique),
      Rcpp::Named("duplicate_examples") = Rcpp::wrap(p->duplicate_examples),
      Rcpp::Named("invalid_examples") = Rcpp::wrap(p->invalid_examples),
      Rcpp::Named("missing_examples") = missing_examples,
      Rcpp::Named("bitmap_bytes") = static_cast<double>(p->bits.size())
    );
  } catch (const std::exception& ex) { Rcpp::stop("id bitmap report: %s", ex.what()); }
  return R_NilValue;
}
