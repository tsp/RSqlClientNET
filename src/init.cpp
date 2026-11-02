#include <R.h>
#include <Rinternals.h>
#include <R_ext/Rdynload.h>

extern "C" {
  SEXP sqlcpp_start(SEXP, SEXP);
  SEXP sqlcpp_bulk_encode(SEXP, SEXP);
  SEXP sqlcpp_send_binary(SEXP, SEXP, SEXP, SEXP);
  SEXP sqlcpp_send(SEXP, SEXP, SEXP);
  SEXP sqlcpp_receive(SEXP, SEXP);
  SEXP sqlcpp_alive(SEXP);
  SEXP sqlcpp_close(SEXP);
  SEXP sqlcpp_pipe_stats(SEXP, SEXP);
  SEXP sqlcpp_decode_batch(SEXP, SEXP, SEXP);
  SEXP sqlcpp_process_memory();
  SEXP sqlcpp_id_bitmap_new(SEXP);
  SEXP sqlcpp_id_bitmap_add(SEXP, SEXP);
  SEXP sqlcpp_id_bitmap_report(SEXP);
}

static const R_CallMethodDef CallEntries[] = {
  {"sqlcpp_start", (DL_FUNC) &sqlcpp_start, 2},
  {"sqlcpp_bulk_encode", (DL_FUNC) &sqlcpp_bulk_encode, 2},
  {"sqlcpp_send_binary", (DL_FUNC) &sqlcpp_send_binary, 4},
  {"sqlcpp_send", (DL_FUNC) &sqlcpp_send, 3},
  {"sqlcpp_receive", (DL_FUNC) &sqlcpp_receive, 2},
  {"sqlcpp_alive", (DL_FUNC) &sqlcpp_alive, 1},
  {"sqlcpp_close", (DL_FUNC) &sqlcpp_close, 1},
  {"sqlcpp_pipe_stats", (DL_FUNC) &sqlcpp_pipe_stats, 2},
  {"sqlcpp_decode_batch", (DL_FUNC) &sqlcpp_decode_batch, 3},
  {"sqlcpp_process_memory", (DL_FUNC) &sqlcpp_process_memory, 0},
  {"sqlcpp_id_bitmap_new", (DL_FUNC) &sqlcpp_id_bitmap_new, 1},
  {"sqlcpp_id_bitmap_add", (DL_FUNC) &sqlcpp_id_bitmap_add, 2},
  {"sqlcpp_id_bitmap_report", (DL_FUNC) &sqlcpp_id_bitmap_report, 1},
  {NULL, NULL, 0}
};

extern "C" void R_init_RSqlClientNET(DllInfo* dll) {
  R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}
