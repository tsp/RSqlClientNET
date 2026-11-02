#include <Rcpp.h>
#include <cmath>
#include "native_process.hpp"

using RSqlClientNET_native::Process;

extern "C" SEXP sqlcpp_start(SEXP dotnet, SEXP dll) {
  BEGIN_RCPP
  Rcpp::XPtr<Process> ptr(new Process(Rcpp::as<std::string>(dotnet),
                                       Rcpp::as<std::string>(dll)), true);
  return ptr;
  END_RCPP
}

extern "C" SEXP sqlcpp_send(SEXP p, SEXP json, SEXP timeout) {
  BEGIN_RCPP
  Rcpp::XPtr<Process> ptr(p);
  try {
    ptr->send(Rcpp::as<std::string>(json), Rcpp::as<double>(timeout),
              []() { Rcpp::checkUserInterrupt(); });
  } catch (...) { ptr->close(); throw; }
  return R_NilValue;
  END_RCPP
}

// One interruptible write for both the JSON control header and the raw binary
// body. The body has an exact byte count and does not use newline framing.
// The header is a short JSON line and the SBIN body an exact raw byte
// count. Return optional diagnostics to R: the call site remains responsible
// for timeout/cancellation and for invalidating an incomplete request.
extern "C" SEXP sqlcpp_send_binary(SEXP p, SEXP json, SEXP binary, SEXP timeout) {
  BEGIN_RCPP
  Rcpp::XPtr<Process> ptr(p);
  if (TYPEOF(binary) != RAWSXP) throw std::runtime_error("Binary upload requires a raw vector.");
  const auto start = RSqlClientNET_native::Clock::now();
  std::string packet = Rcpp::as<std::string>(json);
  if (packet.find('\n') != std::string::npos || packet.find('\r') != std::string::npos)
    throw std::runtime_error("Binary upload header cannot contain newlines.");
  packet.push_back('\n');
  const size_t n = XLENGTH(binary);
  packet.append(reinterpret_cast<const char*>(RAW(binary)), n);
  const auto assembled = RSqlClientNET_native::Clock::now();
  try {
    // The owned packet is moved through Process::send into the Windows I/O
    // worker. The worker MUST own its bytes: an interrupted R call may unwind
    // while a cancelled WriteFile operation is still being cleaned up.
    ptr->send(std::move(packet), Rcpp::as<double>(timeout),
              []() { Rcpp::checkUserInterrupt(); }, false);
  } catch (...) { ptr->close(); throw; }
  const auto complete = RSqlClientNET_native::Clock::now();
  const auto in_ms = [](const RSqlClientNET_native::Clock::duration d) {
    return std::chrono::duration<double, std::milli>(d).count();
  };
  return Rcpp::List::create(
    Rcpp::Named("packetMs") = in_ms(assembled - start),
    Rcpp::Named("pipeSendMs") = in_ms(complete - assembled));
  END_RCPP
}

extern "C" SEXP sqlcpp_receive(SEXP p, SEXP timeout) {
  BEGIN_RCPP
  Rcpp::XPtr<Process> ptr(p);
  RSqlClientNET_native::Frame frame;
  try {
    frame = ptr->receive(Rcpp::as<double>(timeout),
                         []() { Rcpp::checkUserInterrupt(); });
  } catch (...) { ptr->close(); throw; }
  Rcpp::RawVector data(frame.payload.size());
  if (!frame.payload.empty()) std::memcpy(RAW(data), frame.payload.data(), frame.payload.size());
  return Rcpp::List::create(Rcpp::Named("kind") = std::string(1, frame.kind),
                            Rcpp::Named("payload") = data);
  END_RCPP
}

extern "C" SEXP sqlcpp_alive(SEXP p) {
  BEGIN_RCPP
  Rcpp::XPtr<Process> ptr(p);
  return Rcpp::wrap(ptr->alive());
  END_RCPP
}

extern "C" SEXP sqlcpp_close(SEXP p) {
  BEGIN_RCPP
  Rcpp::XPtr<Process> ptr(p);
  ptr->close();
  return R_NilValue;
  END_RCPP
}

extern "C" SEXP sqlcpp_pipe_stats(SEXP p, SEXP reset) {
  BEGIN_RCPP
  Rcpp::XPtr<Process> ptr(p);
  auto st = ptr->take_stats(Rcpp::as<bool>(reset));
  return Rcpp::List::create(
    Rcpp::Named("stdout_peeks") = static_cast<double>(st.peeks),
    Rcpp::Named("empty_polls") = static_cast<double>(st.empty_polls),
    Rcpp::Named("read_calls") = static_cast<double>(st.reads),
    Rcpp::Named("bytes_read") = static_cast<double>(st.bytes),
    Rcpp::Named("sleep_seconds") = static_cast<double>(st.sleep_us) / 1e6,
    Rcpp::Named("blocking_reads") = static_cast<double>(st.reads),
    Rcpp::Named("timeout_count") = static_cast<double>(st.timeouts),
    Rcpp::Named("cancel_count") = static_cast<double>(st.cancellations),
    Rcpp::Named("poll_ms") = st.poll_ms,
    Rcpp::Named("pipe_buffer_kib") = st.buffer_kib);
  END_RCPP
}
