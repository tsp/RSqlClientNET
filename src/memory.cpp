#include <Rcpp.h>
#include <cstdint>
#include <limits>
#include <string>
#ifdef _WIN32
  #ifndef NOMINMAX
    #define NOMINMAX 1
  #endif
  #include <windows.h>
  #include <psapi.h>
#elif defined(__linux__)
  #include <cstdio>
  #include <cstring>
  #include <unistd.h>
#elif defined(__APPLE__)
  #include <mach/mach.h>
#endif

// These are OS resident-set measurements, not R's GC Vcells count. A
// process-lifetime peak must never be advertised as a query-specific peak.
extern "C" SEXP sqlcpp_process_memory() {
  BEGIN_RCPP
  double rss = NA_REAL, lifetime_peak = NA_REAL, private_bytes = NA_REAL;
  std::string source = "unsupported";
#ifdef _WIN32
  PROCESS_MEMORY_COUNTERS_EX pmc{};
  pmc.cb = sizeof(pmc);
  if (GetProcessMemoryInfo(GetCurrentProcess(), reinterpret_cast<PROCESS_MEMORY_COUNTERS*>(&pmc), sizeof(pmc))) {
    rss = static_cast<double>(pmc.WorkingSetSize);
    lifetime_peak = static_cast<double>(pmc.PeakWorkingSetSize);
    private_bytes = static_cast<double>(pmc.PrivateUsage);
    source = "Windows GetProcessMemoryInfo";
  }
#elif defined(__linux__)
  FILE* f = std::fopen("/proc/self/status", "r");
  if (f) {
    char line[256];
    while (std::fgets(line, sizeof(line), f)) {
      unsigned long long kib = 0;
      if (std::sscanf(line, "VmRSS: %llu kB", &kib) == 1)
        rss = static_cast<double>(kib) * 1024.;
      else if (std::sscanf(line, "VmHWM: %llu kB", &kib) == 1)
        lifetime_peak = static_cast<double>(kib) * 1024.;
    }
    std::fclose(f);
    source = "Linux /proc/self/status";
  }
#elif defined(__APPLE__)
  task_basic_info_data_t info{};
  mach_msg_type_number_t n = TASK_BASIC_INFO_COUNT;
  if (task_info(mach_task_self(), TASK_BASIC_INFO, reinterpret_cast<task_info_t>(&info), &n) == KERN_SUCCESS) {
    rss = static_cast<double>(info.resident_size);
    source = "macOS task_info (current only)";
  }
#endif
  return Rcpp::List::create(
    Rcpp::Named("rss_bytes") = rss,
    Rcpp::Named("peak_rss_bytes") = lifetime_peak,
    Rcpp::Named("private_bytes") = private_bytes,
    Rcpp::Named("source") = source);
  END_RCPP
}
