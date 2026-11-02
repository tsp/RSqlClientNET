#ifndef RSQLCLIENTNET_NATIVE_PROCESS_HPP
#define RSQLCLIENTNET_NATIVE_PROCESS_HPP

// RSqlClientNET native transport v0.0.20.
// On Windows, *only an I/O worker thread* enters synchronous ReadFile/WriteFile.
// The calling R thread waits in short, interruptible intervals. On cancellation,
// we terminate the .NET helper and request CancelSynchronousIo on the worker.
// Raw results are kept in memory; no result files are created.

#include <algorithm>
#include <chrono>
#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <cstring>
#include <functional>
#include <memory>
#include <stdexcept>
#include <string>
#include <vector>
#include <thread>
#include <utility>

#ifdef _WIN32
#ifndef NOMINMAX
#define NOMINMAX
#endif
#include <windows.h>
#else
#include <cerrno>
#include <csignal>
#include <fcntl.h>
#include <poll.h>
#include <spawn.h>
#ifdef __APPLE__
// A dynamically loaded macOS module obtains environ through crt_externs.
#include <crt_externs.h>
#else
extern "C" { extern char **environ; }
#endif
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#endif

namespace RSqlClientNET_native {
using Clock = std::chrono::steady_clock;
using InterruptCheck = std::function<void()>;

inline std::runtime_error err(const std::string& what) {
  return std::runtime_error("RSqlClientNET native pipe: " + what);
}

struct Frame { char kind; std::vector<unsigned char> payload; };

class Process {
#ifdef _WIN32
  HANDLE in_ = nullptr, out_ = nullptr, err_ = nullptr, process_ = nullptr;
  static void closeHandle(HANDLE& h) {
    if (h && h != INVALID_HANDLE_VALUE) CloseHandle(h);
    h = nullptr;
  }
  static std::wstring wide(const std::string& value) {
    int n = MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, value.c_str(), -1, nullptr, 0);
    if (n == 0) throw err("UTF-8 to UTF-16 conversion failed");
    std::wstring w(n, L'\0');
    MultiByteToWideChar(CP_UTF8, MB_ERR_INVALID_CHARS, value.c_str(), -1, &w[0], n);
    w.pop_back();
    return w;
  }
  static std::wstring quote(const std::wstring& arg) {
    std::wstring output = L"\"";
    unsigned nslashes = 0;
    for (wchar_t c : arg) {
      if (c == L'\\') { ++nslashes; continue; }
      if (c == L'\"') { output.append(nslashes * 2 + 1, L'\\'); output += c; nslashes = 0; continue; }
      output.append(nslashes, L'\\'); nslashes = 0; output += c;
    }
    output.append(nslashes * 2, L'\\'); output += L'\"';
    return output;
  }

  // Each job owns a duplicated pipe handle. If Windows unexpectedly declines
  // cancellation, the R thread is still free to return after a bounded wait;
  // the worker, not the parent Process, owns the remaining I/O state.
  struct WinJob {
    HANDLE fd = nullptr;
    bool writing = false;
    // 256 KiB default based on randomized SQL Server measurements (RC26):
    // substantially fewer blocking WriteFile calls for large SBIN uploads.
    // 16 KiB remains available for regression comparisons.
    size_t write_chunk_bytes = 256u * 1024u;
    std::string command;
    Frame frame{'J', {}};
    std::string error;
    uint64_t reads = 0, bytes = 0;
    ~WinJob() { if (fd) CloseHandle(fd); }
  };

  static void job_read_exact(WinJob& job, unsigned char* dest, size_t length) {
    size_t done = 0;
    while (done < length) {
      DWORD got = 0;
      DWORD want = static_cast<DWORD>(std::min<size_t>(length - done, 1024u * 1024u));
      if (!ReadFile(job.fd, dest + done, want, &got, nullptr))
        throw err("ReadFile on stdout failed (Win32 " + std::to_string(GetLastError()) + ")");
      if (got == 0) throw err(".NET stdout closed before complete frame");
      done += static_cast<size_t>(got);
      ++job.reads;
      job.bytes += got;
    }
  }

  static void job_read_frame(WinJob& job) {
    unsigned char header[5];
    job_read_exact(job, header, sizeof(header));
    if (header[0] != 'J' && header[0] != 'D')
      throw err("invalid frame tag " + std::to_string(static_cast<unsigned>(header[0])) +
                "; expected J/D; check helper and stdout pollution");
    uint32_t length = static_cast<uint32_t>(header[1]) |
      (static_cast<uint32_t>(header[2]) << 8) |
      (static_cast<uint32_t>(header[3]) << 16) |
      (static_cast<uint32_t>(header[4]) << 24);
    if (length > MAX_FRAME) throw err("oversized frame (" + std::to_string(length) + " bytes)");
    job.frame.kind = static_cast<char>(header[0]);
    job.frame.payload.resize(length);
    if (length) job_read_exact(job, job.frame.payload.data(), length);
  }

  static void job_write_command(WinJob& job) {
    size_t done = 0;
    while (done < job.command.size()) {
      DWORD sent = 0;
      DWORD want = static_cast<DWORD>(std::min<size_t>(job.command.size() - done,
                                                       job.write_chunk_bytes));
      if (!WriteFile(job.fd, job.command.data() + done, want, &sent, nullptr))
        throw err("WriteFile on stdin failed (Win32 " + std::to_string(GetLastError()) + ")");
      if (sent == 0) throw err("stdin pipe was closed while writing");
      done += sent;
    }
  }

  static DWORD WINAPI io_thread(LPVOID arg) {
    // Heap-held shared_ptr keeps the job and duplicated pipe handle alive even
    // if a noncooperative I/O operation survives cancellation briefly.
    std::unique_ptr<std::shared_ptr<WinJob>> holder(
      reinterpret_cast<std::shared_ptr<WinJob>*>(arg));
    std::shared_ptr<WinJob> job = *holder;
    try {
      if (job->writing) job_write_command(*job);
      else job_read_frame(*job);
    } catch (const std::exception& e) {
      job->error = e.what();
    } catch (...) {
      job->error = "unknown native I/O error";
    }
    return 0;
  }

  std::shared_ptr<WinJob> new_job(HANDLE pipe, bool writing) {
    auto job = std::make_shared<WinJob>();
    if (!DuplicateHandle(GetCurrentProcess(), pipe, GetCurrentProcess(),
                         &job->fd, 0, FALSE, DUPLICATE_SAME_ACCESS))
      throw err("DuplicateHandle failed (Win32 " + std::to_string(GetLastError()) + ")");
    job->writing = writing;
    if (writing) {
      // User/benchmark override for Windows WriteFile block size;
      // the default is 256 KiB when the variable is unset.
      const char* opt = std::getenv("RSQLCLIENTNET_PIPE_WRITE_KIB");
      if (opt && *opt) {
        char* end = nullptr;
        const long kib = std::strtol(opt, &end, 10);
        if (*end != '\0' || (kib != 16 && kib != 64 && kib != 256 && kib != 1024))
          throw err("RSQLCLIENTNET_PIPE_WRITE_KIB must be 16, 64, 256, or 1024");
        job->write_chunk_bytes = static_cast<size_t>(kib) * 1024;
      }
    }
    return job;
  }

  void abort_child() {
    // Hard cancellation intentionally invalidates the connection. The helper
    // may be blocked inside SqlCommand.ExecuteReader/SqlDataReader.Read().
    if (process_ && WaitForSingleObject(process_, 0) == WAIT_TIMEOUT)
      TerminateProcess(process_, 1223);
  }

  void run_job(const std::shared_ptr<WinJob>& job, Clock::time_point deadline,
               const InterruptCheck& check) {
    auto holder = std::unique_ptr<std::shared_ptr<WinJob>>(
      new std::shared_ptr<WinJob>(job));
    HANDLE worker = CreateThread(nullptr, 0, &Process::io_thread, holder.get(), 0, nullptr);
    if (!worker) throw err("CreateThread failed (Win32 " + std::to_string(GetLastError()) + ")");
    holder.release();
    bool complete = false;
    try {
      for (;;) {
        if (check) check(); // Rcpp::checkUserInterrupt on the MAIN thread only
        auto now = Clock::now();
        if (now >= deadline) {
          ++timeouts_;
          throw err("timeout during blocking pipe I/O; helper will be terminated");
        }
        const auto remaining = std::chrono::duration_cast<std::chrono::milliseconds>(deadline - now).count();
        DWORD wait_ms = static_cast<DWORD>(std::max<int64_t>(1, std::min<int64_t>(50, remaining)));
        DWORD rc = WaitForSingleObject(worker, wait_ms);
        if (rc == WAIT_OBJECT_0) { complete = true; break; }
        if (rc != WAIT_TIMEOUT) throw err("WaitForSingleObject on I/O worker failed");
        drain_stderr();
      }
    } catch (...) {
      ++cancellations_;
      abort_child();
      // The worker can be inside a blocking anonymous-pipe ReadFile/WriteFile.
      // Cancelling synchronous I/O requires the *worker thread* handle.
      CancelSynchronousIo(worker);
      // Bounded join. If an unusual driver ignores cancellation, job memory and
      // the duplicated pipe handle remain owned by that worker until it exits.
      WaitForSingleObject(worker, 1500);
      CloseHandle(worker);
      throw;
    }
    if (complete) {
      CloseHandle(worker);
      stdout_read_calls_ += job->reads;
      stdout_read_bytes_ += job->bytes;
      if (!job->error.empty()) {
        drain_stderr();
        throw err(job->error + (stderr_tail_.empty() ? "" : ("; stderr: " + stderr_tail_)));
      }
    }
  }
#else
  int in_ = -1, out_ = -1, err_ = -1;
  pid_t pid_ = -1;
  static void closeFd(int& fd) { if (fd >= 0) ::close(fd); fd = -1; }
#endif
  std::string stderr_tail_;
  uint64_t stdout_peek_calls_ = 0, stdout_empty_polls_ = 0;
  uint64_t stdout_read_calls_ = 0, stdout_read_bytes_ = 0;
  uint64_t stdout_sleep_us_ = 0;
  uint64_t timeouts_ = 0, cancellations_ = 0;
  int poll_ms_ = 0;  // no polling on Windows v0.0.20
  int pipe_buffer_kib_ = 64;
  bool closed_ = false;
  static constexpr size_t MAX_FRAME = 128u * 1024u * 1024u;

  static int int_from_env(const char* key, int fallback, int lo, int hi) {
    const char* raw = std::getenv(key);
    if (!raw || !*raw) return fallback;
    char* end = nullptr;
    long number = std::strtol(raw, &end, 10);
    if (end == raw || *end != '\0' || number < lo || number > hi) return fallback;
    return static_cast<int>(number);
  }

  void drain_stderr() {
#ifdef _WIN32
    if (!err_) return;
    for (;;) {
      DWORD available = 0;
      if (!PeekNamedPipe(err_, nullptr, 0, nullptr, &available, nullptr)) { closeHandle(err_); break; }
      if (available == 0) break;
      char buf[1024]; DWORD got = 0;
      if (!ReadFile(err_, buf, std::min<DWORD>(available, sizeof(buf)), &got, nullptr) || !got) break;
      stderr_tail_.append(buf, buf + got);
    }
#else
    if (err_ < 0) return;
    for (;;) {
      char buf[1024]; ssize_t got = ::read(err_, buf, sizeof(buf));
      if (got > 0) stderr_tail_.append(buf, buf + got);
      else { if (got == 0) closeFd(err_); break; }
    }
#endif
    if (stderr_tail_.size() > 4096) stderr_tail_.erase(0, stderr_tail_.size() - 4096);
  }

#ifndef _WIN32
  void read_exact(unsigned char* dest, size_t n, Clock::time_point deadline,
                  const InterruptCheck& check) {
    size_t pos = 0;
    while (pos < n) {
      if (check) check();
      if (Clock::now() >= deadline) throw err("timeout waiting for stdout frame");
      drain_stderr();
      int ms = static_cast<int>(std::max<int64_t>(1, std::min<int64_t>(50,
        std::chrono::duration_cast<std::chrono::milliseconds>(deadline - Clock::now()).count())));
      struct pollfd fds[2] = {{out_, POLLIN, 0}, {err_, POLLIN, 0}};
      int rc = ::poll(fds, 2, ms);
      if (rc < 0) { if (errno == EINTR) continue; throw err("poll failed"); }
      if (fds[1].revents & (POLLIN | POLLHUP)) drain_stderr();
      if (fds[0].revents & (POLLIN | POLLHUP)) {
        ssize_t got = ::read(out_, dest + pos, n - pos);
        if (got > 0) { pos += static_cast<size_t>(got); ++stdout_read_calls_; stdout_read_bytes_ += static_cast<uint64_t>(got); }
        else if (got == 0) { drain_stderr(); throw err(".NET stdout reached EOF: " + stderr_tail_); }
        else if (errno != EINTR && errno != EAGAIN) throw err("read failed");
      }
    }
  }
#endif

public:
  struct Stats {
    uint64_t peeks, empty_polls, reads, bytes, sleep_us, timeouts, cancellations;
    int poll_ms, buffer_kib;
  };

  Process(const std::string& dotnet, const std::string& dll) {
#ifdef _WIN32
    pipe_buffer_kib_ = int_from_env("RSQLCLIENTNET_PIPE_BUFFER_KIB", 64, 0, 256);
    SECURITY_ATTRIBUTES sa{sizeof(SECURITY_ATTRIBUTES), nullptr, TRUE};
    HANDLE child_stdin_r = nullptr, child_stdout_w = nullptr, child_stderr_w = nullptr;
    try {
      if (!CreatePipe(&child_stdin_r, &in_, &sa, 0) ||
          !CreatePipe(&out_, &child_stdout_w, &sa, static_cast<DWORD>(pipe_buffer_kib_ * 1024)) ||
          !CreatePipe(&err_, &child_stderr_w, &sa, 0))
        throw err("CreatePipe failed");
      if (!SetHandleInformation(in_, HANDLE_FLAG_INHERIT, 0) ||
          !SetHandleInformation(out_, HANDLE_FLAG_INHERIT, 0) ||
          !SetHandleInformation(err_, HANDLE_FLAG_INHERIT, 0))
        throw err("SetHandleInformation failed");
      STARTUPINFOW si{}; si.cb = sizeof(si);
      si.dwFlags = STARTF_USESTDHANDLES;
      si.hStdInput = child_stdin_r;
      si.hStdOutput = child_stdout_w;
      si.hStdError = child_stderr_w;
      PROCESS_INFORMATION pi{};
      std::wstring cmd = quote(wide(dotnet)) + L" " + quote(wide(dll));
      std::vector<wchar_t> cmdline(cmd.begin(), cmd.end()); cmdline.push_back(L'\0');
      if (!CreateProcessW(wide(dotnet).c_str(), cmdline.data(), nullptr, nullptr,
                          TRUE, CREATE_NO_WINDOW, nullptr, nullptr, &si, &pi))
        throw err("CreateProcessW failed (Win32 " + std::to_string(GetLastError()) + ")");
      process_ = pi.hProcess;
      CloseHandle(pi.hThread);
      closeHandle(child_stdin_r); closeHandle(child_stdout_w); closeHandle(child_stderr_w);
    } catch (...) {
      closeHandle(child_stdin_r); closeHandle(child_stdout_w); closeHandle(child_stderr_w);
      closeHandle(in_); closeHandle(out_); closeHandle(err_);
      throw;
    }
#else
    int stdinfds[2] = {-1,-1}, stdoutfds[2] = {-1,-1}, stderrfds[2] = {-1,-1};
    auto close_created = [&]() {
      for (int fd : {stdinfds[0], stdinfds[1], stdoutfds[0], stdoutfds[1],
                     stderrfds[0], stderrfds[1]}) if (fd >= 0) ::close(fd);
    };
    if (::pipe(stdinfds) || ::pipe(stdoutfds) || ::pipe(stderrfds)) {
      int saved_errno = errno;
      close_created();
      throw err("pipe creation failed (errno " + std::to_string(saved_errno) + ")");
    }
#ifdef __APPLE__
    // Suppress SIGPIPE only on the helper stdin pipe. Darwin does not offer
    // sigtimedwait(); F_SETNOSIGPIPE makes a closed peer return EPIPE instead.
    if (::fcntl(stdinfds[1], F_SETNOSIGPIPE, 1) == -1) {
      int saved_errno = errno;
      close_created();
      throw err("fcntl(F_SETNOSIGPIPE) failed (errno " + std::to_string(saved_errno) + ")");
    }
#endif
    // POSIX spawn sets up the three pipes atomically without running any R/C++
    // code between fork and exec. It also avoids child-only calls to _exit,
    // which are correctly flagged by R CMD check's static compiled-code scan.
    posix_spawn_file_actions_t actions;
    int spawn_error = ::posix_spawn_file_actions_init(&actions);
    if (spawn_error != 0) {
      close_created();
      throw err("posix_spawn_file_actions_init failed (error " + std::to_string(spawn_error) + ")");
    }
    auto add_action = [&](int rc) {
      if (spawn_error == 0 && rc != 0) spawn_error = rc;
    };
    add_action(::posix_spawn_file_actions_adddup2(&actions, stdinfds[0], STDIN_FILENO));
    if (spawn_error == 0) add_action(::posix_spawn_file_actions_adddup2(&actions, stdoutfds[1], STDOUT_FILENO));
    if (spawn_error == 0) add_action(::posix_spawn_file_actions_adddup2(&actions, stderrfds[1], STDERR_FILENO));
    if (spawn_error == 0) {
      for (int fd : {stdinfds[0], stdinfds[1], stdoutfds[0], stdoutfds[1],
                     stderrfds[0], stderrfds[1]}) {
        // Standard descriptors are never closed after redirection.
        if (fd > STDERR_FILENO) add_action(::posix_spawn_file_actions_addclose(&actions, fd));
        if (spawn_error != 0) break;
      }
    }
    if (spawn_error == 0) {
      char* args[] = {const_cast<char*>(dotnet.c_str()), const_cast<char*>(dll.c_str()), nullptr};
#ifdef __APPLE__
      char* const* environment = *_NSGetEnviron();
#else
      char* const* environment = ::environ;
#endif
      spawn_error = ::posix_spawnp(&pid_, dotnet.c_str(), &actions, nullptr, args, environment);
    }
    ::posix_spawn_file_actions_destroy(&actions);
    if (spawn_error != 0) {
      close_created();
      pid_ = -1;
      throw err("posix_spawnp setup/start failed (error " + std::to_string(spawn_error) + ")");
    }
    ::close(stdinfds[0]); in_ = stdinfds[1];
    ::close(stdoutfds[1]); out_ = stdoutfds[0];
    ::close(stderrfds[1]); err_ = stderrfds[0];
    int flags = ::fcntl(err_, F_GETFL, 0);
    ::fcntl(err_, F_SETFL, flags | O_NONBLOCK);
    flags = ::fcntl(in_, F_GETFL, 0);
    ::fcntl(in_, F_SETFL, flags | O_NONBLOCK);
    flags = ::fcntl(out_, F_GETFL, 0);
    ::fcntl(out_, F_SETFL, flags | O_NONBLOCK);
#endif
  }

  Stats take_stats(bool reset) {
    Stats stats{stdout_peek_calls_, stdout_empty_polls_, stdout_read_calls_,
                stdout_read_bytes_, stdout_sleep_us_, timeouts_, cancellations_,
                poll_ms_, pipe_buffer_kib_};
    if (reset) {
      stdout_peek_calls_ = stdout_empty_polls_ = stdout_read_calls_ = 0;
      stdout_read_bytes_ = stdout_sleep_us_ = timeouts_ = cancellations_ = 0;
    }
    return stats;
  }

  Process(const Process&) = delete;
  Process& operator=(const Process&) = delete;
  ~Process() { close(); }

  bool alive() {
    if (closed_) return false;
#ifdef _WIN32
    return process_ && WaitForSingleObject(process_, 0) == WAIT_TIMEOUT;
#else
    if (pid_ <= 0) return false;
    int status = 0;
    pid_t r = ::waitpid(pid_, &status, WNOHANG);
    if (r == pid_) { pid_ = -1; return false; }
    return r == 0;
#endif
  }

  void send(std::string json, double timeout_seconds, const InterruptCheck& check = {}, bool newline = true) {
    if (closed_ || !alive()) throw err(".NET helper is not running");
    if (!std::isfinite(timeout_seconds) || timeout_seconds <= 0) throw err("invalid timeout");
    // JSON messages are short; for bulk uploads newline=false and the
    // potentially 128 MiB string is moved, not copied again.
    std::string line = newline ? json + "\n" : std::move(json);
    auto deadline = Clock::now() + std::chrono::milliseconds(static_cast<int64_t>(timeout_seconds * 1000));
#ifdef _WIN32
    auto job = new_job(in_, true);
    job->command = std::move(line);
    run_job(job, deadline, check);
#else
    size_t offset = 0;
    while (offset < line.size()) {
      if (check) check();
      if (Clock::now() > deadline) throw err("timeout writing JSON command");
      drain_stderr();
      int ms = static_cast<int>(std::max<int64_t>(1, std::min<int64_t>(50,
        std::chrono::duration_cast<std::chrono::milliseconds>(deadline - Clock::now()).count())));
      struct pollfd fd{in_, POLLOUT, 0};
      int ret = ::poll(&fd, 1, ms);
      if (ret < 0) { if (errno == EINTR) continue; throw err("poll failed while writing"); }
      if (!ret) continue;
#ifdef __APPLE__
      // F_SETNOSIGPIPE is set on in_ during construction, so no signal mask
      // or timed signal wait is needed on macOS.
      ssize_t n = ::write(in_, line.data() + offset, line.size() - offset);
      int write_errno = (n < 0) ? errno : 0;
#else
      // Linux: block SIGPIPE around write and consume only a newly generated
      // signal, preserving the caller's signal disposition.
      sigset_t mask, oldmask; sigemptyset(&mask); sigaddset(&mask, SIGPIPE);
      sigset_t pending_before; sigpending(&pending_before);
      ::sigprocmask(SIG_BLOCK, &mask, &oldmask);
      ssize_t n = ::write(in_, line.data() + offset, line.size() - offset);
      int write_errno = (n < 0) ? errno : 0;
      if (n < 0 && write_errno == EPIPE && !sigismember(&pending_before, SIGPIPE)) {
        struct timespec zero = {0,0}; ::sigtimedwait(&mask, nullptr, &zero);
      }
      ::sigprocmask(SIG_SETMASK, &oldmask, nullptr);
#endif
      if (n > 0) offset += static_cast<size_t>(n);
      else if (n == 0 || (write_errno != EINTR && write_errno != EAGAIN))
        throw err("write failed (helper closed stdin)");
    }
#endif
  }

  Frame receive(double timeout_seconds, const InterruptCheck& check = {}) {
    if (closed_ || !alive()) throw err(".NET helper is not running");
    if (!std::isfinite(timeout_seconds) || timeout_seconds <= 0) throw err("invalid receive timeout");
    auto deadline = Clock::now() + std::chrono::milliseconds(static_cast<int64_t>(timeout_seconds * 1000));
#ifdef _WIN32
    auto job = new_job(out_, false);
    run_job(job, deadline, check);
    return std::move(job->frame);
#else
    unsigned char header[5];
    read_exact(header, sizeof(header), deadline, check);
    if (header[0] != 'J' && header[0] != 'D')
      throw err("invalid frame tag " + std::to_string(static_cast<unsigned>(header[0])));
    uint32_t n = static_cast<uint32_t>(header[1]) |
      (static_cast<uint32_t>(header[2]) << 8) |
      (static_cast<uint32_t>(header[3]) << 16) |
      (static_cast<uint32_t>(header[4]) << 24);
    if (n > MAX_FRAME) throw err("oversized frame (" + std::to_string(n) + " bytes)");
    Frame frame{static_cast<char>(header[0]), std::vector<unsigned char>(n)};
    if (n) read_exact(frame.payload.data(), n, deadline, check);
    return frame;
#endif
  }

  void close() {
    if (closed_) return;
    closed_ = true;
#ifdef _WIN32
    if (process_ && WaitForSingleObject(process_, 0) == WAIT_TIMEOUT)
      TerminateProcess(process_, 1223);
    closeHandle(in_); closeHandle(out_); closeHandle(err_);
    if (process_) {
      WaitForSingleObject(process_, 1000);
      closeHandle(process_);
    }
#else
    closeFd(in_); closeFd(out_); closeFd(err_);
    if (pid_ > 0) {
      int status = 0;
      if (::waitpid(pid_, &status, WNOHANG) == 0) {
        ::kill(pid_, SIGTERM);
        for (int i=0; i<25; ++i) {
          if (::waitpid(pid_, &status, WNOHANG) == pid_) { pid_ = -1; break; }
          std::this_thread::sleep_for(std::chrono::milliseconds(10));
        }
        if (pid_ > 0) { ::kill(pid_, SIGKILL); ::waitpid(pid_, &status, 0); }
      }
      pid_ = -1;
    }
#endif
  }
};
} // namespace RSqlClientNET_native
#endif
