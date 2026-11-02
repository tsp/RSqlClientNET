#include "../../src/native_process.hpp"
#include <chrono>
#include <iostream>
#include <stdexcept>
#include <string>
using RSqlClientNET_native::Process;

int main(int argc, char** argv) {
  if (argc < 2) return 2;
  std::string helper = argv[1];
  {
    Process p("python3", helper);
    p.send("{\"op\":\"ping\"}", 5);
    auto f = p.receive(5);
    if (f.kind != 'J' || f.payload.size() < 10) return 3;
    p.send("{\"op\":\"large\"}", 5);
    auto large = p.receive(5);
    if (large.kind != 'D' || large.payload.size() != 1048576 || large.payload[0] != 'x') return 4;
    if (!p.alive()) return 5;
    p.close();
  }
  {
    Process p("python3", helper);
    p.send("{\"op\":\"hang\"}", 5);
    auto start = std::chrono::steady_clock::now();
    bool timed_out = false;
    try { p.receive(0.2); } catch (const std::exception& e) { timed_out = true; }
    auto delta = std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
    if (!timed_out || delta > 1.5) { std::cerr << "timeout failure " << delta << '\n'; return 6; }
    p.close();
  }
  {
    Process p("python3", helper);
    p.send("{\"op\":\"partial\"}", 5);
    auto start = std::chrono::steady_clock::now();
    bool timed_out = false;
    try { p.receive(0.2); } catch (const std::exception&) { timed_out = true; }
    auto delta = std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
    if (!timed_out || delta > 1.5) { std::cerr << "partial failure " << delta << '\n'; return 7; }
    p.close();
  }
  {
    Process p("python3", helper);
    p.send("{\"op\":\"hang\"}", 5);
    auto start = std::chrono::steady_clock::now();
    bool cancelled = false;
    try { p.receive(10, []() { throw std::runtime_error("simulated user interrupt"); }); }
    catch (const std::runtime_error& e) { cancelled = std::string(e.what()) == "simulated user interrupt"; }
    auto delta = std::chrono::duration<double>(std::chrono::steady_clock::now() - start).count();
    if (!cancelled || delta > 1.5) { std::cerr << "interrupt failure " << delta << '\n'; return 8; }
    p.close();
  }
  {
    Process p("python3", helper);
    p.send("{\"op\":\"bad\"}", 5);
    bool bad = false;
    try { p.receive(5); } catch (const std::runtime_error&) { bad = true; }
    if (!bad) return 9;
    p.close();
  }
  std::cout << "PASS: normal, 1MiB, timeout, partial frame, interrupt, bad header\n";
}
