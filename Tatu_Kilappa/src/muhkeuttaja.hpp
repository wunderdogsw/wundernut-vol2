#ifndef MUHKEUTTAJA_HPP
#define MUHKEUTTAJA_HPP

#include <string>
#include <vector>

/// There is no class, only an interface is provided. This is to allow the compiler to optimize everything
/// at all possible.
namespace muhkeuttaja
{
  /// Initialize muhkeutus.
  ///
  /// \param input_files Files to read before solving.
  void init(const std::vector<std::string> &input_files);

  /// Initialize muhkeutus using multiple threads.
  ///
  /// \param input_files Files to read before solving.
  void init_threaded(const std::vector<std::string> &input_files);

  /// Get formatted results.
  ///
  /// Will only return meaningful data after initialization and solving.
  ///
  /// \return Report as string.
  std::string results();

  /// Turn verbosity on or off.
  ///
  /// \param enabled True to turn on, false to turn off.
  void set_verbosity(const bool enabled);

  /// Solve using only one thread.
  void solve();

  /// Solve using multiple threads.
  void solve_threaded();
}

#endif
