#include "muhkeuttaja.hpp"

#include <cstdio>
#include <iostream>
#include <map>
#include <set>
#include <sstream>

#include <boost/filesystem.hpp>
#include <boost/foreach.hpp>
#include <boost/thread.hpp>

#if defined(WIN32)
#include <Windows.h>
#else
#include <ctime>
#endif

namespace fs = boost::filesystem;
using namespace muhkeuttaja;

/// Letter identifier.
static const uint8_t LETTER_ARING = 2;

/// Letter identifier.
static const uint8_t LETTER_ADOTS = 1;

/// Letter identifier.
static const uint8_t LETTER_ODOTS = 0;

/// Maximum letter count.
static const unsigned LETTER_COUNT = 29;

/// Verbosity.
static bool g_verbose = false;

/// Tell if verbosity is on.
///
/// \return True if yes, false if no.
static bool is_verbose()
{
  return g_verbose;
}

/// Get timestamp in nanoseconds.
///
/// Probably nowhere near actual nanosecond granularity.
///
/// \return Nanosecond timestamp since some unspecified staring point.
uint64_t get_timestamp_ns()
{
#if defined(WIN32)
  return static_cast<uint64_t>(timeGetTime()) * 1000000;
#else
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return static_cast<uint64_t>(ts.tv_sec) * 1000000000 + static_cast<uint64_t>(ts.tv_nsec);
#endif
}

/// Get duration as human-readable value.
///
/// \param op Duration input.
/// \return String representing duration (human-readable).
static std::string get_human_readable_duration(uint64_t op)
{
  std::ostringstream sstr;
  sstr << static_cast<int>(static_cast<double>(op) / 1000000.0 + 0.5) << "ms";
  return sstr.str();
}

/// Hamming weight.
///
/// See: http://stackoverflow.com/questions/109023/how-to-count-the-number-of-set-bits-in-a-32-bit-integer
///
/// \param op Operand.
/// \return Hamming weight for integer operand.
static unsigned calculate_hamming_weight(uint32_t op)
{
  op -= (op >> 1) & 0x55555555;
  op = (op & 0x33333333) + ((op >> 2) & 0x33333333);
  return (((op + (op >> 4)) & 0x0F0F0F0F)* 0x01010101) >> 24;
}

/// Cache-friendly word set (as opposed to using just 'set').
class WordSet
{
  private:
    /// Words in a vector.
    std::vector<char> m_words;

  public:
    /// Insert a word.
    ///
    /// Does nothing if word already exist in the set.
    ///
    /// \param word Word vector to insert.
    void insert(const std::vector<char> &word)
    {
      const size_t input_length = word.size();
      const size_t existing_length = m_words.size();

      for(size_t ii = 0, jj = 0; (existing_length > jj);)
      {
        if(word[ii++] == m_words[jj++])
        {
          if(ii == input_length)
          {
            if(!m_words[jj])
            {
              return;
            }
          }
          else if(m_words[jj])
          {
            continue;
          }
        }
        ii = 0;
        while(m_words[jj++]);
      }

      for(size_t ii = 0; (input_length > ii); ++ii)
      {
        m_words.push_back(word[ii]);
      }
      m_words.push_back(0);
    }

  public:
    /// Print word set to an output stream.
    ///
    /// \param lhs Stream to print to.
    /// \param rhs Word set to print.
    /// \return Stream after printing.
    friend std::ostream& operator<<(std::ostream &lhs, const WordSet &rhs)
    {
      if(rhs.m_words.empty())
      {
        return lhs;
      }

      lhs << '\'';

      for(size_t ii = 0, ee = rhs.m_words.size(); (ii < ee); ++ii)
      {
        char cc = rhs.m_words[ii];

        if(cc)
        {
          lhs << cc;
        }
        else
        {
          lhs << '\'';

          if(ii + 1 < ee)
          {
            lhs << ", '";
          }
        }
      }

      return lhs;
    }
};

/// One word.
class Word
{
  private:
    /// Muhkeus.
    unsigned m_muhkeus;

    /// Individual letters.
    uint32_t m_letters;

    /// Individual words matching this muhkeus.
    WordSet m_words;

  public:
    /// Append an existing word.
    ///
    /// \param word Word to insert.
    void append(const std::vector<char> &word)
    {
      m_words.insert(word);
    }

    /// Calculate combined muhkeus (with another word).
    ///
    /// \param rhs Another word.
    unsigned calculateCombinedMuhkeus(const Word &rhs) const
    {
      return calculate_hamming_weight(m_letters | rhs.m_letters);
    }

    /// Get muhkeus of this.
    ///
    /// \return Muhkeus of this.
    unsigned getMuhkeus() const
    {
      return m_muhkeus;
    }

  public:
    /// Empty constructor.
    Word() { }

    /// Constructor.
    ///
    /// \param muhkeus Muhkeus of this
    /// \param letters Individual letters.
    /// \param word The word that makes up this.
    Word(unsigned muhkeus, uint32_t letters, const std::vector<char> &word) :
      m_muhkeus(muhkeus),
      m_letters(letters)
    {
      this->append(word);
    }

  public:
    /// Print word to an output stream.
    ///
    /// \param lhs Stream to print to.
    /// \param rhs Word to print.
    /// \return Stream after printing.
    friend std::ostream& operator<<(std::ostream &lhs, const Word &rhs)
    {
      lhs << rhs.m_muhkeus << ": ";

      for(unsigned ii = 0; (LETTER_COUNT > ii); ++ii)
      {
        lhs << ((rhs.m_letters & (1 << ii)) ? '1' : '0');
      }

      return lhs << ": " << rhs.m_words;
    }
};

/// File abstraction.
///
/// Performs multi-threaded reading so processing thread never needs to stop.
class TextReader
{
  public:
    /// Buffer size for reading operations.
    static const size_t BUFFER_SIZE = 8192;

  private:
    /// Mutex for protecting the reads.
    boost::mutex m_mutex;

    /// Condition variable for signalling, just in case.
    boost::condition_variable m_cond;

    /// Thread doing the background reading.
    boost::thread m_thread;

    /// First buffer.
    uint8_t *m_buffer_front;

    /// Second buffer.
    uint8_t *m_buffer_back;

    /// First buffer iterator.
    uint8_t *m_buffer_front_iter;

    /// First buffer end pointer.
    uint8_t *m_buffer_front_end;

    /// Second buffer end pointer.
    uint8_t *m_buffer_back_end;

    /// File descriptor.
    FILE* m_fd;

    /// Is the thread (still) active?
    bool m_thread_active;

  private:
    /// Fill one buffer.
    ///
    /// \return True if somethin to read still exists, false if not.
    bool fillBuffer()
    {
      size_t bytes_read = fread(m_buffer_back, sizeof(char), BUFFER_SIZE, m_fd);

      if(0 == bytes_read)
      {
        m_buffer_back_end = NULL;
      }
      else
      {
        m_buffer_back_end = m_buffer_back + bytes_read;
      }

      return (bytes_read == BUFFER_SIZE);
    }

  public:
    /// Get one character.
    ///
    /// \return Read character.
    int getChar()
    {
      if(m_buffer_front_iter >= m_buffer_front_end)
      {
        boost::mutex::scoped_lock sl(m_mutex);

        while(m_thread_active && !m_buffer_back_end)
        {
          m_cond.wait(sl);
        }

        std::swap(m_buffer_front, m_buffer_back);
        m_buffer_front_iter = m_buffer_front;
        m_buffer_front_end = m_buffer_back_end;
        m_buffer_back_end = NULL;

        if(!m_buffer_front_end)
        {
          return EOF;
        }

        m_cond.notify_one();
      }
      else
      {
        //std::cout << "gonna read char " << static_cast<int>(*m_buffer_front_iter) << "\n";
      }

      return static_cast<int>(*m_buffer_front_iter++);
    }

    /// Reader thread function.
    void readerThread()
    {
      boost::mutex::scoped_lock sl(m_mutex);

      for(;;)
      {
        if(m_buffer_back_end)
        {
          m_cond.notify_one();
          m_cond.wait(sl);
          continue;
        }

        if(!fillBuffer())
        {
          m_cond.notify_one();
          m_thread_active = false;
          return;
        }
      }
    }

  public:
    /// Constructor.
    ///
    /// Implicitly opens file for reading.
    ///
    /// \param filename Filename to open.
    TextReader(const std::string &filename) :
      m_buffer_front(new uint8_t[BUFFER_SIZE]),
      m_buffer_back(new uint8_t[BUFFER_SIZE]),
      m_buffer_front_iter(NULL),
      m_buffer_front_end(NULL),
      m_buffer_back_end(NULL),
      m_fd(fopen(filename.c_str(), "r")),
      m_thread_active(false)
    {
      if(!m_fd)
      {
        std::ostringstream sstr;
        sstr << "failed to open file " << fs::path(filename) << ": " << errno;
        BOOST_THROW_EXCEPTION(std::runtime_error(sstr.str()));
      }

      if(fillBuffer())
      {
        m_thread_active = true;
        m_thread = boost::thread(boost::bind(&TextReader::readerThread, this));
      }
    }

    /// Destructor.
    ~TextReader()
    {
      m_thread.join();

      if(m_fd)
      {
        fclose(m_fd);
      }
    }
};

/// Word map.
typedef std::map<uint64_t, Word, std::greater<uint64_t> > WordMap;

/// Result map.
typedef std::set<std::pair<const Word*, const Word*> > ResultSet;

/// Word reader.
///
/// Abstraction for reading a word from a source.
class WordReader
{
  private:
    /// Initial storage size for word. Should be enough.
    static const size_t INITIAL_WORD_SIZE = 64;

  private:
    /// Current muhkeus.
    unsigned m_muhkeus;

    /// Current letter mask.
    uint32_t m_letters;

    /// Word storage.
    std::vector<char> m_word;

  private:
    /// Clear state (after insertion).
    void clear()
    {
      m_muhkeus = 0;
      m_letters = 0;
      m_word.clear();
    }

    /// Flag a letter mask on.
    ///
    /// \param mask Mask to flag.
    void flag(const int shift)
    {
      const uint32_t mask = 1 << shift;

      if(m_letters & mask)
      {
        return;
      }

      m_letters |= mask;
      ++m_muhkeus;
    }

    /// Generate key.
    ///
    /// \return Key generated from this.
    uint64_t generateKey() const
    {
      return (static_cast<uint64_t>(m_muhkeus) << LETTER_COUNT) | static_cast<uint64_t>(m_letters);
    }

  public:
    /// Incorporate one ASCII character.
    ///
    /// \param cc Character read.
    /// \return True if word encountered a non-letter and word may have formed.
    bool incorporateAscii(const int cc)
    {
      if(('a' <= cc) && ('z' >= cc))
      {
        flag('z' - cc + 3);
        m_word.push_back(static_cast<char>(cc));
      }
      else if(('A' <= cc) && ('Z' >= cc))
      {
        flag('Z' - cc + 3);
        m_word.push_back(static_cast<char>(('a' - 'A') + cc));
      }
      else
      {
        return true;
      }
      return false;
    }

    /// Incorporate one UTF8-latin code point 195 character.
    ///
    /// \param cc Character read.
    /// \return True if word encountered a non-letter and word may have formed.
    bool incorporateUtf8Latin195(const int cc)
    {
      if((132 == cc) || (164 == cc))
      {
        flag(LETTER_ADOTS);
        m_word.push_back(static_cast<char>(195U));
        m_word.push_back(static_cast<char>(164U));
      }
      else if((150 == cc) || (182 == cc))
      {
        flag(LETTER_ODOTS);
        m_word.push_back(static_cast<char>(195U));
        m_word.push_back(static_cast<char>(182U));
      }
      else if((133 == cc) || (165 == cc))
      {
        flag(LETTER_ARING);
        m_word.push_back(static_cast<char>(195U));
        m_word.push_back(static_cast<char>(165U));
      }
      else
      {
        return true;
      }
      return false;
    }

    /// Tell if this is insertable.
    ///
    /// \return True if yes, false if no.
    bool isInsertable() const
    {
      return (0 < m_muhkeus);
    }

    /// Insert this word into a word map.
    ///
    /// \param word_map Word map to insert to.
    void insert(WordMap &word_map)
    {
      uint64_t key = generateKey();
      WordMap::iterator iter = word_map.find(key);
      if(word_map.end() != iter)
      {
        iter->second.append(m_word);
      }
      else
      {
        word_map[key] = Word(m_muhkeus, m_letters, m_word);
      }
      clear();
    }

  public:
    /// Constructor.
    WordReader() :
      m_muhkeus(0),
      m_letters(0)
    {
      // Herb Sutter said vectors are contiguous.
      m_word.reserve(INITIAL_WORD_SIZE);
    }
};

/// Duration of init (ns).
static uint64_t g_duration_init = 0;

/// Duration of solve (ns).
static uint64_t g_duration_solve = 0;

/// Highest muhkeus pair found.
static unsigned g_highest_muhkeus = 0;

/// Set of all best pairs.
ResultSet g_highest_pairs;

/// Global end iterator for solvers.
static WordMap::const_iterator g_iter_end;

/// Global Iterator for tracking current threaded progress.
static WordMap::const_iterator g_iter_main;

/// Mutex guarding the solving.
static boost::mutex g_mutex;

/// Threads used for solving.
static unsigned g_threads_used = 0;

/// All different words found.
static WordMap g_words;

void muhkeuttaja::init(const std::vector<std::string> &input_files)
{
  uint64_t timestamp_begin = get_timestamp_ns();

  BOOST_FOREACH(const std::string &vv, input_files)
  {
    FILE *fd = fopen(vv.c_str(), "r");

    if(!fd)
    {
      std::ostringstream sstr;
      sstr << "failed to open file " << fs::path(vv) << ": " << errno;
      BOOST_THROW_EXCEPTION(std::runtime_error(sstr.str()));
    }    
    else if(is_verbose())
    {
      std::cout << "Reading file: " << fs::path(vv) << std::endl;
    }

    WordReader word;

    for(;;)
    {
      int cc = fgetc(fd);

      if(195 == cc)
      {
        cc = fgetc(fd);

        if(word.incorporateUtf8Latin195(cc))
        {
          if(word.isInsertable())
          {
            word.insert(g_words);
          }

          if(EOF == cc)
          {
            break;
          }
        }
      }
      else if(word.incorporateAscii(cc))
      {
        if(word.isInsertable())
        {
          word.insert(g_words);
        }

        if(EOF == cc)
        {
          break;
        }
      }
    }

    fclose(fd);
  }

  g_duration_init = get_timestamp_ns() - timestamp_begin;
}

void muhkeuttaja::init_threaded(const std::vector<std::string> &input_files)
{
  uint64_t timestamp_begin = get_timestamp_ns();

  BOOST_FOREACH(const std::string &vv, input_files)
  {
    TextReader text_file(vv);

    if(is_verbose())
    {
      std::cout << "Reading file: " << fs::path(vv) << std::endl;
    }

    WordReader word;

    for(;;)
    {
      int cc = text_file.getChar();

      if(cc == 195)
      {
        cc = text_file.getChar();

        if(word.incorporateUtf8Latin195(cc))
        {
          if(word.isInsertable())
          {
            word.insert(g_words);
          }

          if(EOF == cc)
          {
            break;
          }
        }
      }
      else if(word.incorporateAscii(cc))
      {
        if(word.isInsertable())
        {
          word.insert(g_words);
        }

        if(EOF == cc)
        {
          break;
        }
      }
    }
  }

  g_duration_init = get_timestamp_ns() - timestamp_begin;
}

std::string muhkeuttaja::results()
{
  std::ostringstream sstr;

  sstr << g_words.size() << " individual muhkeus values found, highest value of pair: " << g_highest_muhkeus <<
    "\n----\n";

  BOOST_FOREACH(const ResultSet::value_type &vv, g_highest_pairs)
  {
    sstr << *(vv.first) << std::endl << *(vv.second) << "\n----\n";
  }

  sstr << get_human_readable_duration(g_duration_init) << " for loading input files, " <<
    get_human_readable_duration(g_duration_solve) << " for " << g_threads_used << "-threaded solve";

  return sstr.str();
}

void muhkeuttaja::set_verbosity(const bool enabled)
{
  g_verbose = enabled;
}

void muhkeuttaja::solve()
{
  if(g_words.empty())
  {
    return;
  }

  uint64_t timestamp_begin = get_timestamp_ns();
  WordMap::const_iterator ii = g_words.begin();
  WordMap::const_iterator ee = g_words.end();

  for(;;)
  {
    WordMap::const_iterator jj = ii;
    ++jj;

    if(jj == ee)
    {
      break;
    }

    const Word &comparator = ii->second;
    unsigned comparator_muhkeus = comparator.getMuhkeus();

    if(comparator_muhkeus + jj->second.getMuhkeus() < g_highest_muhkeus)
    {
      break;
    }

    do {
      unsigned combined_muhkeus = comparator.calculateCombinedMuhkeus(jj->second);

      if(combined_muhkeus > g_highest_muhkeus)
      {
        g_highest_muhkeus = combined_muhkeus;
        g_highest_pairs.clear();
        g_highest_pairs.insert(ResultSet::value_type(&comparator, &(jj->second)));
      }
      else if(combined_muhkeus == g_highest_muhkeus)
      {
        g_highest_pairs.insert(ResultSet::value_type(&comparator, &(jj->second)));
      }

      ++jj;
    } while((jj != ee) && (comparator_muhkeus + jj->second.getMuhkeus() >= g_highest_muhkeus));

    ++ii;
  }

  g_duration_solve = get_timestamp_ns() - timestamp_begin;
  g_threads_used = 1;
}

/// Singular solver thread.
void solver_thread()
{
  for(;;)
  {
    g_mutex.lock();

    WordMap::const_iterator ii = g_iter_main;
    if(ii == g_iter_end)
    {
      g_mutex.unlock();
      return;
    }

    unsigned highest_muhkeus = g_highest_muhkeus;
    WordMap::const_iterator jj = ++g_iter_main;

    g_mutex.unlock();

    if(jj == g_iter_end)
    {
      return;
    }

    const Word &comparator = ii->second;
    unsigned comparator_muhkeus = comparator.getMuhkeus();

    if(comparator_muhkeus + jj->second.getMuhkeus() < highest_muhkeus)
    {
      return;
    }

    do {
      unsigned combined_muhkeus = comparator.calculateCombinedMuhkeus(jj->second);

      if(combined_muhkeus > highest_muhkeus)
      {
        g_mutex.lock();

        highest_muhkeus = g_highest_muhkeus;
        if(combined_muhkeus > highest_muhkeus)
        {
          highest_muhkeus = combined_muhkeus;
          g_highest_muhkeus = combined_muhkeus;
          g_highest_pairs.clear();
          g_highest_pairs.insert(ResultSet::value_type(&comparator, &(jj->second)));
        }
        else if(combined_muhkeus == highest_muhkeus)
        {
          g_highest_pairs.insert(ResultSet::value_type(&comparator, &(jj->second)));
        }

        g_mutex.unlock();
      }
      else if(combined_muhkeus == highest_muhkeus)
      {
        g_mutex.lock();

        highest_muhkeus = g_highest_muhkeus;
        if(combined_muhkeus == highest_muhkeus)
        {
          g_highest_pairs.insert(ResultSet::value_type(&comparator, &(jj->second)));
        }

        g_mutex.unlock();
      }

      ++jj;
    } while((jj != g_iter_end) && (comparator_muhkeus + jj->second.getMuhkeus() >= g_highest_muhkeus));
  }
}

void muhkeuttaja::solve_threaded()
{
  uint64_t timestamp_begin = get_timestamp_ns();

  g_iter_main = g_words.begin();
  g_iter_end = g_words.end();
  g_threads_used = boost::thread::hardware_concurrency();

  boost::thread *threads = new boost::thread[g_threads_used];
  for(unsigned ii = 0; (ii < g_threads_used); ++ii)
  {
    threads[ii] = boost::thread(solver_thread);
  }

  for(unsigned ii = 0; (ii < g_threads_used); ++ii)
  {
    threads[ii].join();
  }
  delete[] threads;

  g_duration_solve = get_timestamp_ns() - timestamp_begin;
}

