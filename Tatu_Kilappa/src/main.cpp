#include "muhkeuttaja.hpp"

#include <iostream>

#include <boost/exception/diagnostic_information.hpp>
#include <boost/program_options.hpp>

namespace po = boost::program_options;

/// Usage help string.
static const char g_usage[] =
"usage: muhkeuttaja [options] <input-file(s)>\n"
"\n"
"This program will coalculate maximum 'muhkeus' factor for an input text file.\n\n";

/// Main function.
///
/// \param argc Argument count.
/// \param argv Argument data.
/// \return Program exit code.
int main(int argc, char **argv)
{
  try
  {
    std::vector<std::string> input_files;
    std::string version(VERSION);
    bool help_printed = false;
    bool threaded = false;
    bool version_printed = false;

    po::options_description desc("Options");
    desc.add_options()
      ("help,h", "Print help text.")
      ("input-file,i", po::value<std::vector<std::string> >(), "Add an input file to process.")
      ("threaded,t", "Solve using multiple threads.")
      ("verbose,v", "Turn on verbose reporting.")
      ("version,V", "Print version string");

    po::positional_options_description pdesc;
    pdesc.add("input-file", -1);

    po::variables_map vmap;
    po::store(po::command_line_parser(argc, argv).options(desc).positional(pdesc).run(), vmap);
    po::notify(vmap);

    if(vmap.count("input-file"))
    {
      input_files = vmap["input-file"].as< std::vector<std::string> >();
    }
    if((1 >= argc) || vmap.count("help"))
    {
      std::cout << g_usage << desc;
      help_printed = true;
    }
    if(vmap.count("threaded"))
    {
      threaded = true;
    }
    if(vmap.count("verbose"))
    {
      muhkeuttaja::set_verbosity(true);
    }
    if(vmap.count("version"))
    {
      std::cout << VERSION << std::endl;
      version_printed = true;
    }

    if(input_files.empty())
    {
      if(!version_printed && !help_printed)
      {
        std::cout << g_usage << desc;
      }
      return 0;
    }


    if(threaded)
    {
      muhkeuttaja::init_threaded(input_files);
      muhkeuttaja::solve_threaded();
    }
    else
    {
      muhkeuttaja::init(input_files);
      muhkeuttaja::solve();
    }

    std::cout << muhkeuttaja::results() << std::endl;
  }
  catch(const boost::exception &err)
  {
    std::cerr << boost::diagnostic_information(err);
    return 1;
  }
  catch(const std::exception &err)
  {
    std::cerr << err.what() << std::endl;
    return -1;
  }

  return 0;
}

