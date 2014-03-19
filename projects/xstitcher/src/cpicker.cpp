#include "config.h"
#include <iostream>
#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>
#include <arx/ext/Vigra.h>
#include <arx/ext/Magick.h>

int main(int argc, char** argv) {
  using namespace boost::program_options;
  using namespace std;

  try {
    vigra::Size2D position;
    bool showHelp;
    string inputFileName, format;

    options_description desc("Allowed options");
    desc.add_options()
      ("help",     bool_switch(&showHelp),                                     "Produce help message.")
      ("input,i",  value<string>(&inputFileName)->required(),                  "Input file name.")
      ("pixel,p",  value<vigra::Size2D>()->default_value(vigra::Size2D(0, 0), "0:0"),
                                                                               "Pixel position, in format x:y.")
      ("format,f", value<string>(&format)->default_value("$red $green $blue"), "Output format.");

    try {
      variables_map vm;
      store(command_line_parser(argc, argv).options(desc).run(), vm);
      notify(vm);
    } catch (required_option&) {
      showHelp = true;
    }

    if(showHelp) {
      cout << "cpicker - color picker, version " << XSTITCHER_VERSION << "." << endl;
      cout << endl;
      cout << "USAGE:" << endl;
      cout << "  cpicker [options]" << endl;
      cout << endl;
      cout << desc << endl;
      return 1;
    }

    vigra::BRGBImage image;
    importImage(image, inputFileName);
    vigra::BRGBImage::value_type s = image(position.width(), position.height());

    std::string out = format;
    boost::replace_all(out, "$red",   boost::lexical_cast<string>(static_cast<int>(s.red())));
    boost::replace_all(out, "$green", boost::lexical_cast<string>(static_cast<int>(s.green())));
    boost::replace_all(out, "$blue",  boost::lexical_cast<string>(static_cast<int>(s.blue())));
    cout << out;
  } catch(exception& e) {
    cerr << "error: " << e.what() << endl;
    return 1;
  } catch(...) {
    cerr << "error: Exception of unknown type." << endl;
    return 1;
  }

  return 0;
}