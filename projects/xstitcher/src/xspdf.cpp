#include "config.h"
#include <string>
#include <iostream>
#include <boost/program_options.hpp>
#include <QApplication>
#include <arx/ext/Vigra.h>
#include <arx/ext/Magick.h>
#include <arx/ext/qt/FormPrinter.h>
#include "xs/Palette.h"
#include "xs/FrontPageGenerator.h"
#include "xs/ColorPageGenerator.h"

#ifdef XSX_MEASURE_TIMINGS
#  include <boost/timer.hpp>
#endif

int main(int argc, char** argv) {
  using namespace boost::program_options;
  using namespace std;

  QApplication app(argc, argv, false);

  try {
    bool showHelp;
    std::string inputFileName, paletteFileName, outputFileName;
    std::string name;

    options_description desc("Allowed options");
    desc.add_options()
      ("help",                    bool_switch(&showHelp),                                   "Produce help message.")
      ("input,i",                 value<string>(&inputFileName)->required(),                "Input file name.")
      ("name,n",                  value<string>(&name)->required(),                         "Scheme name.")
      ("palette,p",               value<string>(&paletteFileName)->required(),              "Palette file name.")
      ("output,o",                value<string>(&outputFileName)->default_value("out.pdf"), "Output file name.");

    try {
      variables_map vm;
      store(command_line_parser(argc, argv).options(desc).run(), vm);
      notify(vm);
    } catch (required_option&) {
      showHelp = true;
    }

    /* Output help & exit if needed. */
    if(showHelp) {
      cout << "xspdf, version " << XSTITCHER_VERSION << "." << endl;
      cout << endl;
      cout << "USAGE:" << endl;
      cout << "  xspdf [options]" << endl;
      cout << endl;
      cout << desc << endl;
      return 1;
    }

    /* Import Image */
    vigra::BRGBImage rgbImage;
    importImage(rgbImage, inputFileName);

    /* Map to palette. */
    xs::Palette palette(paletteFileName);
    mapToPalette(rgbImage, palette);

    /* Create a set of all used colors. */
    std::set<xs::RgbValue> colorSet;
    for(int y = 0; y < rgbImage.height(); y++)
      for(int x = 0; x < rgbImage.width(); x++)
        colorSet.insert(rgbImage(x, y));

    /* Create palette signs. */
    wchar_t colorChars[] = L"abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
    int colorIndex = 0;
    xs::Palette imagePalette;
    foreach(const xs::RgbValue& value, colorSet) {
      xs::Color color = palette.color(value);
      color.setSymbol(colorChars[colorIndex++]);
      imagePalette.add(color);
    }

    XSX_TIMING(boost::timer t);

    /* Output. */
    QPrinter printer;
    printer.setPaperSize(QPrinter::A4);
    printer.setOutputFormat(QPrinter::PdfFormat);
    printer.setOutputFileName(QString::fromLocal8Bit(outputFileName.c_str()));
    printer.setOrientation(QPrinter::Portrait);
    printer.setPageMargins(0, 0, 0, 0, QPrinter::Millimeter);

    arx::FormPrinter formPrinter(printer);
    formPrinter
      (new xs::FrontPageGenerator(rgbImage, imagePalette, QString::fromLocal8Bit(name.c_str())))
      (new xs::ColorPageGenerator(rgbImage, imagePalette, QString::fromLocal8Bit(name.c_str())))
      ();

    XSX_TIMING(cout << t.elapsed() << " secs spent on pdf generation." << endl);
  } catch(exception& e) {
    cerr << "error: " << e.what() << endl;
    return 1;
  } catch(...) {
    cerr << "error: Exception of unknown type." << endl;
    return 1;
  }

  return 0;
}
