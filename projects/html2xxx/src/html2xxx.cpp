#include "config.h"
#include <iostream>
#include <boost/program_options.hpp>
#include <QApplication>
#include <QtPlugin>
#include "h2x/Html2Xxx.h"

#ifdef STATIC_PLUGINS
Q_IMPORT_PLUGIN(qjpeg)
Q_IMPORT_PLUGIN(qgif)
Q_IMPORT_PLUGIN(qtiff)
Q_IMPORT_PLUGIN(qmng)
Q_IMPORT_PLUGIN(qico)
#endif

int main(int argc, char** argv) {
  using namespace boost::program_options;
  using namespace std;

  QApplication app(argc, argv);

  try {
    string inputFileName, outputFileName;
    int maxWidth;

    options_description desc("Allowed options");
    desc.add_options()
      ("help",                                                    "produce help message")
      ("input,i",      value<string>(&inputFileName),             "input file name")
      ("output,o",     value<string>(&outputFileName),            "output file name")
      ("width,w",      value<int>(&maxWidth),                     "maximal width of the output image");

    variables_map vm;
    store(command_line_parser(argc, argv).options(desc).run(), vm);
    notify(vm);

    if(vm.count("help") > 0 || vm.count("input") == 0 || vm.count("output") == 0 || vm.count("width") == 0) {
      cout << "html2xxx - html to image converter, version " << H2X_VERSION << "." << endl;
      cout << endl;
      cout << "USAGE:" << endl;
      cout << "  html2xxx [options]" << endl;
      cout << endl;
      cout << desc << endl;
      return 1;
    }

    h2x::Html2Xxx html2xxx(QString::fromStdString(inputFileName), QString::fromStdString(outputFileName), maxWidth);
    QObject::connect(&html2xxx, SIGNAL(finished()), &app, SLOT(quit()));
    app.exec();
  } catch (exception& e) {
    cerr << "error: " << e.what() << endl;
    return 1;
  }

  return 0;
}