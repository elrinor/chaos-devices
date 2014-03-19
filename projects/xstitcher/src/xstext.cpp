#include "config.h"
#include <boost/program_options.hpp>
#include <boost/algorithm/string.hpp>
#include <QFile>
#include <QFont>
#include <QTextCodec>
#include <QImage>
#include <QPainter>
#include <QApplication>
#include <arx/Foreach.h>
#include <arx/ext/Vigra.h>
#include <arx/ext/Magick.h>
#include "xs/RgbValue.h"


namespace boost {
  
  /** Boost program_options validation function for Qt::AlignmentFlag. */
  inline void validate(boost::any& v, const std::vector<std::string>& values, Qt::AlignmentFlag* /* target_type */, int) {
    using namespace boost::program_options;
    using namespace boost::spirit::qi;

    /* Check that it's the only one. */
    validators::check_first_occurrence(v);
    const std::string& s = boost::algorithm::to_lower_copy(validators::get_single_string(values));

    /* Match. */
    if(s == "left") {
      v = Qt::AlignLeft;
    } else if(s == "right") {
      v = Qt::AlignRight;
    } else if(s == "center") {
      v = Qt::AlignHCenter;
    } else {
      throw invalid_option_value(s);
    }
  }

} // namespace boost

vigra::Size2D textSize(const QString& text, const QFont& font) {
  QImage image(1, 1, QImage::Format_ARGB32);
  QPainter painter;

  QRect rect(0, 0, image.width(), image.height());
  painter.begin(&image);
  painter.setFont(font);
  painter.drawText(rect, 0, text, &rect);
  painter.end();

  return vigra::Size2D(rect.width(), rect.height());
}

int main(int argc, char** argv) {
  using namespace boost::program_options;
  using namespace std;

  QApplication app(argc, argv);

  try {
    string textFileName, encoding, outputFileName, fontName;
    bool showHelp, italic = false;
    int fontSize;
    xs::RgbValue color, background;
    Qt::AlignmentFlag alignment;
    vigra::Size2D imageSize;

    options_description desc("Allowed options");
    desc.add_options()
      ("help",                    bool_switch(&showHelp),                                   "Produce help message.")
      ("textfile,t",              value<string>(&textFileName)->required(),                 "File name to read text from, or \"-\" to read from stdin.")
      ("encoding,e",              value<string>(&encoding)->default_value(QTextCodec::codecForLocale()->name().data()),                                     
                                                                                            "Input file encoding.")
      ("font,f",                  value<string>(&fontName)->required(),                     "Font name.")
      ("font-size,s",             value<int>(&fontSize)->required(),                        "Font size, in pixels. Use 0 to determine font size automatically to fit into provided output image size.")
      ("image-size,z",            value<vigra::Size2D>(&imageSize)->required(),             "Output image size, in format w:h. Use 0:0 to determine output image size automatically using font size.")
      //("italic,i",                bool_switch(&italic),                                     "Use italic font.")
      ("align,a",                 value<Qt::AlignmentFlag>(&alignment)->default_value(Qt::AlignHCenter, "center"), 
                                                                                            "Text alignment, either left, right or center.")
      ("color,c",                 value<xs::RgbValue>(&color)->required(),                  "Text color, in format r:g:b.")
      ("background,b",            value<xs::RgbValue>(&background)->required(),             "Background color, in format r:g:b.")
      ("output,o",                value<string>(&outputFileName)->default_value("out.png"), "Output file name.");

    try {
      variables_map vm;
      store(command_line_parser(argc, argv).options(desc).run(), vm);
      notify(vm);
    } catch (required_option&) {
      showHelp = true;
    }

    /* Output help & exit if needed. */
    if(showHelp) {
      cout << "xstext, version " << XSTITCHER_VERSION << "." << endl;
      cout << endl;
      cout << "USAGE:" << endl;
      cout << "  xspng [options]" << endl;
      cout << endl;
      cout << desc << endl;
      return 1;
    }

    /* Check font size. */
    if(fontSize < 0)
      throw logic_error("Font size must be positive.");

    /* Check that font size and image size are consistent. */
    if(fontSize == 0 && (imageSize.width() == 0 || imageSize.height() == 0))
      throw logic_error("Either font size or output image size must be non-zero");

    /* Get text codec. */
    QTextCodec* textCodec = QTextCodec::codecForName(encoding.c_str());
    if(textCodec == NULL) {
      cout << "Codec for encoding \"" + encoding + "\" not found." << endl;
      cout << endl;
      cout << "Available encodings:" << endl;
      foreach(const QByteArray& codecName, QTextCodec::availableCodecs())
        cout << "  " << codecName.data() << endl;
      return 1;
    }

    /* Read text */
    QFile textFile(QString::fromLocal8Bit(textFileName.c_str()));
    bool isOpen;
    if(textFileName == "-")
      isOpen = textFile.open(QIODevice::ReadOnly, 0);
    else 
      isOpen = textFile.open(QIODevice::ReadOnly);
    if(!isOpen)
      throw logic_error("Cannot open file \"" + textFileName + "\" for reading.");
    
    QTextDecoder* textDecoder = textCodec->makeDecoder();
    QString text = textDecoder->toUnicode(textFile.readAll());
    /* No need to delete textDecoder here. */

    /* Estimate font size. */
    if(fontSize == 0) {
      for(fontSize = 1; fontSize < 100; fontSize++) {
        vigra::Size2D size = textSize(text, QFont(QString::fromLocal8Bit(fontName.c_str()), fontSize, -1, italic));
        if(size.width() > imageSize.width() || size.height() > imageSize.height()) {
          fontSize--;
          break;
        }
      }
    }
    if(fontSize == 0)
      throw logic_error("Cannot fit given text into given output image size.");

    /* Load font */
    QFont font(QString::fromLocal8Bit(fontName.c_str()), fontSize, -1, italic);
    font.setStyleStrategy(QFont::NoAntialias);

    /* Get bounding rect for text if needed. */ 
    if(imageSize.width() == 0 || imageSize.height() == 0)
      imageSize = textSize(text, font);

    /* Draw! */
    QImage image(imageSize.width(), imageSize.height(), QImage::Format_ARGB32);
    image.fill(QColor(Qt::white).rgb());

    QPainter painter;
    painter.begin(&image);
    painter.setFont(font);
    painter.setPen(QPen(QColor(Qt::black)));
    painter.drawText(0, 0, image.width(), image.height(), alignment, text);
    painter.end();
    image = image.convertToFormat(QImage::Format_Mono, Qt::ThresholdDither);

    /* Output. */
    vigra::BRGBImage outImage(imageSize);
    for(int y = 0; y < image.height(); y++)
      for(int x = 0; x < image.width(); x++)
        outImage(x, y) = image.pixel(x, y) == QColor(0, 0, 0).rgb() ? color : background;

    /* Export result. */
    exportImage(outImage, outputFileName);
  } catch(exception& e) {
    cerr << "error: " << e.what() << endl;
    return 1;
  } catch(...) {
    cerr << "error: Exception of unknown type." << endl;
    return 1;
  }

  return 0;
}