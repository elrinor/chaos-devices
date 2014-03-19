#include "Palette.h"
#include <map>
#include <limits>
#include <QtXml>
#include <QFile>
#include <arx/Foreach.h>

namespace xs {
// -------------------------------------------------------------------------- //
// PaletteReader
// -------------------------------------------------------------------------- //
  class PaletteReader {
  public:
    PaletteReader(Palette& palette, const QDomDocument& doc): mPalette(palette), mDoc(doc) {}

    PaletteReader(Palette& palette, QString fileName): mPalette(palette) {
      QFile file(fileName);
      mDoc.setContent(&file);
    }

    void operator() () const {
      QDomElement node = mDoc.documentElement();
      while(node.nodeName() != "flossScheme") {
        node = node.nextSiblingElement();
        if(node.isNull())
          return;
      }

      for(node = node.firstChildElement(); !node.isNull(); node = node.nextSiblingElement()) {
        if(node.nodeName() != "floss")
          continue;

        QString name = node.firstChildElement("name").text();
        QString desc = node.firstChildElement("description").text();

        QDomElement colorNode = node.firstChildElement("color");
        QString r = colorNode.firstChildElement("red").text();
        QString g = colorNode.firstChildElement("green").text();
        QString b = colorNode.firstChildElement("blue").text();

        bool rOk = true, gOk = true, bOk = true;

        Color color(r.trimmed().toInt(&rOk), g.trimmed().toInt(&gOk), b.trimmed().toInt(&bOk), name.trimmed().toStdString(), desc.trimmed().toStdString());
        if(rOk && gOk && bOk) {
          mPalette.mColors.push_back(color);
          mPalette.mColorMap[color.rgb()] = color;
        }
      }
    }

  private:
    Palette& mPalette;
    QDomDocument mDoc;
  };


// -------------------------------------------------------------------------- //
// Palette
// -------------------------------------------------------------------------- //
  Palette::Palette(const std::string& fileName) {
    PaletteReader(*this, QString::fromLocal8Bit(fileName.c_str()))();
  }

  Palette::Palette(const std::wstring& fileName) {
    PaletteReader(*this, QString::fromStdWString(fileName))();
  }


  void mapToPalette(const vigra::BRGBImage& src, const Palette& palette, vigra::BRGBImage& dst) {
    dst.resize(src.size());

    /* Initialize color map. */
    std::map<RgbValue, RgbValue> mapping;
    for(int y = 0; y < dst.height(); y++)
      for(int x = 0; x < dst.width(); x++)
        mapping[dst(x, y)] = xs::RgbValue();

    /* Find closest colors from palette. */
    map_foreach(const xs::RgbValue& key, xs::RgbValue& value, mapping) {
      xs::RgbValue closest;
      int minDist = std::numeric_limits<int>::max();

      foreach(const xs::Color& color, palette.colors()) {
        int dist = (color.rgb() - key).magnitude();
        if(dist < minDist) {
          minDist = dist;
          closest = color.rgb();
        }
      }

      value = closest;
    }

    /* Apply mapping. */
    for(int y = 0; y < src.height(); y++)
      for(int x = 0; x < src.width(); x++)
        dst(x, y) = mapping[src(x, y)];
  }

} // namespace xs
