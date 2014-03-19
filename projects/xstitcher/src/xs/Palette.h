#ifndef __XSX_PALETTE_H__
#define __XSX_PALETTE_H__

#include "config.h"
#include <string>
#include <vector>
#include <vigra/stdimage.hxx>
#include <arx/Collections.h>
#include "Color.h"

namespace xs {
// -------------------------------------------------------------------------- //
// Palette
// -------------------------------------------------------------------------- //
  class Palette {
  public:
    Palette() {}
    
    Palette(const std::string& fileName);
    Palette(const std::wstring& fileName);

    const std::vector<Color>& colors() const {
      return mColors;
    }

    const Color& color(int index) const {
      return mColors[index];
    }

    const Color& color(const RgbValue& rgb) const {
      return mColorMap[rgb];
    }

    void add(const Color& color) {
      if(!mColorMap.contains(color.rgb())) {
        mColors.push_back(color);
        mColorMap[color.rgb()] = color;
      }
    }

  private:
    friend class PaletteReader;

    std::vector<Color> mColors;
    arx::map<RgbValue, Color> mColorMap;
  };


  void mapToPalette(const vigra::BRGBImage& src, const Palette& palette, vigra::BRGBImage& dst);

  inline void mapToPalette(vigra::BRGBImage& image, const Palette& palette) {
    mapToPalette(image, palette, image);
  }

} // namespace xs

#endif // __XSX_PALETTE_H__
