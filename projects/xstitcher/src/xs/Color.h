#ifndef __XSX_COLOR_H__
#define __XSX_COLOR_H__

#include "config.h"
#include <string>
#include <vigra/rgbvalue.hxx>
#include "RgbValue.h"

namespace xs {
// -------------------------------------------------------------------------- //
// Color
// -------------------------------------------------------------------------- //
  class Color {
  public:
    Color(): mRed(0), mGreen(0), mBlue(0) {}

    Color(unsigned char red, unsigned char green, unsigned char blue, const std::string& name, const std::string& description, wchar_t symbol = L'\0'):
      mRed(red), mGreen(green), mBlue(blue), mName(name), mDescription(description), mSymbol(symbol) {};

    unsigned char red() const {
      return mRed;
    }

    unsigned char green() const {
      return mGreen;
    }

    unsigned char blue() const {
      return mBlue;
    }

    unsigned char gray() const {
      return (static_cast<int>(mRed) + mGreen + mBlue) / 3; /* TODO: use correct formula? */
    }

    RgbValue rgb() const {
      return RgbValue(mRed, mGreen, mBlue);
    }

    const std::string& name() const {
      return mName;
    }

    const std::string& description() const {
      return mDescription;
    }

    wchar_t symbol() const {
      return mSymbol;
    }

    void setSymbol(wchar_t symbol) {
      mSymbol = symbol;
    }

  private:
    unsigned char mRed, mGreen, mBlue;
    std::string mName, mDescription;
    wchar_t mSymbol;
  };

} // namespace xs

#endif // __XSX_COLOR_H__
