#ifndef __XSX_COLOR_PAGE_GENERATOR_H__
#define __XSX_COLOR_PAGE_GENERATOR_H__

#include "config.h"
#include <algorithm> /* for std::min() */
#include "PageGeneratorBase.h"

namespace xs {
// -------------------------------------------------------------------------- //
// ColorPageGenerator
// -------------------------------------------------------------------------- //
  class ColorPageGenerator: public PageGeneratorBase<ColorPageGenerator> {
  public:
    enum {
      COLS = 40,
      ROWS = 50,
      SIZE = 4000
    };

    ColorPageGenerator(const vigra::BRGBImage& image, const Palette& palette, QString name): 
      PageGeneratorBase<ColorPageGenerator>(((image.width() + COLS - 1) / COLS) * ((image.height() + ROWS - 1) / ROWS), name), mImage(image), mPalette(palette) {}

    void drawPage(QPainter& painter, int pageNum) {
      PageGeneratorBase<ColorPageGenerator>::drawPage(painter, pageNum);

      int pagesInRow = (mImage.width() + COLS - 1) / COLS;
      int xPage = pageNum % pagesInRow;
      int yPage = pageNum / pagesInRow;

      int xStart = COLS * xPage;
      int yStart = ROWS * yPage;

      int left = (width() - COLS * SIZE) / 2;
      int top  = (height() - ROWS * SIZE) / 2;

      painter.setFont(QFont("Arial", 3000));
      for(int c = 0; c < COLS && xStart + c < mImage.width(); c++) {
        for(int r = 0; r < ROWS && yStart + r < mImage.height(); r++) {
          RgbValue rgb = mImage(xStart + c, yStart + r);

          QRect symbolRect(left + c * SIZE, top + r * SIZE, SIZE, SIZE);
          painter.fillRect(symbolRect, QColor(rgb.red(), rgb.green(), rgb.blue()));
          if(rgb.luminance() < 128)
            painter.setPen(QPen(QBrush(Qt::white), 200));
          else
            painter.setPen(QPen(QBrush(Qt::black), 200));
          painter.drawText(symbolRect, Qt::AlignCenter, QChar(mPalette.color(rgb).symbol()));
        }
      }

      painter.setFont(QFont("Arial", 2000));

      for(int c = 0; c <= COLS && xStart + c <= mImage.width(); c++) {
        int yLo = top, yHi = top + std::min(static_cast<int>(ROWS), mImage.height() - yStart) * SIZE;
        int x = left + c * SIZE;
        painter.setPen(QPen(QBrush(Qt::black), c % 5 == 0 ? 350 : 200));
        painter.drawLine(x, yLo, x, yHi);

        if(c % 5 == 0) {
          painter.drawText(QRect(x - 5000, yLo - 5000, 10000, 5000), Qt::AlignCenter, QString::number(xStart + c));
          painter.drawText(QRect(x - 5000, yHi,        10000, 5000), Qt::AlignCenter, QString::number(xStart + c));
        }
      }

      for(int r = 0; r <= ROWS && yStart + r <= mImage.height(); r++) {
        int xLo = left, xHi = left + std::min(static_cast<int>(COLS), mImage.width() - xStart) * SIZE;
        int y = top + r * SIZE;
        painter.setPen(QPen(QBrush(Qt::black), r % 5 == 0 ? 350 : 200));
        painter.drawLine(xLo, y, xHi, y);

        if(r % 5 == 0) {
          painter.drawText(QRect(xLo - 5000, y - 5000, 5000, 10000), Qt::AlignCenter, QString::number(yStart + r));
          painter.drawText(QRect(xHi,        y - 5000, 5000, 10000), Qt::AlignCenter, QString::number(yStart + r));
        }
      }
    

    }


  private:
    vigra::BRGBImage mImage;
    Palette mPalette;
  };

} // namespace xs

#endif // __XSX_COLOR_PAGE_GENERATOR_H__
