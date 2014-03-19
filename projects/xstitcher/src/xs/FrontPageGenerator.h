#ifndef __XSX_FRONT_PAGE_GENERATOR_H__
#define __XSX_FRONT_PAGE_GENERATOR_H__

#include "config.h"
#include <arx/ext/Qt.h>
#include "PageGeneratorBase.h"
#include "StitchUpscale.h"

namespace xs {
// -------------------------------------------------------------------------- //
// FrontPageGenerator
// -------------------------------------------------------------------------- //
  class FrontPageGenerator: public PageGeneratorBase<FrontPageGenerator> {
  public:
    FrontPageGenerator(const vigra::BRGBImage& image, const Palette& palette, QString name): 
      PageGeneratorBase<FrontPageGenerator>(1, name), mImage(image), mPalette(palette) {}

    void drawPage(QPainter& painter, int pageNum) {
      assert(pageNum == 0);

      /* Create stitched image. */
      vigra::BRGBImage upscaledImage(mImage.size() * 2);
      xs::stitchUpscale(mImage, upscaledImage, 0.9);
      
      /* Convert to QImage */
      QImage qImage;
      convert(upscaledImage, qImage);

      int y = 0, x = 0;
      
      /* Header (1 cm). */
      PageGeneratorBase<FrontPageGenerator>::drawPage(painter, pageNum);
      y += 10000;

      /* Draw info header (1 cm). */
      painter.setFont(QFont("Arial", 5000));
      painter.drawText(QRect(0, y, width(), 10000), Qt::AlignLeft | Qt::AlignVCenter, QString::fromWCharArray(L"Схема для вышивания крестом"));
      y += 10000;

      /* Draw info (4 cm). */
      x = 0;
      painter.setFont(QFont("Arial", 3000, QFont::Bold));
      painter.drawText(QRect(x, y +     0, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString::fromWCharArray(L"Имя схемы:"));
      painter.drawText(QRect(x, y +  5000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString::fromWCharArray(L"Цветов:"));
      painter.drawText(QRect(x, y + 10000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString::fromWCharArray(L"Размер:"));
      painter.drawText(QRect(x, y + 15000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString::fromWCharArray(L"Дата создания:"));
      painter.drawText(QRect(x, y + 20000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString::fromWCharArray(L"Версия:"));
      //painter.drawText(QRect(x, y + 25000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString::fromWCharArray(L"FIXME:"));
      x = 40000;
      painter.setFont(QFont("Arial", 3000));
      painter.drawText(QRect(x, y +     0, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, name());
      painter.drawText(QRect(x, y +  5000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString::number(mPalette.colors().size()));
      painter.drawText(QRect(x, y + 10000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString::number(mImage.width()) + "x" + QString::number(mImage.height()));
      painter.drawText(QRect(x, y + 15000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QDateTime::currentDateTime().toString("dd.MM.yyyy"));
      painter.drawText(QRect(x, y + 20000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString(XSTITCHER_VERSION));
      //painter.drawText(QRect(x, y + 25000, width() - x, 5000), Qt::AlignLeft | Qt::AlignVCenter, QString::fromWCharArray(L"FIXME:"));

      /* Draw image. */
      x = width() / 2;
      int maxImageWidth = width() - x;
      int maxImageHeight = 40000;
      float imageRatio = static_cast<float>(upscaledImage.width()) / upscaledImage.height();
      float regionRatio = static_cast<float>(maxImageWidth) / maxImageHeight;
      int imageWidth = imageRatio > regionRatio ? maxImageWidth : maxImageHeight * imageRatio;
      int imageHeight = imageRatio > regionRatio ? maxImageWidth / imageRatio : maxImageHeight;
      painter.drawImage(QRect(x + (maxImageWidth - imageWidth) / 2, y + (maxImageHeight - imageHeight) / 2, imageWidth, imageHeight), qImage);
      
      y += 40000;

      /* Skip 0.5 cm. */
      y += 5000;

      /* Draw palette header (1 cm). */
      painter.setFont(QFont("Arial", 5000));
      painter.drawText(QRect(0, y, width(), 10000), Qt::AlignLeft | Qt::AlignVCenter, QString::fromWCharArray(L"Карта цветов (DMC)"));
      y += 10000;

      /* Draw palette (N cm). */
      int minY = y;
      int maxY = height() - 10000;
      int stepX = 60000;
      x = 0;

      painter.setFont(QFont("Arial", 3000));
      foreach(const Color& color, mPalette.colors()) {
        QRect symbolRect(x, y, 4000, 4000);
        QRect textRect(symbolRect.right(), symbolRect.top(), stepX - 5000, symbolRect.height());

        painter.fillRect(symbolRect, QColor(color.red(), color.green(), color.blue()));
        
        painter.setPen(QPen(QBrush(Qt::black), 200));
        painter.drawRect(symbolRect);
        painter.drawText(textRect, Qt::AlignVCenter | Qt::AlignLeft, " " + QString::fromStdString(color.description()) + " (" + QString::fromStdString(color.name()) + ")");

        if(color.gray() < 128)
          painter.setPen(QPen(QBrush(Qt::white), 200));
        painter.drawText(symbolRect, Qt::AlignCenter, QChar(color.symbol()));

        y += 5000;
        if(y + 5000 > maxY) {
          y = minY;
          x += stepX;
        }
      }
      y = maxY;

      /* Footer (1 cm). */
    }

  private:
    vigra::BRGBImage mImage;
    Palette mPalette;
  };

}


#endif // __XSX_FRONT_PAGE_GENERATOR_H__
