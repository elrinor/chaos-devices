#ifndef __XSX_PAGE_GENERATOR_BASE_H__
#define __XSX_PAGE_GENERATOR_BASE_H__

#include "config.h"
#include <QDateTime>
#include <arx/ext/qt/FormGenerator.h>

namespace xs {
// -------------------------------------------------------------------------- //
// PageGeneratorBase
// -------------------------------------------------------------------------- //
  template<class Derived>
  class PageGeneratorBase: public arx::FormGenerator<Derived> {
  public:
    PageGeneratorBase(int numPages, QString name): arx::FormGenerator<Derived>(numPages, QPrinter::Portrait, 190000, 260000), mName(name), mYear(QDateTime::currentDateTime().toString("yyyy")) {}

    void drawPage(QPainter& painter, int pageNum) {
      (void) pageNum; /* Just to make the compiler happy. */

      painter.setPen(QPen(QBrush(Qt::black), 500));
      
      /* Header. */
      painter.drawLine(0, 5000, width(), 5000);
      painter.setFont(QFont("Arial", 4000));
      painter.drawText(QRect(0, 0, width(), 5000), Qt::AlignVCenter | Qt::AlignLeft, QString::fromWCharArray(L"FIXME"));
      
      /* Footer. */
      painter.drawLine(0, height() - 5000, width(), height() - 5000);
      painter.setFont(QFont("Arial", 3000));
      painter.drawText(QRect(0, height() - 5000, width(), 5000), Qt::AlignVCenter | Qt::AlignRight, QString::number(currentPage() + 1) + QString::fromWCharArray(L" из ") + QString::number(totalPages()));
      /*painter.drawText(QRect(0, height() - 5000, width(), 5000), Qt::AlignVCenter | Qt::AlignLeft, 
        QString::fromWCharArray(L"Схема для вышивания крестом (") + mName + QString::fromWCharArray(L") © ") + mYear + QString::fromWCharArray(L", FIXME")
      );*/
    }

  protected:
    const QString& name() const {
      return mName;
    }

  private:
    QString mName;
    QString mYear;
  };

} // namespace xs

#endif // __XSX_PAGE_GENERATOR_BASE_H__
