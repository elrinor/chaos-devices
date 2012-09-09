#ifndef __UTF8HEX_MAIN_WIDGET_H__
#define __UTF8HEX_MAIN_WIDGET_H__

#include <QtGui>

namespace utf8hex {
  class MainWidget: public QWidget {
    Q_OBJECT;
  public:
    MainWidget() {
      mInputEdit = new QTextEdit();
      mInputEdit->setAcceptRichText(false);

      mOutputEdit = new QTextEdit();
      mOutputEdit->setAcceptRichText(false);
      mOutputEdit->setReadOnly(true);

      QPushButton* convertButton = new QPushButton("Convert");
      connect(convertButton, SIGNAL(clicked()), this, SLOT(convert()));

      QVBoxLayout* layout = new QVBoxLayout();
      layout->addWidget(mInputEdit);
      layout->addWidget(mOutputEdit);
      layout->addWidget(convertButton);

      setLayout(layout);
    }

  private slots:
    void convert() {
      QByteArray utf8 = mInputEdit->toPlainText().toUtf8();

      QString out = "\"";
      for(int i = 0; i < utf8.size(); i++)
        out.append(QString("\\x%1").arg(static_cast<unsigned>(static_cast<unsigned char>(utf8[i])), 2, 16, QChar('0')));
      out.append("\"");

      mOutputEdit->setPlainText(out);
    }

  private:
    QTextEdit* mInputEdit;
    QTextEdit* mOutputEdit;
  };

} // namespace utf8hex

#endif // __UTF8HEX_MAIN_WIDGET_H__
