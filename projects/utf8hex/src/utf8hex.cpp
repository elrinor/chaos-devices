#include <QApplication>
#include "utf8hex/MainWidget.h"

int main(int argc, char** argv) {
  QApplication app(argc, argv);

  utf8hex::MainWidget mainWidget;
  mainWidget.show();
  return app.exec();
}
