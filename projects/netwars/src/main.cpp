#include "config.h"
#include <QApplication>
#include "ui/MainForm.h"
#include <cstdlib>
#include <QTime>
#include <QHash>

int main(int argc, char *argv[]) {
	srand(qHash(QTime::currentTime().toString("hh:mm:ss.zzz")));

  QApplication app(argc, argv);
  MainForm mainForm;
  mainForm.show();
  return app.exec();
}

