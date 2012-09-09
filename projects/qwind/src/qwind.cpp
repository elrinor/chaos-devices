#include <qw/config.h>
#include <QApplication>
#include "qw/MainWindow.h"

int main(int argc, char **argv) {
    QApplication app(argc, argv);

    qw::MainWindow mainWindow;
    mainWindow.show();

    return app.exec();
}