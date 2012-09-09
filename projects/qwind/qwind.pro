TEMPLATE  = app
CONFIG   += qt warn_on

SOURCES = \
  src/qwind.cpp \
  src/qw/MainWindow.cpp \
  
HEADERS = \
  src/qw/config.h \
  src/qw/GraphicsObject.h \
  src/qw/MainWindow.h \

FORMS = \
  src/qw/MainWindow.ui \

RESOURCES = \

INCLUDEPATH += \
  src \

UI_DIR    = bin/temp/ui
MOC_DIR   = bin/temp/moc
RCC_DIR   = bin/temp/rcc
TARGET    = qwind

CONFIG(debug, debug|release) {
  CONFIG           += console
  win32 {
    DESTDIR         = bin/debug
    OBJECTS_DIR     = bin/debug
  }
}

CONFIG(release, debug|release) {
  CONFIG           -= console
  win32 {
    DESTDIR         = bin/release
    OBJECTS_DIR     = bin/release
    QMAKE_POST_LINK = upx -9 -q $$DESTDIR/$$join(TARGET, "", "", ".exe")
  }
}

win32 {
  DEFINES += _SCL_SECURE_NO_WARNINGS
}

unix:QMAKE_CXX = g++-4.5
*-g++*:QMAKE_CXXFLAGS += -std=c++0x

contains(CONFIG, static) {
  DEFINES  += QW_STATIC_BUILD
}
