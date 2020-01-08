TEMPLATE  = app
CONFIG   += qt warn_on
QT       += webkit

SOURCES = \
  src/html2xxx.cpp \
  
HEADERS = \
  src/config.h \
  src/h2x/Html2Xxx.h \

FORMS = \

RESOURCES = \

UI_DIR    = src/ui
MOC_DIR   = bin/temp/moc
RCC_DIR   = bin/temp/rcc
TARGET    = html2xxx

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
  }
}

contains(CONFIG, static): {
  QTPLUGIN += qjpeg qgif qmng qico qtiff
  DEFINES  += STATIC_PLUGINS
}

unix:LIBS += -lboost_program_options
