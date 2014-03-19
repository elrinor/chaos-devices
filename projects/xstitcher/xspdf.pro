TEMPLATE  = app
CONFIG   += qt warn_on console
QT       += xml

SOURCES = \
  src/xspdf.cpp \
  src/xs/Palette.cpp \
  
HEADERS = \

include($(ARXPATH)/arx/ext/qt/FormPrinter.pri)

FORMS = \

RESOURCES = \

UI_DIR    = src/ui
MOC_DIR   = bin/temp/moc
RCC_DIR   = bin/temp/rcc
TARGET    = xspdf

CONFIG(debug, debug|release) {
  win32 {
    DESTDIR         = bin/debug
    OBJECTS_DIR     = bin/debug
  }
}

CONFIG(release, debug|release) {
  win32 {
    DESTDIR         = bin/release
    OBJECTS_DIR     = bin/release
  }
}
