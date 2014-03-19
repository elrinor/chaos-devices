TEMPLATE  = app
CONFIG   += qt warn_on console
QT       += xml

SOURCES = \
  src/xspng.cpp \
  src/xs/Palette.cpp \
  
HEADERS = \
  src/xs/ColorPageGenerator.h \
  src/xs/FrontPageGenerator.h \
  src/xs/PageGeneratorBase.h \

FORMS = \

RESOURCES = \

UI_DIR    = src/ui
MOC_DIR   = bin/temp/moc
RCC_DIR   = bin/temp/rcc
TARGET    = xspng

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
