TEMPLATE  = app
CONFIG   += qt warn_on console
QT       += xml

SOURCES = \
  src/palviewer.cpp \
  src/xs/Palette.cpp \
  
HEADERS = \

FORMS = \

RESOURCES = \

UI_DIR    = src/ui
MOC_DIR   = bin/temp/moc
RCC_DIR   = bin/temp/rcc
TARGET    = palviewer

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
