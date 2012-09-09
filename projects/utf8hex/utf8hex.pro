TEMPLATE  = app
CONFIG   += qt warn_on

SOURCES = \
  src/utf8hex.cpp \

HEADERS = \
  src/utf8hex/MainWidget.h \

FORMS = 

RESOURCES = \

INCLUDEPATH += src

UI_DIR    = src/ui
MOC_DIR   = bin/temp/moc
RCC_DIR   = bin/temp/rcc
TARGET    = utf8hex

CONFIG(debug, debug|relase) {
  win32 {
    DESTDIR         = bin/debug
    OBJECTS_DIR     = bin/debug
  }
}

CONFIG(release, debug|release) {
  win32 {
    DESTDIR         = bin/release
    OBJECTS_DIR     = bin/release
    QMAKE_POST_LINK = upx -9 -q $$DESTDIR/$$join(TARGET, "", "", ".exe")
  }
}
