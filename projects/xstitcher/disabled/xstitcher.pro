TEMPLATE  = app
CONFIG   += qt warn_on console
QT       += xml

SOURCES = \
  src/xstitcher.cpp \
  src/xs/Palette.cpp \
  
HEADERS = \
  src/config.h \
  src/xs/Color.h \
  src/xs/Palette.h \
  src/xs/RgbValue.h \
  src/xs/ColorPageGenerator.h \
  src/xs/FrontPageGenerator.h \
  src/xs/PageGeneratorBase.h \

include($(ARXPATH)/arx/ext/qt/FormPrinter.pri)

FORMS = \

RESOURCES = \

UI_DIR    = src/ui
MOC_DIR   = bin/temp/moc
RCC_DIR   = bin/temp/rcc
TARGET    = xstitcher

#win32:RC_FILE    = src/res/xstitcher.rc

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
    #QMAKE_POST_LINK = upx -9 -q $$DESTDIR/$$join(TARGET, "", "", ".exe")
  }
}
