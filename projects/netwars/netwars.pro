TEMPLATE  = app
CONFIG   += qt warn_on
#CONFIG   += embed_manifest_exe
CONFIG   += static
QT       += 

SOURCES = \
  src/Actor.cpp \
  src/Attack.cpp \
  src/Constraint.cpp \
  src/DBase.cpp \
  src/Defence.cpp \
  src/FuzzySet.cpp \
  src/Game.cpp \
  src/GameMap.cpp \
  src/Link.cpp \
  src/main.cpp \
  src/Tool.cpp \
  src/Unit.cpp \
  src/Util.cpp \
  src/ui/AboutDialog.cpp \
  src/ui/MainForm.cpp \
  src/ui/MapWidget.cpp \
  src/ui/UnitConfigurationsBox.cpp \
  src/ui/UnitGraphicsItem.cpp \
  src/ui/UnitIcon.cpp \
  src/ui/HighlightGraphicsItem.cpp \
  src/ui/NewGameDialog.cpp \
  src/ui/LinkGraphicsItem.cpp \
  src/ui/StartGameDialog.cpp \
  src/ui/UnitDialog.cpp \
  src/ui/ToolInfoWidget.cpp \
  src/ui/ToolInfoDialog.cpp \
  src/ui/FlowLayout.cpp \
  src/ui/LogBox.cpp \
  src/ui/ResourceBox.cpp \
  src/ui/ChooseToolDialog.cpp \
  src/ui/GameControlBox.cpp \
  
HEADERS = \
  src/arx/Collections.h \
  src/arx/smart_ptr.h \
  src/arx/config.h \
  src/arx/Preprocessor.h \
  src/arx/Mpl.h \
  src/Actor.h \
  src/Attack.h \
  src/Constraint.cpp \
  src/DBase.h \
  src/Defence.h \
  src/forward.h \
  src/FuzzySet.h \
  src/Game.h \
  src/GameMap.h \
  src/Link.h \
  src/Tool.h \
  src/Unit.h \
  src/Util.h \
  src/ui/AboutDialog.h \
  src/ui/MainForm.h \
  src/ui/MapWidget.h \
  src/ui/UnitConfigurationsBox.h \
  src/ui/UnitGraphicsItem.h \
  src/ui/UnitIcon.cpp \
  src/ui/HighlightGraphicsItem.h \
  src/ui/NewGameDialog.h \
  src/ui/LinkGraphicsItem.h \
  src/ui/StartGameDialog.h \
  src/ui/UnitDialog.h \
  src/ui/ToolInfoWidget.h \
  src/ui/ToolInfoDialog.h \
  src/ui/FlowLayout.h \
  src/ui/LogBox.h \
  src/ui/ResourceBox.h \
  src/ui/ChooseToolDialog.h \
  src/ui/GameControlBox.h \

FORMS = 

RESOURCES = \
  src/res/netwars.qrc \

UI_DIR    = src/ui
MOC_DIR   = src/moc
RCC_DIR   = src/rcc
TARGET    = netwars

win32:debug:DESTDIR       = bin/debug
win32:debug:OBJECTS_DIR   = bin/debug
win32:release:OBJECTS_DIR = bin/release
win32:release:DESTDIR     = bin/release

win32:debug:CONFIG       += console

win32:release:CONFIG     -= console

#win32:release:QMAKE_POST_LINK = upx -9 -q $$DESTDIR/$$join(TARGET, "", "", ".exe")
#win32:debug:QMAKE_POST_LINK = 
