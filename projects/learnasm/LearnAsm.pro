TEMPLATE  = app
CONFIG   += qt warn_on
#CONFIG   += embed_manifest_exe
CONFIG   += static
QT       += xml

SOURCES = \
	src/main.cpp \
	src/Util.cpp \
	src/MainForm.cpp \
	src/ChooseTopicDialog.cpp \
	src/ChooseTestDialog.cpp \
	src/QuestDialog.cpp \
	src/Parser.cpp \
	src/Context.cpp \
	src/Template.cpp \
  
HEADERS = \
	src/Util.h \
	src/MainForm.h \
	src/ChooseTopicDialog.h \
	src/ChooseTestDialog.h \
	src/QuestDialog.h \
	src/Parser.h \
	src/Context.h \
	src/Template.h \
	src/TemplateGUI.h \


FORMS = 

RESOURCES = \
	src/res/LearnAsm.qrc \

UI_DIR    = src/ui
MOC_DIR   = src/moc
RCC_DIR   = src/rcc
TARGET    = LearnAsm

win32:debug:DESTDIR       = bin/debug
win32:debug:OBJECTS_DIR   = bin/debug
win32:release:OBJECTS_DIR = bin/release
win32:release:DESTDIR     = bin/release

win32:debug:CONFIG       += console

win32:release:CONFIG     -= console

win32:release:QMAKE_POST_LINK = upx -9 -q $$DESTDIR/$$join(TARGET, "", "", ".exe")
win32:debug:QMAKE_POST_LINK = 
