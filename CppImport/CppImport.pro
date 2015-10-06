TARGET = cppimport
include(../Core/common_plugin.pri)

DEFINES += CPPIMPORT_LIBRARY
win32:LIBS += -llogger \
    -lselftest \
    -lmodelbase \
    -loomodel \
    -lvisualizationbase \
    -linteractionbase \
    -loovisualization \
    -loointeraction

# Use this instead of the line under it to avoid warnings
QMAKE_CXXFLAGS += -isystem ""$(shell $${ENVISION_ROOT_DIR}/misc/llvm-config-envision.sh --includedir)""
#INCLUDEPATH +=  /usr/lib/llvm/include

# The _GLIBCXX_USE_CXX11_ABI=0 macro is needed to fix a compatibility issue with the new C++11 string ABI
# See: https://gcc.gnu.org/onlinedocs/libstdc++/manual/using_dual_abi.html
# It might not be needed in later versions of Clang which take this into consideration
DEFINES += __STDC_LIMIT_MACROS __STDC_CONSTANT_MACROS _GLIBCXX_USE_CXX11_ABI=0


LIBS += -lclangTooling\
				-lclangFrontendTool\
				-lclangFrontend\
				-lclangDriver\
				-lclangSerialization\
				-lclangParse\
				-lclangSema\
				-lclangAnalysis \
				-lclangEdit\
				-lclangAST\
				-lclangLex\
				-lclangBasic\
                                ""$(shell $${ENVISION_ROOT_DIR}/misc/llvm-config-envision.sh --libs)"" \
                                $$system( $${ENVISION_ROOT_DIR}/misc/llvm-config-envision.sh --ldflags --libs cppbackend)

## END LLVM SPECIFIC

HEADERS += src/precompiled.h \
    src/CppImportException.h \
	src/cppimport_api.h \
    src/ClangAstConsumer.h \
    src/visitors/ClangAstVisitor.h \
    src/CppImportManager.h \
    src/CppImportUtilities.h \
    src/CppImportLogger.h \
    src/visitors/ExpressionVisitor.h \
    src/ClangFrontendActionFactory.h \
    src/visitors/TemplateArgumentVisitor.h \
    src/manager/NodeHasher.h \
    src/manager/TranslateManager.h \
    src/TranslateFrontendAction.h \
    src/CppImportPlugin.h \
    src/comments/CommentParser.h \
    src/CppImportPPCallback.h \
    src/manager/MacroImportHelper.h \
	src/manager/ClangHelper.h \
    src/manager/MacroExpansion.h \
    src/manager/AstMapping.h \
    src/manager/NodeMapping.h \
    src/manager/MacroArgumentLocation.h \
    src/manager/MacroArgumentInfo.h \
    src/manager/ExpansionManager.h \
    src/manager/DefinitionManager.h \
    src/manager/StaticStuff.h \
    src/manager/LexicalHelper.h
SOURCES += src/CppImportException.cpp \
	test/SimpleTest.cpp \
    src/ClangAstConsumer.cpp \
    src/visitors/ClangAstVisitor.cpp \
    src/CppImportManager.cpp \
    src/CppImportUtilities.cpp \
    src/CppImportLogger.cpp \
    src/visitors/ExpressionVisitor.cpp \
    src/ClangFrontendActionFactory.cpp \
    src/visitors/TemplateArgumentVisitor.cpp \
    src/manager/NodeHasher.cpp \
    src/manager/TranslateManager.cpp \
    src/TranslateFrontendAction.cpp \
    src/CppImportPlugin.cpp \
    src/comments/CommentParser.cpp \
    src/CppImportPPCallback.cpp \
    src/manager/MacroImportHelper.cpp \
    src/manager/ClangHelper.cpp \
    src/manager/MacroExpansion.cpp \
    src/manager/AstMapping.cpp \
    src/manager/NodeMapping.cpp \
    src/manager/MacroArgumentLocation.cpp \
    src/manager/MacroArgumentInfo.cpp \
    src/manager/ExpansionManager.cpp \
    src/manager/DefinitionManager.cpp \
    src/manager/StaticStuff.cpp \
    src/manager/LexicalHelper.cpp
