/***********************************************************************************************************************
 **
 ** Copyright (c) 2011, 2015 ETH Zurich
 ** All rights reserved.
 **
 ** Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
 ** following conditions are met:
 **
 **    * Redistributions of source code must retain the above copyright notice, this list of conditions and the
 **      following disclaimer.
 **    * Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the
 **      following disclaimer in the documentation and/or other materials provided with the distribution.
 **    * Neither the name of the ETH Zurich nor the names of its contributors may be used to endorse or promote products
 **      derived from this software without specific prior written permission.
 **
 **
 ** THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 ** INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 ** DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 ** SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 ** SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 ** WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 ** OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 **
 **********************************************************************************************************************/

#pragma once

#include "cppimport_api.h"

#include "ClangHelper.h"
#include "AstMapping.h"
#include "MacroExpansion.h"
#include "NodeMapping.h"
#include "MacroArgumentLocation.h"
#include "MacroArgumentInfo.h"

#include "OOModel/src/allOOModelNodes.h"
#include "clang/Lex/MacroArgs.h"

namespace CppImport {

class CPPIMPORT_API ExpansionManager
{
	public:
		QVector<MacroExpansion*> expansions_;

		QHash<const clang::MacroDirective*, QString> definitions_;

		void addMacroDefinition(QString name, const clang::MacroDirective* md);
		void addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md, const clang::MacroArgs* args);

		void mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode);
		void mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode);

		ClangHelper* clang();
		AstMapping* astMapping();

		QString getDefinitionName(const clang::MacroDirective* md);

		QVector<MacroExpansion*> getTopLevelExpansions();

		MacroExpansion* getExpansion(clang::SourceLocation loc);
		MacroExpansion* getExpansion(OOModel::MetaCallExpression* metaCall);
		MacroExpansion* getImmediateExpansion(clang::SourceLocation loc);
		QSet<MacroExpansion*> getExpansion(Model::Node* node);

		QVector<Model::Node*> getTopLevelNodes(MacroExpansion* expansion);

		QString hashExpansion(MacroExpansion* expansion);
		bool shouldCreateMetaCall(MacroExpansion* expansion);

		QVector<Model::Node*> getNodes(MacroExpansion* expansion, NodeMapping* mapping);

		bool validContext(Model::Node* node);
		OOModel::Declaration* getActualContext(MacroExpansion* expansion);


		bool getUnexpandedCode(clang::SourceLocation loc, QString* result, clang::SourceLocation* outEnd = nullptr);
		bool getUnexpandedCode(clang::SourceLocation start, clang::SourceLocation end, QString* result,
									  clang::SourceLocation* outEnd = nullptr);
		bool getUnexpandedCode(clang::SourceRange range, QString* result, clang::SourceLocation* outEnd = nullptr);
		bool getUnexpandedNameWithQualifiers(clang::SourceLocation loc, QString* result);
		bool nameSeparator(QString candidate);

		void correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* arg);
		void correctCastType(clang::Expr* expr, OOModel::CastExpression* cast);
		void correctFormalResultType(clang::FunctionDecl* method, OOModel::Method*  ooMethod);
		void correctMethodCall(clang::Expr* expr, OOModel::MethodCallExpression* methodCall);
		void correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* reference);
		void correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
													OOModel::ReferenceExpression* reference);
		OOModel::Expression* correctIntegerLiteral(clang::IntegerLiteral* intLit);
		OOModel::Expression* correctStringLiteral(clang::StringLiteral* strLit);
		void correctNamedDecl(clang::Decl* decl, Model::Node* node);

		QVector<MacroArgumentLocation> getArgumentHistory(clang::SourceRange range);
		QVector<MacroArgumentLocation> getArgumentHistory(Model::Node* node);
		void getAllArguments(Model::Node* node, QVector<MacroArgumentInfo>* result, NodeMapping* mapping);

		QHash<Model::Node*, QSet<MacroExpansion*>> expansionCache_;

		OOModel::Project* root_{};

	private:
		ClangHelper clang_;
		AstMapping astMapping_;

		QSet<QString> metaCallDuplicationPrevention_;

};

}
