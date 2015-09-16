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

#include "OOModel/src/allOOModelNodes.h"
#include "clang/Lex/MacroArgs.h"

namespace CppImport {

class CPPIMPORT_API MacroImportHelper
{
	public:
		void setSourceManager(const clang::SourceManager* sourceManager);
		void setPreprocessor(const clang::Preprocessor* preprocessor);

		void addMacroDefinition(QString name, const clang::MacroDirective* md);
		void addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md,
									  const clang::MacroArgs* args);

		void mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode);
		void mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode);

		void macroGeneration();

		void DebugStmt(clang::Stmt* S);

	private:
		struct ExpansionEntry
		{
				clang::SourceRange range;
				const clang::MacroDirective* definition;
				ExpansionEntry* parent;
				QVector<clang::SourceLocation> argumentLocs;
				QVector<ExpansionEntry*> children;
				OOModel::MetaCallExpression* metaCall;

				bool isChildOf(ExpansionEntry* entry);
		};

		const clang::SourceManager* sourceManager_;
		const clang::Preprocessor* preprocessor_;

		QHash<const clang::MacroDirective*, QString> definitions_;
		QHash<const clang::MacroDirective*, OOModel::Declaration*> metaDefParents_;
		QHash<QString, OOModel::MetaDefinition*> metaDefinitions_;
		QHash<Model::Node*, clang::SourceRange> astMapping_;
		QHash<Model::Node*, clang::StringLiteral*> stringLiteralMapping_;
		QHash<Model::Node*, ExpansionEntry*> expansionCache_;
		QVector<ExpansionEntry*> expansions_;

		typedef std::pair<ExpansionEntry*, int> MacroArgumentLocation;

		QString getSpelling(clang::SourceRange range);
		QString getSpelling(clang::SourceLocation start, clang::SourceLocation end);
		QString getDefinitionName(const clang::MacroDirective* md);
		bool isIncompleteDefinition(const clang::MacroDirective* md);

		void handleMacroExpansion(QVector<Model::Node*> nodes, ExpansionEntry* expansion);

		QVector<ExpansionEntry*> getTopLevelExpansions();

		clang::SourceLocation getImmediateMacroLoc(clang::SourceLocation loc);
		ExpansionEntry* getImmediateExpansion(clang::SourceLocation loc);
		ExpansionEntry* getExpansion(clang::SourceLocation loc);
		ExpansionEntry* getExpansion(Model::Node* node);
		ExpansionEntry* getExpansion(OOModel::MetaCallExpression* metaCall);
		Model::Node* closestParentWithAstMapping(Model::Node* node);
		QVector<clang::SourceLocation> getMacroExpansionStack(clang::SourceLocation loc);

		QVector<Model::Node*> getNodes(ExpansionEntry* expansion);
		void getAllNodes(ExpansionEntry* expansion, QVector<Model::Node*>* result);

		QVector<MacroArgumentLocation> getArgumentLocation(clang::SourceRange range);
		QVector<MacroArgumentLocation> getArgumentLocation(Model::Node* node);
		void getImmediateSpellingHistory(clang::SourceLocation loc, QVector<clang::SourceLocation>* result);
		void getAllArguments(Model::Node* node,
									QVector<std::pair<QVector<MacroArgumentLocation>, Model::Node*>>* result);
		QVector<QString> getArgumentNames(const clang::MacroDirective* definition);

		OOModel::Declaration* createContext(Model::Node* node);
		OOModel::Declaration* getActualContext(Model::Node* node);
		OOModel::Declaration* getMetaDefParent(ExpansionEntry* expansion);
		void createMetaDef(QVector<Model::Node*> nodes, ExpansionEntry* expansion);
		void nodeReplaced(Model::Node* node, Model::Node* replacement);

		Model::Node* cloneRetainingMetaCallExpansionMapping(Model::Node* node);
		void buildMetaCallExpansionMappingInfo(Model::Node* node, QList<ExpansionEntry*>* info);
		void useMetaCallExpansionMappingInfo(Model::Node* node, QList<ExpansionEntry*>* info);

		void calculateMetaDefParents();
		void calculateMetaCallArguments();

		void handleIdentifierConcatentation(Model::Node* node);
		void handleIdentifierConcatentation(clang::Decl* decl, Model::Node* node);
		void handleStringifycation(Model::Node* node);
		std::pair<clang::SourceLocation, clang::SourceLocation>
		getStringLiteralSpellingLoc(clang::StringLiteral* stringLiteral);

		bool getUnexpandedCode(clang::SourceLocation loc, QString* result);
		bool getUnexpandedCode(clang::SourceLocation start, clang::SourceLocation end, QString* result);
		bool getUnexpandedCode(clang::SourceRange range, QString* result);
		bool getUnexpandedCode(clang::SourceLocation start, clang::SourceLocation end, QString regex,
									  int capture, QString* result);
		bool getUnexpandedCode(clang::SourceRange range, QString regex, int capture, QString* result);
};

}
