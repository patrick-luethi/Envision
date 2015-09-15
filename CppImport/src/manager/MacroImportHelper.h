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
		struct ExpansionEntry
		{
				clang::SourceRange range;
				const clang::MacroDirective* definition;
				ExpansionEntry* parent;
				QVector<clang::SourceLocation> arguments;
				QVector<ExpansionEntry*> children;
				OOModel::MetaCallExpression* metaCall;

				bool isChildOf(ExpansionEntry* entry);
		};

		typedef std::pair<MacroImportHelper::ExpansionEntry*, int> MacroArgumentLocation;
		typedef QVector<Model::Node*> MetaDefBody;

		void setSourceManager(const clang::SourceManager* sourceManager);
		void setPreprocessor(const clang::Preprocessor* preprocessor);

		void addMacroDefinition(QString name, const clang::MacroDirective* md);
		void addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md,
									  const clang::MacroArgs* args);

		void mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode);
		void mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode);

		QString getDefinitionName(const clang::MacroDirective* md);

		MacroImportHelper::ExpansionEntry* getImmediateExpansion(clang::SourceLocation loc);

		MacroImportHelper::ExpansionEntry* getExpansion(clang::SourceLocation loc);
		MacroImportHelper::ExpansionEntry* getExpansion(Model::Node* node);
		MacroImportHelper::ExpansionEntry* getExpansion(OOModel::MetaCallExpression* metaCall);

		QVector<MacroImportHelper::ExpansionEntry*> getTopLevelExpansions();

		QVector<Model::Node*> getNodes(MacroImportHelper::ExpansionEntry* expansion);

		void nodeReplaced(Model::Node* node, Model::Node* replacement);

		QVector<OOModel::FormalMetaArgument*> generateFormalArguments(const clang::MacroDirective* definition);

		QVector<MacroImportHelper::MacroArgumentLocation> getArgumentLocation(clang::SourceRange range);
		QVector<MacroImportHelper::MacroArgumentLocation> getArgumentLocation(Model::Node* node);

		void getAllArguments(Model::Node* node,
									QVector<std::pair<QVector<MacroImportHelper::MacroArgumentLocation>, Model::Node*>>* result);

		Model::Node* cloneRetainingMetaCallExpansionMapping(Model::Node* node);
		QVector<QString> getArgumentNames(const clang::MacroDirective* definition);

		void handleIdentifierConcatentation(Model::Node* node);
		void handleStringifycation(Model::Node* node);

		QString getSpelling(clang::SourceLocation start, clang::SourceLocation end);

		OOModel::Declaration* createContext(Model::Node* node);
		OOModel::Declaration* getActualContext(Model::Node* node);
		void createMetaDef(QVector<Model::Node*> nodes, ExpansionEntry* expansion);
		void calculateMetaDefParents();
		void getAllNodes(ExpansionEntry* expansion, QVector<Model::Node*>* result);
	private:
		const clang::SourceManager* sourceManager_;
		const clang::Preprocessor* preprocessor_;

		QHash<const clang::MacroDirective*, QString> definitions_;
		QHash<const clang::MacroDirective*, OOModel::Declaration*> metaDefParents_;
		QHash<QString, OOModel::MetaDefinition*> metaDefinitions_;
		QHash<Model::Node*, clang::SourceRange> astMapping_;
		QHash<OOModel::Declaration*, QString> declarationNames_;
		QHash<Model::Node*, clang::StringLiteral*> stringLiteralMapping_;
		QHash<Model::Node*, ExpansionEntry*> expansionCache_;
		QVector<ExpansionEntry*> expansions_;

		clang::SourceLocation getImmediateMacroLoc(clang::SourceLocation loc);

		QVector<clang::SourceLocation> getMacroExpansionStack(clang::SourceLocation loc);

		Model::Node*closestParentWithAstMapping(Model::Node* node);
		Model::Node*farthestParentWithAstMapping(Model::Node* node);

		void buildMetaCallExpansionMappingInfo(Model::Node* node, QList<MacroImportHelper::ExpansionEntry*>* info);
		void useMetaCallExpansionMappingInfo(Model::Node* node, QList<MacroImportHelper::ExpansionEntry*>* info);
		void getImmediateSpellingHistory(clang::SourceLocation loc, QVector<clang::SourceLocation>* result);
		QString print(clang::SourceLocation loc);
		std::pair<clang::SourceLocation, clang::SourceLocation> getStringLiteralSpellingLoc(
				clang::StringLiteral* stringLiteral);
		OOModel::Declaration* getMetaDefParent(ExpansionEntry* expansion);
		std::string getSpellingStd(clang::SourceLocation start, clang::SourceLocation end);
};

}
