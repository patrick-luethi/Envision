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

#include "ModelBase/src/nodes/Node.h"
#include "OOModel/src/elements/FormalMetaArgument.h"

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
		};

		void setSourceManager(const clang::SourceManager* sourceManager);

		void addMacroDefinition(QString name, const clang::MacroDirective* md);
		void addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md,
									  const clang::MacroArgs* args);

		void mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode);
		void mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode);

		QString getDefinitionName(const clang::MacroDirective* md);

		MacroImportHelper::ExpansionEntry* getExpansion(clang::SourceRange S);
		MacroImportHelper::ExpansionEntry* getExpansion(Model::Node* node);

		QVector<Model::Node*> getTopLevelMacroExpansionNodes();
		Model::Node* getNode(MacroImportHelper::ExpansionEntry* expansion);
		void nodeReplaced(Model::Node* node, Model::Node* replacement);

		QVector<OOModel::FormalMetaArgument*> generateFormalArguments(const clang::MacroDirective* definition);

		Model::Node* calculateAnchor(Model::Node* node, MacroImportHelper::ExpansionEntry* expansion, bool up);

	private:
		const clang::SourceManager* sourceManager_;

		QHash<const clang::MacroDirective*, QString> definitions_;
		QHash<Model::Node*, clang::SourceRange> astMapping_;
		QHash<Model::Node*, ExpansionEntry*> expansionCache_;
		QVector<ExpansionEntry*> expansions_;

		clang::SourceLocation getImmediateMacroLoc(clang::SourceLocation loc);
		QVector<clang::SourceLocation> getMacroExpansionStack(clang::SourceLocation loc);
		bool calculateJoin(QVector<clang::SourceLocation> hl1, QVector<clang::SourceLocation> hl2,
								 clang::SourceLocation* result);
		Model::Node*closestParentWithAstMapping(Model::Node* node);
		Model::Node*farthestParentWithAstMapping(Model::Node* node);
};

}
