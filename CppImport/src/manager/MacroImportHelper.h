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
#include "ExpansionManager.h"
#include "DefinitionManager.h"
#include "LexicalHelper.h"

#include "OOModel/src/allOOModelNodes.h"

#include "MetaDefinitionManager.h"

namespace CppImport {

class CPPIMPORT_API MacroImportHelper
{
	public:
		MacroImportHelper(OOModel::Project* project)
			: root_(project), lexicalHelper_(this), definitionManager_(this), expansionManager_(this),
				metaDefManager_(this) {}

		void macroGeneration();

		void finalize();

		void mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode);
		void mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode);

		ClangHelper* clang();
		AstMapping* astMapping();

		bool shouldCreateMetaCall(MacroExpansion* expansion);

		OOModel::Declaration* getActualContext(MacroExpansion* expansion);

		QVector<MacroArgumentLocation> getArgumentHistory(clang::SourceRange range);
		QVector<MacroArgumentLocation> getArgumentHistory(Model::Node* node);
		void getAllArguments(Model::Node* node, QVector<MacroArgumentInfo>* result, NodeMapping* mapping);

		OOModel::Project* root_;

		LexicalHelper lexicalHelper_;
		DefinitionManager definitionManager_;
		ExpansionManager expansionManager_;
		MetaDefinitionManager metaDefManager_;

	private:
		ClangHelper clang_;
		AstMapping astMapping_;

		QHash<QString, OOModel::MetaCallExpression*> metaCallDuplicationPrevention_;

		struct {
				QVector<Model::Node*> nodes;
				QHash<Model::Node*, MacroExpansion*> metaCalls;
		} finalizationInfo;

		void handleMacroExpansion(QVector<Model::Node*> nodes, MacroExpansion* expansion, NodeMapping* mapping,
										  QVector<MacroArgumentInfo>& arguments, QHash<MacroExpansion*, Model::Node*>* splices);

		MacroExpansion* getMatchingXMacroExpansion(Model::Node* node);

		void clear();
};

}
