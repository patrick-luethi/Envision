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
#include "MacroArgumentInfo.h"
#include "NodeMapping.h"
#include "OOModel/src/allOOModelNodes.h"

namespace CppImport {

class DefinitionManager;
class ExpansionManager;
class LexicalHelper;

class CPPIMPORT_API MetaDefinitionManager
{
	public:
		MetaDefinitionManager(OOModel::Project* root,
		ClangHelper* clang,
		DefinitionManager* definitionManager,
		ExpansionManager* expansionManager,
		LexicalHelper* lexicalHelper);

		void createMetaDef(QVector<Model::Node*> nodes, MacroExpansion* expansion, NodeMapping* mapping,
								 QVector<MacroArgumentInfo>& arguments, QHash<MacroExpansion*, Model::Node*>* splices);

		OOModel::MetaDefinition* createXMacroMetaDef(MacroExpansion* xMacroExpansionH_input,
																	 MacroExpansion* xMacroExpansionCpp_input);

	private:
		OOModel::Project* root_;
		ClangHelper* clang_;
		DefinitionManager* definitionManager_;
		ExpansionManager* expansionManager_;
		LexicalHelper* lexicalHelper_;

		QHash<QString, OOModel::MetaDefinition*> metaDefinitions_;
		QHash<QString, OOModel::MetaDefinition*> xMacroMetaDefinitions_;

		void addChildMetaCalls(OOModel::MetaDefinition* metaDef, MacroExpansion* expansion,	NodeMapping* childMapping,
										QHash<MacroExpansion*, Model::Node*>* splices);
		void getChildrenNotBelongingToExpansion(Model::Node* node, MacroExpansion* expansion,
																NodeMapping* mapping, QVector<Model::Node*>* result);
		bool removeUnownedNodes(Model::Node* cloned, MacroExpansion* expansion,	NodeMapping* mapping);
		void insertArgumentSplices(NodeMapping* mapping, NodeMapping* childMapping, QVector<MacroArgumentInfo>& arguments);

		MacroExpansion* partialBeginChild(MacroExpansion* expansion);
		void renameMetaCalls(Model::Node* node, QString current, QString replace);

		OOModel::Declaration* getMetaDefParent(const clang::MacroDirective* md);
		OOModel::MetaDefinition*getMetaDefinition(const clang::MacroDirective* md);
		void addMetaDefinition(const clang::MacroDirective* md, OOModel::MetaDefinition* metaDef);
		void handlePartialBeginSpecialization(OOModel::Declaration* metaDefParent, OOModel::MetaDefinition* metaDef,
														  MacroExpansion* expansion, MacroExpansion* beginChild);
		MacroExpansion* getBasePartialBegin(MacroExpansion* partialBeginExpansion);
		OOModel::ReferenceExpression* getExpansionQualifier(const clang::MacroDirective* md);
};

}
