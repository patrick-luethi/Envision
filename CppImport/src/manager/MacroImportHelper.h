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

#include "ExpansionManager.h"
#include "MacroArgumentInfo.h"
#include "MacroArgumentLocation.h"
#include "MacroExpansion.h"
#include "cppimport_api.h"

#include "OOModel/src/allOOModelNodes.h"
#include "ClangHelper.h"

namespace CppImport {

class CPPIMPORT_API MacroImportHelper
{
	public:
		void setProject(OOModel::Project* project);
		void setSourceManager(const clang::SourceManager* sourceManager);
		void setPreprocessor(const clang::Preprocessor* preprocessor);

		void macroGeneration();

		void finalize();

		ExpansionManager expansionManager_;

	private:
		struct {
				QSet<Model::Node*> nodes;
				QHash<Model::Node*, MacroExpansion*> metaCalls;
		} finalizationInfo;

		QHash<QString, OOModel::MetaDefinition*> metaDefinitions_;

		void clear();

		void handleMacroExpansion(QVector<Model::Node*> nodes, MacroExpansion* expansion, NodeMapping* mapping,
										  QVector<MacroArgumentInfo>& arguments, QHash<MacroExpansion*, Model::Node*>* splices);

		OOModel::Declaration* getActualContext(Model::Node* node);
		void createMetaDef(QVector<Model::Node*> nodes, MacroExpansion* expansion, NodeMapping* mapping,
								 QVector<MacroArgumentInfo>& arguments, QHash<MacroExpansion*, Model::Node*>* splices);


		Model::Node* cloneWithMapping(Model::Node* node, NodeMapping* mapping);
		void buildMappingInfo(Model::Node* node, QList<Model::Node*>* info);
		void useMappingInfo(Model::Node* node, QList<Model::Node*>* info, NodeMapping* mapping);

		OOModel::Declaration* createContext(OOModel::Declaration* actualContext);

		void removeNode(Model::Node* node);
		void getChildrenNotBelongingToExpansion(Model::Node* node, MacroExpansion* expansion,
															 NodeMapping* mapping, QVector<Model::Node*>* result,
															 QHash<MacroExpansion*, Model::Node*>* splices);

		void buildMappingInfo(Model::Node* node, QList<Model::Node*>* info, NodeMapping* master);

		void addNodeToMetaDef(Model::Node* cloned, OOModel::MetaDefinition* metaDef);
		void insertArgumentSplices(NodeMapping* mapping, NodeMapping* childMapping, QVector<MacroArgumentInfo>& arguments);
		bool removeUnownedNodes(Model::Node* cloned, MacroExpansion* expansion,
										NodeMapping* mapping, QHash<MacroExpansion*, Model::Node*>* splices);
		void addChildMetaCalls(OOModel::MetaDefinition* metaDef, MacroExpansion* expansion,
									  NodeMapping* childMapping, QHash<MacroExpansion*, Model::Node*>* splices);
};

}
