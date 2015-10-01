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
#include "RawMacroInfo.h"
#include "LexicalHelper.h"

#include "OOModel/src/allOOModelNodes.h"

namespace CppImport {

class CPPIMPORT_API ExpansionManager
{
	public:
		ExpansionManager(RawMacroInfo* rawMacroInfo, LexicalHelper* lexicalHelper);

		void removeIncompleteExpansions();

		QVector<MacroExpansion*> getTopLevelExpansions();

		QVector<Model::Node*> getNodes(MacroExpansion* expansion, NodeMapping* mapping);

		void getAllArguments(Model::Node* node, QVector<MacroArgumentInfo>* result, NodeMapping* mapping);

		bool removeUnownedNodes(Model::Node* cloned, MacroExpansion* expansion, NodeMapping* mapping);
		MacroExpansion* getMatchingXMacroExpansion(Model::Node* node);

		QVector<MacroExpansion*> expansions() { return expansions_; }

	private:
		RawMacroInfo* rawMacroInfo_;
		LexicalHelper* lexicalHelper_;
		MacroExpansion* currentXMacroParent {};

		QVector<MacroExpansion*> expansions_;
		QHash<Model::Node*, QSet<MacroExpansion*>> expansionCache_;

		MacroExpansion* getImmediateExpansion(clang::SourceLocation loc);
		MacroExpansion* getExpansion(clang::SourceLocation loc);
		QSet<MacroExpansion*> getExpansion(Model::Node* node);
		MacroExpansion* getExpansionForRawExpansion(RawMacroInfo::RawExpansion* rawExpansion);

		QVector<MacroArgumentLocation> getArgumentHistory(clang::SourceRange range);
		QVector<MacroArgumentLocation> getArgumentHistory(Model::Node* node);

		void getChildrenNotBelongingToExpansion(Model::Node* node, MacroExpansion* expansion, NodeMapping* mapping,
															 QVector<Model::Node*>* result);

};

}
