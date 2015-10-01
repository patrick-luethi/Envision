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

#include "ClangHelper.h"
#include "LexicalHelper.h"
#include "NodeMapping.h"
#include "RawMacroInfo.h"
#include "cppimport_api.h"

#include "OOModel/src/expressions/MetaCallExpression.h"
#include "clang/Lex/MacroArgs.h"

namespace CppImport {

class CPPIMPORT_API MacroExpansion
{
	public:
		MacroExpansion(RawMacroInfo* rawMacroInfo, RawMacroInfo::RawExpansion* rawExpansion, LexicalHelper* lexicalHelper);

		MacroExpansion* parent;
		MacroExpansion* xMacroParent;

		bool isChildOf(MacroExpansion* entry);

		QString getDefinitionName() {	return rawMacroInfo_->getDefinitionName(definition()); }
		QString hash();
		QVector<Model::Node*> getTopLevelNodes();
		OOModel::Declaration* getActualContext();

		const clang::MacroDirective* definition() { return rawExpansion_->definition(); }
		const clang::SourceRange range() { return rawExpansion_->range(); }


		OOModel::MetaCallExpression* metaCall() { return metaCall_; }
		QVector<clang::SourceLocation> argumentLocs() { return argumentLocs_; }
		QVector<MacroExpansion*> xMacroChildren() { return xMacroChildren_; }
		QVector<MacroExpansion*> children() { return children_; }
		RawMacroInfo::RawExpansion* rawExpansion() { return rawExpansion_; }

	private:
		RawMacroInfo* rawMacroInfo_;
		RawMacroInfo::RawExpansion* rawExpansion_;

		OOModel::MetaCallExpression* metaCall_;
		QVector<clang::SourceLocation> argumentLocs_;
		QVector<MacroExpansion*> children_;
		QVector<MacroExpansion*> xMacroChildren_;
};

}
