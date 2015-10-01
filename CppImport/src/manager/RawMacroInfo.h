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

#include "AstMapping.h"
#include "ClangHelper.h"
#include "cppimport_api.h"

#include "OOModel/src/allOOModelNodes.h" // TODO: REMOVE AFTER REFACTORING

namespace CppImport {

class CPPIMPORT_API RawMacroInfo
{
	public:
		class RawExpansion
		{
			public:
				RawExpansion(clang::SourceRange range, const clang::MacroDirective* definition,
									  const clang::MacroArgs* args) :
					range_(range), definition_(definition), args_(args) {}

				clang::SourceRange range() { return range_; }
				const clang::MacroDirective* definition() { return definition_; }
				const clang::MacroArgs* args() { return args_; }

			private:
				clang::SourceRange range_;
				const clang::MacroDirective* definition_;
				const clang::MacroArgs* args_;
		};

		QHash<const clang::MacroDirective*, QString> definitions_;
		QList<RawExpansion*> expansions_;

		QString getDefinitionName(const clang::MacroDirective* md);
		ClangHelper* clang() { return &clang_; }
		AstMapping* astMapping() { return &astMapping_; }
		QString hashDefinition(const clang::MacroDirective* md);

		bool isExpansionception(clang::SourceLocation loc);

		bool isIncompleteMacroBegin(const clang::MacroDirective* definition)
		{
			return getDefinitionName(definition).startsWith("BEGIN_");
		}
		bool isIncompleteMacroEnd(const clang::MacroDirective* definition)
		{
			return getDefinitionName(definition).startsWith("END_");
		}
		OOModel::MetaCallExpression* containsMetaCall(Model::Node* node);
		void removeNode(Model::Node* node);

		void clear()
		{
			definitions_.clear();
			astMapping_.clear();
		}

		RawMacroInfo::RawExpansion* getImmediateExpansion(clang::SourceLocation loc);

	private:
		ClangHelper clang_;
		AstMapping astMapping_;

};

}
