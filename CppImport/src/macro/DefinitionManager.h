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
#include "OOModel/src/expressions/ReferenceExpression.h"

namespace CppImport {

class CPPIMPORT_API DefinitionManager
{
	public:
		DefinitionManager(ClangHelper* clang);

		void addMacroDefinition(const QString& name, const clang::MacroDirective* md);

		QString definitionName(const clang::MacroDirective* md);

		/**
		 * return whether md defines a begin incomplete macro.
		 */
		bool isPartialBegin(const clang::MacroDirective* md);

		/**
		 * return whether md defines an end incomplete macro.
		 */
		bool isPartialEnd(const clang::MacroDirective* md);

		/**
		 * if the location of md is part of Envision's project structure then
		 *  return true and set namespaceName/fileName
		 * otherwise
		 *  return false
		 */
		bool macroDefinitionLocation(const clang::MacroDirective* md, QString& namespaceName, QString& fileName);

		QString hash(const clang::MacroDirective* md);

		/**
		 * return a qualifier expression based on the macroDefinitionLocation of md.
		 */
		OOModel::ReferenceExpression* expansionQualifier(const clang::MacroDirective* md);

		void clear();

	private:
		ClangHelper* clang_{};
		QHash<const clang::MacroDirective*, QString> definitions_;

};

inline void DefinitionManager::addMacroDefinition(const QString& name, const clang::MacroDirective* md)
{
	definitions_[md] = name;
}

inline bool DefinitionManager::isPartialBegin(const clang::MacroDirective* md)
{
	return definitionName(md).startsWith("BEGIN_");
}

inline bool DefinitionManager::isPartialEnd(const clang::MacroDirective* md)
{
	return definitionName(md).startsWith("END_");
}

inline void DefinitionManager::clear() { definitions_.clear(); }

}
