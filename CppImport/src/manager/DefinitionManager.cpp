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

#include "DefinitionManager.h"

namespace CppImport {

DefinitionManager::DefinitionManager(ClangHelper* clang) : clang_(clang) {}

void DefinitionManager::addMacroDefinition(QString name, const clang::MacroDirective* md)
{
	if (name == "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_ALL")
		name = "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_BASE";
	if (name == "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_STYLE")
		name = "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_BASE";

	definitions_[md] = name;
}

QString DefinitionManager::getDefinitionName(const clang::MacroDirective* md)
{
	if (!definitions_.contains(md)) return nullptr;

	return definitions_[md];
}

bool DefinitionManager::isPartialBegin(const clang::MacroDirective* md)
{
	return getDefinitionName(md).startsWith("BEGIN_");
}

bool DefinitionManager::isPartialEnd(const clang::MacroDirective* md)
{
	return getDefinitionName(md).startsWith("END_");
}

void DefinitionManager::clear()
{
	definitions_.clear();
}

}