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

#include "CppImportPPCallback.h"

#include "clang/Lex/MacroArgs.h"

namespace CppImport {

void CppImportPPCallback::MacroDefined(const clang::Token& MacroNameTok, const clang::MacroDirective* MD)
{
	auto name = QString::fromStdString(MacroNameTok.getIdentifierInfo()->getName().str());

	if (name.startsWith("_")) return; // TODO: just for debug
	if (name.endsWith("_API")) return;
	if (name.startsWith("QT_")) return;
	//if (name != "STATEMENTS") return;

	definitions_[name] = MD;

	result_.addMacroDefinition(name, MD);
	macroImportHelper_.addMacroDefinition(name, MD);

	qDebug() << "definition"
				<< name
				<< "|";
}

void CppImportPPCallback::MacroExpands(const clang::Token& MacroNameTok, const clang::MacroDirective* md,
													clang::SourceRange sr, const clang::MacroArgs* args)
{
	//auto expansionRange = sourceManager_->getExpansionRange(sr.getBegin());
	auto name = QString::fromStdString(MacroNameTok.getIdentifierInfo()->getName().str());
	if (!definitions_.contains(name)) return;

	macroImportHelper_.addMacroExpansion(sr, md, args);

	qDebug() << "expanding"
				<< name
				<< "|";
}

}
