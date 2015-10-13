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

#include "ClangHelper.h"

namespace CppImport {

QString ClangHelper::spelling(clang::SourceLocation start, clang::SourceLocation end)
{
	clang::SourceLocation b = sourceManager_->getSpellingLoc(start);
	clang::SourceLocation e = clang::Lexer::getLocForEndOfToken(sourceManager_->getSpellingLoc(end), 0, *sourceManager_,
																					preprocessor_->getLangOpts());

	auto length = sourceManager_->getCharacterData(e) - sourceManager_->getCharacterData(b);
	if (length > 1000000) return "ERROR_IN_GET_SPELLING";

	try
	{
		return 0 < length ? QString::fromStdString(std::string(sourceManager_->getCharacterData(b), length)) : "";
	}
	catch (...)
	{
		return "ERROR_IN_GET_SPELLING";
	}
}

clang::SourceLocation ClangHelper::immediateMacroLocation(clang::SourceLocation location)
{
	// this code is an adaptation of clang::lexer::immediateMacroName

	if (location.isMacroID())
		while (true)
		{
			auto fileID = sourceManager_->getFileID(location);
			auto sourceLocationEntry = &sourceManager_->getSLocEntry(fileID);
			if (!sourceLocationEntry->isExpansion()) break;

			auto expansion = sourceLocationEntry->getExpansion();
			location = expansion.getExpansionLocStart();
			if (!expansion.isMacroArgExpansion()) break;

			location = sourceManager_->getImmediateExpansionRange(location).first;
			auto spellingLocation = expansion.getSpellingLoc();
			if (spellingLocation.isFileID())	break;

			auto macroFileID = sourceManager_->getFileID(location);
			if (sourceManager_->isInFileID(spellingLocation, macroFileID))	break;

			location = spellingLocation;
		}

	return location;
}

void ClangHelper::immediateSpellingHistory(clang::SourceLocation location, QVector<clang::SourceLocation>* result)
{
	result->append(location);

	auto next = sourceManager_->getImmediateSpellingLoc(location);

	if (next != location)
		immediateSpellingHistory(next, result);
}

QVector<QString> ClangHelper::argumentNames(const clang::MacroDirective* definition)
{
	QVector<QString> result;

	for (auto i = definition->getMacroInfo()->arg_begin(); i != definition->getMacroInfo()->arg_end(); i++)
		result.append(QString::fromStdString((*i)->getName().str()));

	return result;
}

}
