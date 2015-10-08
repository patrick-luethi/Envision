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

void ClangHelper::setSourceManager(const clang::SourceManager* sourceManager)
{
	sm_ = sourceManager;
}

void ClangHelper::setPreprocessor(const clang::Preprocessor* preprocessor)
{
	preprocessor_ = preprocessor;
}

QString ClangHelper::getSpelling(clang::SourceLocation loc)
{
	return getSpelling(loc, loc);
}

QString ClangHelper::getSpelling(clang::SourceRange range)
{
	return getSpelling(range.getBegin(), range.getEnd());
}

QString ClangHelper::getSpelling(clang::SourceLocation start, clang::SourceLocation end)
{
	clang::SourceLocation b = sm_->getSpellingLoc(start);
	clang::SourceLocation e = clang::Lexer::getLocForEndOfToken(sm_->getSpellingLoc(end), 0, *sm_,
																					preprocessor_->getLangOpts());

	auto length = sm_->getCharacterData(e) - sm_->getCharacterData(b);
	if (length > 1000000) return "ERROR_IN_GET_SPELLING";

	try
	{
		return 0 < length ? QString::fromStdString(std::string(sm_->getCharacterData(b), length)) : "";
	}
	catch (...)
	{
		return "ERROR_IN_GET_SPELLING";
	}
}

clang::SourceLocation ClangHelper::getImmediateMacroLoc(clang::SourceLocation Loc)
{
	if (Loc.isMacroID())
	{
		while (1)
		{
			auto FID = sm_->getFileID(Loc);
			const clang::SrcMgr::SLocEntry *E = &sm_->getSLocEntry(FID);
			if (!E->isExpansion())
				break;
			const clang::SrcMgr::ExpansionInfo &Expansion = E->getExpansion();
			Loc = Expansion.getExpansionLocStart();
			if (!Expansion.isMacroArgExpansion())
				break;

			Loc = sm_->getImmediateExpansionRange(Loc).first;
			auto SpellLoc = Expansion.getSpellingLoc();
			if (SpellLoc.isFileID())
				break;

			auto MacroFID = sm_->getFileID(Loc);
			if (sm_->isInFileID(SpellLoc, MacroFID))
				break;

			Loc = SpellLoc;
		}
	}

	return Loc;
}

void ClangHelper::getImmediateSpellingHistory(clang::SourceLocation loc, QVector<clang::SourceLocation>* result)
{
	result->append(loc);

	auto next = sm_->getImmediateSpellingLoc(loc);

	if (next != loc)
		getImmediateSpellingHistory(next, result);
}

QVector<QString> ClangHelper::getArgumentNames(const clang::MacroDirective* definition)
{
	QVector<QString> result;

	for (auto i = definition->getMacroInfo()->arg_begin(); i != definition->getMacroInfo()->arg_end(); i++)
		result.append(QString::fromStdString((*i)->getName().str()));

	return result;
}

bool ClangHelper::contains(clang::SourceRange range, clang::SourceRange other)
{
	auto s = sm_->getSpellingLoc(range.getBegin()).getPtrEncoding();
	auto e = sm_->getSpellingLoc(range.getEnd()).getPtrEncoding();
	auto os = sm_->getSpellingLoc(other.getBegin()).getPtrEncoding();

	return s <= os && os <= e;
}

const clang::SourceManager* ClangHelper::sourceManager()
{
	return sm_;
}

}
