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

namespace CppImport {

class CPPIMPORT_API ClangHelper
{
	public:

		void setSourceManager(const clang::SourceManager* sourceManager);
		void setPreprocessor(const clang::Preprocessor* preprocessor);

		clang::SourceLocation getLocForEndOfToken(clang::SourceLocation loc);

		QString getSpelling(clang::SourceRange range);
		QString getSpelling(clang::SourceLocation loc);
		QString getSpelling(clang::SourceLocation start, clang::SourceLocation end);

		clang::SourceLocation getImmediateMacroLoc(clang::SourceLocation loc);

		void getImmediateSpellingHistory(clang::SourceLocation loc, QVector<clang::SourceLocation>* result);

		QVector<QString> getArgumentNames(const clang::MacroDirective* definition);

		bool contains(clang::SourceRange range, clang::SourceRange other);

		const clang::SourceManager* sourceManager();

		class Token
		{
			public:
				Token(ClangHelper* clang, clang::SourceLocation loc) : clang_(clang), loc_(loc) {}

				QString value() { return clang_->getSpelling(loc_); }
				Token next() { return Token(clang_, clang_->getLocForEndOfToken(loc_)); }

			private:
				ClangHelper* clang_;
				clang::SourceLocation loc_;
		};

		bool isMacroRange(clang::SourceRange range) { return range.getBegin().isMacroID() && range.getEnd().isMacroID(); }

	private:
		const clang::Preprocessor* preprocessor_;
		const clang::SourceManager* sm_;

};

}
