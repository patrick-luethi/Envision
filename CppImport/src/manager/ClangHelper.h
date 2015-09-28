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

				Token next()
				{
					auto nextLoc = clang_->getLocForEndOfToken(loc_);
					if (nextLoc == loc_) nextLoc = loc_.getLocWithOffset(1);

					return Token(clang_, nextLoc);
				}

				QVector<Token*> type()
				{
					auto first = qualifiedIdentifier();

					if (toString(first) == "const")
					{
						auto nextToken = first.last()->next();
						while (nextToken.isEmpty() || nextToken.isWhitespace()) nextToken = nextToken.next();
						auto second = nextToken.qualifiedIdentifier();

						QVector<Token*> total;
						for (auto f : first) total.append(f);
						for (auto s : second) total.append(s);
						return total;
					}

					return first;
				}

				QVector<Token*> qualifiedIdentifier()
				{
					QVector<Token*> result;
					buildQualifiedIdentifier(&result);
					return result;
				}

				QVector<Token*> identifier()
				{
					QVector<Token*> result;
					buildIdentifier(&result);
					return result;
				}

				QString toString(QVector<Token*> tokens)
				{
					if (tokens.empty()) return "";

					return clang_->getSpelling(tokens.first()->loc(), tokens.last()->loc());
				}

				bool isIdentifier() { return matchesRegex("^\\w+$");	}
				bool isWhitespace() { return matchesRegex("^\\s+$"); }
				bool isEmpty() { return value().length() == 0; }
				bool isConcatenation() { return value() == "##"; }
				bool isStringifycation() { return value() == "#"; }
				bool isNamespaceSeparator() { return value() == "::"; }

				clang::SourceLocation loc() { return loc_; }

				bool matchesRegex(QString regex)
				{
					QRegularExpression regularExpression(regex);
					return regularExpression.match(value()).hasMatch();
				}

			private:
				void buildQualifiedIdentifier(QVector<Token*>* tokens)
				{
					if (isIdentifier() || isConcatenation() || isStringifycation() || isNamespaceSeparator())
					{
						tokens->append(new Token(clang_, loc_));
						next().buildQualifiedIdentifier(tokens);
					}
				}

				void buildIdentifier(QVector<Token*>* tokens)
				{
					if (isIdentifier() || isConcatenation() || isStringifycation())
					{
						tokens->append(new Token(clang_, loc_));
						next().buildIdentifier(tokens);
					}
				}

				ClangHelper* clang_;
				clang::SourceLocation loc_;
		};

		bool isMacroRange(clang::SourceRange range) { return range.getBegin().isMacroID() && range.getEnd().isMacroID(); }

	private:
		const clang::Preprocessor* preprocessor_;
		const clang::SourceManager* sm_;

};

}
