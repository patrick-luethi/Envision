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
#include "NodeMapping.h"
#include "OOModel/src/allOOModelNodes.h"

namespace CppImport {

class MacroImportHelper;

class CPPIMPORT_API LexicalHelper
{
	public:
		LexicalHelper(MacroImportHelper* mih);

		void applyLexicalTransformations(Model::Node* node, NodeMapping* mapping, QVector<QString> formalArgs);

		QString getUnexpandedSpelling(clang::SourceRange range);

		void correctNode(clang::SourceRange range, Model::Node* original);

		void correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* original);
		void correctFormalResultType(clang::FunctionDecl* method, OOModel::FormalResult* original);
		void correctMethodCall(clang::Expr* expr, OOModel::MethodCallExpression* methodCall);
		void correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* original);
		void correctStringLiteral(clang::StringLiteral* strLit, OOModel::StringLiteral* original);
		void correctIntegerLiteral(clang::IntegerLiteral* intLit, OOModel::IntegerLiteral* original);
		void correctCastType(clang::Expr* expr, OOModel::CastExpression* original);
		void correctNamedDecl(clang::Decl* decl, Model::Node* node);
		void correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
																			 OOModel::ReferenceExpression* original);

	private:
		MacroImportHelper* mih_;
		QHash<Model::Node*, QString> lexicalTransform_;

		class Token
		{
			public:
				Token(ClangHelper* clang, clang::SourceLocation loc) : clang_(clang), loc_(loc) {}

				QString value() { return clang_->getSpelling(loc_); }
				Token next();

				QVector<Token*> type();
				QVector<Token*> qualifiedIdentifier();
				QVector<Token*> identifier();

				QString toString(QVector<Token*> tokens);

				bool isIdentifier() { return matchesRegex("^\\w+$");	}
				bool isWhitespace() { return matchesRegex("^\\s+$"); }
				bool isEmpty() { return value().length() == 0; }
				bool isConcatenation() { return value() == "##"; }
				bool isStringifycation() { return value() == "#"; }
				bool isNamespaceSeparator() { return value() == "::"; }

				clang::SourceLocation loc() { return loc_; }

				bool matchesRegex(QString regex);

			private:
				void buildQualifiedIdentifier(QVector<Token*>* tokens);
				void buildIdentifier(QVector<Token*>* tokens);

				ClangHelper* clang_;
				clang::SourceLocation loc_;
		};

		Token getUnexpToken(clang::SourceLocation start);
		bool getUnexpandedNameWithQualifiers(clang::SourceLocation loc, QString* result);
		bool isExpansionception(clang::SourceLocation loc);
		bool nameSeparator(QString candidate);

};

}
