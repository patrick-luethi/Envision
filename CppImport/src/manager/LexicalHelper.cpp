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

#include "LexicalHelper.h"

namespace CppImport {

LexicalHelper::Token LexicalHelper::getUnexpToken(clang::SourceLocation start)
{
	auto loc = rawMacroInfo_->isExpansionception(start) ?
				rawMacroInfo_->clang()->sourceManager()->getImmediateExpansionRange(start).first : start;

	return LexicalHelper::Token(rawMacroInfo_->clang(), rawMacroInfo_->clang()->sourceManager()->getSpellingLoc(loc));
}

void LexicalHelper::correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* arg)
{
	if (!rawMacroInfo_->clang()->isMacroRange(namedDecl->getSourceRange())) return;

	auto token = getUnexpToken(namedDecl->getSourceRange().getBegin());
	auto typeTokens = token.type();

	auto identifier = token.toString(typeTokens);
	auto nextToken = typeTokens.last()->next();

	while (nextToken.isWhitespace() || nextToken.isEmpty()) nextToken = nextToken.next();

	if (nextToken.value() == "*") // TODO: this won't work with multipointers
		arg->setTypeExpression(
					new OOModel::PointerTypeExpression(new OOModel::ReferenceExpression(identifier)));
	else if (nextToken.value() == "&")
		arg->setTypeExpression(
					new OOModel::ReferenceTypeExpression(new OOModel::ReferenceExpression(identifier)));
	else
		arg->setTypeExpression(new OOModel::ReferenceExpression(identifier));
}

OOModel::FormalResult* LexicalHelper::correctFormalResultType(clang::FunctionDecl* method,
																					  OOModel::FormalResult* current)
{
	if (!rawMacroInfo_->clang()->isMacroRange(method->getReturnTypeSourceRange())) return current;

	if (auto ooRefType = DCast<OOModel::ReferenceTypeExpression>(current->typeExpression()))
		if (auto ooRef = DCast<OOModel::ReferenceExpression>(ooRefType->typeExpression()))
			if (ooRef->typeArguments()->size() > 0) return current;

	OOModel::FormalResult* correctedResult = new OOModel::FormalResult();

	auto token = getUnexpToken(method->getReturnTypeSourceRange().getBegin());
	auto typeTokens = token.type();

	auto identifier = token.toString(typeTokens);
	auto nextToken = typeTokens.last()->next();

	while (nextToken.isWhitespace() || nextToken.isEmpty()) nextToken = nextToken.next();

	if (nextToken.value() == "*")
		correctedResult->setTypeExpression(new OOModel::PointerTypeExpression(
													  new OOModel::ReferenceExpression(identifier)));
	else if (nextToken.value() == "&")
		correctedResult->setTypeExpression(new OOModel::ReferenceTypeExpression(
													  new OOModel::ReferenceExpression(identifier)));
	else
	{
		if (identifier == "void" || identifier == "bool" || identifier == "int")
			return current;

		correctedResult->setTypeExpression(new OOModel::ReferenceExpression(identifier));
	}

	return correctedResult;
}

void LexicalHelper::correctMethodCall(clang::Expr* expr, OOModel::MethodCallExpression* methodCall)
{
	if (!expr->getExprLoc().isMacroID()) return;

	auto ref = DCast<OOModel::ReferenceExpression>(methodCall->callee());
	if (!ref) return;

	auto token = getUnexpToken(expr->getSourceRange().getBegin());

	/*
	 * these conditions intend to skip all implict type initializations.
	 * the locations don't map to the actual call in those situations because they don't exist in the source (implicit)
	 * we therefore have to skip them.
	 */
	if (!getUnexpandedSpelling(expr->getSourceRange()).contains("(") && methodCall->arguments()->size() == 0) return;
	if (token.value() == "{") return;

	/*
	 * an exception to the above are implicit QString initializations.
	 * since those might contain vital information we have to handle those.
	 */
	if (ref->name() == "QString" && methodCall->arguments()->size() == 1)
	{
		if (auto sLit = DCast<OOModel::StringLiteral>(methodCall->arguments()->first()))
		{
			auto newLit = new OOModel::ReferenceExpression(getUnexpandedSpelling(expr->getSourceRange()));
			methodCall->arguments()->replaceChild(sLit, newLit);
			if (rawMacroInfo_->astMapping()->astMapping_.contains(sLit))
			{
				rawMacroInfo_->astMapping()->astMapping_.insert(newLit,
																				rawMacroInfo_->astMapping()->astMapping_[sLit]);
				rawMacroInfo_->astMapping()->astMapping_.remove(sLit);
			}
			return;
		}
	}

	QStack<OOModel::ReferenceExpression*> refs;
	while (true)
	{
		for (auto i = ref->typeArguments()->size() - 1; i >= 0; i--)
			if (auto r = DCast<OOModel::ReferenceExpression>(ref->typeArguments()->at(i)))
				refs.push(r);
			else
				return;

		refs.push(ref);

		if (ref->prefix())
		{
			ref = DCast<OOModel::ReferenceExpression>(ref->prefix());
			if (!ref) return;
		}
		else
			break;
	}

	auto identTokens = token.identifier();

	if (!identTokens.empty())
	{
		token = token.identifier().last()->next();
		refs.pop()->setName(token.toString(identTokens));
	}
	else
	{
		// TODO: maybe examine this deeper (only intended to happen for global scope)
		refs.pop();
		token = token.next();
	}

	while (!refs.empty())
	{
		while (token.isNameSeparator())
			token = token.next();

		identTokens = token.identifier();

		if (!identTokens.empty())
		{
			refs.pop()->setName(token.toString(identTokens));
			token = identTokens.last()->next();
		}
		else
		{
			// TODO: why?
			refs.pop();
			token = token.next();
		}
	}
}

void LexicalHelper::correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* reference)
{
	if (loc.isMacroID())
		if (rawMacroInfo_->isExpansionception(loc))
		{
			auto token = getUnexpToken(loc);
			reference->setName(token.toString(token.identifier()));
		}
}

void LexicalHelper::correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
																	 OOModel::ReferenceExpression* ref)
{
	if (!rawMacroInfo_->clang()->isMacroRange(specializationDecl->getSourceRange()))	return;

	auto spelling = rawMacroInfo_->clang()->getSpelling(specializationDecl->getLocation(),
														  specializationDecl->getSourceRange().getEnd());

	// TODO: improve parsing to handle more complicated cases.
	QString ident = "(\\w+)";
	QRegularExpression regularExpression("^" + ident + "<" + ident + "::" + ident + ">$");

	auto match = regularExpression.match(spelling);
	if (match.hasMatch())
	{
		ref->setName(match.captured(1));

		auto typeArgPrefix = new OOModel::ReferenceExpression(match.captured(2));
		auto typeArg = new OOModel::ReferenceExpression(match.captured(3));
		typeArg->setPrefix(typeArgPrefix);

		ref->typeArguments()->clear();
		ref->typeArguments()->append(typeArg);
	}
	else
	{
		qDebug() << "could not correct explicit template instantiation: " << spelling;
	}
}

OOModel::Expression* LexicalHelper::correctStringLiteral(clang::StringLiteral* strLit)
{
	if (strLit->getLocStart().isMacroID())
	{
		auto rawValue = getUnexpandedSpelling(strLit->getSourceRange());

		if (rawValue == "__FILE__")
			return new OOModel::MetaCallExpression(rawValue);

		return new OOModel::StringLiteral(rawValue);
	}

	return new OOModel::StringLiteral(QString::fromStdString(strLit->getBytes().str()));
}

OOModel::Expression* LexicalHelper::correctIntegerLiteral(clang::IntegerLiteral* intLit)
{
	if (intLit->getLocation().isMacroID())
	{
		auto token = getUnexpToken(intLit->getLocStart());

		if (token.value() == "__LINE__")
			return new OOModel::MetaCallExpression(token.value());
	}

	return new OOModel::IntegerLiteral(intLit->getValue().getLimitedValue());
}

void LexicalHelper::correctCastType(clang::Expr* expr, OOModel::CastExpression* cast)
{
	if (!rawMacroInfo_->clang()->isMacroRange(expr->getSourceRange())) return;

	auto spelling = rawMacroInfo_->clang()->getSpelling(expr->getSourceRange());

	QRegularExpression regularExpression("static_cast<\\s*((\\w|\\*|##)+)\\s*>");
	auto match = regularExpression.match(spelling);

	if (match.hasMatch())
	{
		auto typeExpression = match.captured(1);

		if (typeExpression.endsWith("*"))
		{
			typeExpression.replace(typeExpression.length() - 1, 1, "");
			Q_ASSERT(!typeExpression.contains("*"));
			cast->setType(new OOModel::PointerTypeExpression(new OOModel::ReferenceExpression(typeExpression)));
		}
		else
			cast->setType(new OOModel::ReferenceExpression(typeExpression));
	}
}

void LexicalHelper::correctNamedDecl(clang::Decl* decl, Model::Node* node)
{
	if (!rawMacroInfo_->clang()->isMacroRange(decl->getSourceRange())) return;

	if (auto ooDecl = DCast<OOModel::Declaration>(node))
	{
		if (auto namedDecl = clang::dyn_cast<clang::NamedDecl>(decl))
		{
			auto token = getUnexpToken(namedDecl->getLocation());
			ooDecl->setName(token.toString(token.qualifiedIdentifier()));
		}
		else
			Q_ASSERT(false && "not implemented");
	}
	else
		Q_ASSERT(false && "not implemented");
}


QString LexicalHelper::getUnexpandedSpelling(clang::SourceRange range)
{
	clang::SourceLocation start, end;

	if (rawMacroInfo_->isExpansionception(range.getBegin()))
		start = rawMacroInfo_->clang()->sourceManager()->getImmediateExpansionRange(range.getBegin()).first;
	else
		start = range.getBegin();

	if (rawMacroInfo_->isExpansionception(range.getEnd()))
		end = rawMacroInfo_->clang()->sourceManager()->getImmediateExpansionRange(range.getEnd()).second;
	else
		end = range.getEnd();

	return rawMacroInfo_->clang()->getSpelling(start, end);
}

}
