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

#include "MacroImportHelper.h"

namespace CppImport {

LexicalHelper::LexicalHelper(MacroImportHelper* mih) : mih_(mih) {}

bool LexicalHelper::getUnexpandedNameWithQualifiers(clang::SourceLocation loc, QString* result)
{
	auto token = getUnexpToken(loc);

	*result = token.value();
	if (result->startsWith("#")) return true; // TODO: wth?

	QRegularExpression regularExpression("^(\\w|##)*$");
	auto match = regularExpression.match(token.value());
	if (!match.hasMatch())
	{
		*result = "";
		return false;
	}

	while (true)
	{
		auto sepa = token.next();
		if (!nameSeparator(sepa.value()))
			break;

		auto identifier = sepa.next();
		if (identifier.value() == ",")
			break;

		*result += sepa.value() + identifier.value();
		token = identifier.next();
	}

	return true;
}

bool LexicalHelper::nameSeparator(QString candidate)
{
	return candidate == "::" || candidate == "." || candidate == "<" || candidate == ">" || candidate == "->";
}

bool LexicalHelper::isExpansionception(clang::SourceLocation loc)
{
	if (loc.isMacroID())
		if (auto immediateExpansion = mih_->expansionManager_.getImmediateExpansion(loc))
			return mih_->clang()->sourceManager()->getImmediateExpansionRange(loc).first !=
					immediateExpansion->range.getBegin();

	return false;
}

LexicalHelper::Token LexicalHelper::getUnexpToken(clang::SourceLocation start)
{
	auto loc = isExpansionception(start) ?
				mih_->clang()->sourceManager()->getImmediateExpansionRange(start).first : start;

	return LexicalHelper::Token(mih_->clang(), mih_->clang()->sourceManager()->getSpellingLoc(loc));
}

QString LexicalHelper::getUnexpandedSpelling(clang::SourceRange range)
{
	clang::SourceLocation start, end;

	if (isExpansionception(range.getBegin()))
		start = mih_->clang()->sourceManager()->getImmediateExpansionRange(range.getBegin()).first;
	else
		start = range.getBegin();

	if (isExpansionception(range.getEnd()))
		end = mih_->clang()->sourceManager()->getImmediateExpansionRange(range.getEnd()).second;
	else
		end = range.getEnd();

	return mih_->clang()->getSpelling(start, end);
}

void LexicalHelper::correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* original)
{
	if (!mih_->clang()->isMacroRange(namedDecl->getSourceRange())) return;

	auto token = getUnexpToken(namedDecl->getSourceRange().getBegin());
	auto typeTokens = token.type();

	auto identifier = token.toString(typeTokens);
	auto nextToken = typeTokens.last()->next();

	while (nextToken.isWhitespace() || nextToken.isEmpty()) nextToken = nextToken.next();

	if (nextToken.value() == "*") // TODO: this won't work with multipointers
	{
		if (auto ptrExpr = DCast<OOModel::PointerTypeExpression>(original->typeExpression()))
			if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
				lexicalTransform_.insert(refExpr, identifier);
	}
	else if (nextToken.value() == "&")
	{
		if (auto ptrExpr = DCast<OOModel::ReferenceTypeExpression>(original->typeExpression()))
			if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
				lexicalTransform_.insert(refExpr, identifier);
	}
	else
		lexicalTransform_.insert(original->typeExpression(), identifier);
}

void LexicalHelper::correctFormalResultType(clang::FunctionDecl* method, OOModel::FormalResult* original)
{
	if (!mih_->clang()->isMacroRange(method->getReturnTypeSourceRange())) return;

	auto token = getUnexpToken(method->getReturnTypeSourceRange().getBegin());
	auto typeTokens = token.type();

	auto identifier = token.toString(typeTokens);
	auto nextToken = typeTokens.last()->next();

	while (nextToken.isWhitespace() || nextToken.isEmpty()) nextToken = nextToken.next();

	if (nextToken.value() == "*")
	{
		if (auto ptrExpr = DCast<OOModel::PointerTypeExpression>(original->typeExpression()))
			if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
				lexicalTransform_.insert(refExpr, identifier);
	}
	else if (nextToken.value() == "&")
	{
		if (auto ptrExpr = DCast<OOModel::ReferenceTypeExpression>(original->typeExpression()))
			if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
				lexicalTransform_.insert(refExpr, identifier);
	}
	else
		lexicalTransform_.insert(original->typeExpression(), identifier);
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
			lexicalTransform_.insert(sLit, getUnexpandedSpelling(expr->getSourceRange()));
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
		lexicalTransform_.insert(refs.pop(), token.toString(identTokens));
	}
	else
	{
		// TODO: maybe examine this deeper (only intended to happen for global scope)
		refs.pop();
		token = token.next();
	}

	while (!refs.empty())
	{
		while (nameSeparator(token.value()))
			token = token.next();

		identTokens = token.identifier();

		if (!identTokens.empty())
		{
			lexicalTransform_.insert(refs.pop(), token.toString(identTokens));
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

void LexicalHelper::correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* original)
{
	if (loc.isMacroID())
		if (isExpansionception(loc))
		{
			auto token = getUnexpToken(loc);
			lexicalTransform_.insert(original, token.toString(token.identifier()));
		}
}

void LexicalHelper::correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
																OOModel::ReferenceExpression* original)
{
	if (!mih_->clang()->isMacroRange(specializationDecl->getSourceRange()))	return;

	auto spelling = mih_->clang()->getSpelling(specializationDecl->getLocation(),
															 specializationDecl->getSourceRange().getEnd());

	// TODO: improve parsing to handle more complicated cases.
	QRegularExpression regularExpression("^(\\w+)<(\\w+::\\w+)>$");

	auto match = regularExpression.match(spelling);
	if (match.hasMatch())
	{
		lexicalTransform_.insert(original, match.captured(1));

		Q_ASSERT(original->typeArguments()->size() == 1);

		auto typeArg = DCast<OOModel::ReferenceExpression>(original->typeArguments()->first());
		Q_ASSERT(typeArg);
		Q_ASSERT(!typeArg->prefix());
		lexicalTransform_.insert(typeArg, match.captured(2));
	}
	else
	{
		qDebug() << "could not correct explicit template instantiation: " << spelling;
	}
}

void LexicalHelper::correctStringLiteral(clang::StringLiteral* strLit, OOModel::StringLiteral* original)
{
	if (!strLit->getLocStart().isMacroID()) return;

	auto rawValue = getUnexpandedSpelling(strLit->getSourceRange());

	lexicalTransform_.insert(original, rawValue);
}

void LexicalHelper::correctIntegerLiteral(clang::IntegerLiteral* intLit, OOModel::IntegerLiteral* original)
{
	if (!intLit->getLocation().isMacroID()) return;

	auto token = getUnexpToken(intLit->getLocStart());

	lexicalTransform_.insert(original, token.value());
}

void LexicalHelper::correctCastType(clang::Expr* expr, OOModel::CastExpression* original)
{
	if (!mih_->clang()->isMacroRange(expr->getSourceRange())) return;

	auto spelling = mih_->clang()->getSpelling(expr->getSourceRange());

	QRegularExpression regularExpression("static_cast<\\s*((\\w|\\*|##)+)\\s*>");
	auto match = regularExpression.match(spelling);

	if (match.hasMatch())
	{
		auto typeExpression = match.captured(1);

		if (typeExpression.endsWith("*"))
		{
			if (auto ptrExpr = DCast<OOModel::PointerTypeExpression>(original->castType()))
				if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
					lexicalTransform_.insert(refExpr, typeExpression);
		}
		else
			lexicalTransform_.insert(original->castType(), typeExpression);
	}
}

void LexicalHelper::correctNamedDecl(clang::Decl* decl, Model::Node* node)
{
	if (!mih_->clang()->isMacroRange(decl->getSourceRange())) return;

	if (auto ooDecl = DCast<OOModel::Declaration>(node))
	{
		if (auto namedDecl = clang::dyn_cast<clang::NamedDecl>(decl))
		{
			auto token = getUnexpToken(namedDecl->getLocation());

			lexicalTransform_.insert(ooDecl, token.toString(token.qualifiedIdentifier()));
		}
		else
			Q_ASSERT(false && "not implemented");
	}
	else
		Q_ASSERT(false && "not implemented");
}

void LexicalHelper::applyLexicalTransformations(Model::Node* node, NodeMapping* mapping)
{
	if (lexicalTransform_.contains(mapping->original(node)))
	{
		auto transformed = lexicalTransform_.value(mapping->original(node));

		if (auto ref = DCast<OOModel::ReferenceExpression>(node))
			ref->setName(transformed);
		else if (auto decl = DCast<OOModel::Declaration>(node))
			decl->setName(transformed);
		else if (auto strLit = DCast<OOModel::StringLiteral>(node))
			strLit->setValue(transformed);
		else
			qDebug() << "Unhandled transformed node type" << node->typeName();
	}

	for (auto child : node->children())
		applyLexicalTransformations(child, mapping);
}

LexicalHelper::Token LexicalHelper::Token::next()
{
	auto nextLoc = clang_->getLocForEndOfToken(loc_);
	if (nextLoc == loc_) nextLoc = loc_.getLocWithOffset(1);

	return Token(clang_, nextLoc);
}

QVector<LexicalHelper::Token*> LexicalHelper::Token::type()
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

QVector<LexicalHelper::Token*> LexicalHelper::Token::qualifiedIdentifier()
{
	QVector<Token*> result;
	buildQualifiedIdentifier(&result);
	return result;
}

QVector<LexicalHelper::Token*> LexicalHelper::Token::identifier()
{
	QVector<Token*> result;
	buildIdentifier(&result);
	return result;
}

QString LexicalHelper::Token::toString(QVector<LexicalHelper::Token*> tokens)
{
	if (tokens.empty()) return "";

	return clang_->getSpelling(tokens.first()->loc(), tokens.last()->loc());
}

bool LexicalHelper::Token::matchesRegex(QString regex)
{
	QRegularExpression regularExpression(regex);
	return regularExpression.match(value()).hasMatch();
}

void LexicalHelper::Token::buildQualifiedIdentifier(QVector<LexicalHelper::Token*>* tokens)
{
	if (isIdentifier() || isConcatenation() || isStringifycation() || isNamespaceSeparator())
	{
		tokens->append(new Token(clang_, loc_));
		next().buildQualifiedIdentifier(tokens);
	}
}

void LexicalHelper::Token::buildIdentifier(QVector<LexicalHelper::Token*>* tokens)
{
	if (isIdentifier() || isConcatenation() || isStringifycation())
	{
		tokens->append(new Token(clang_, loc_));
		next().buildIdentifier(tokens);
	}
}

}
