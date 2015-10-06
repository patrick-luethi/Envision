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

#include "ExpansionManager.h"

#include "MacroImportHelper.h"

namespace CppImport {

ClangHelper* ExpansionManager::clang()
{
	return &clang_;
}

AstMapping* ExpansionManager::astMapping()
{
	return &astMapping_;
}

void ExpansionManager::mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode)
{
	if (auto bop = clang::dyn_cast<clang::BinaryOperator>(clangAstNode))
		astMapping()->astMapping_[envisionAstNode]
				.append(clang::SourceRange(bop->getOperatorLoc(), bop->getOperatorLoc()));
	else if (auto op = clang::dyn_cast<clang::CXXOperatorCallExpr>(clangAstNode))
		astMapping()->astMapping_[envisionAstNode]
				.append(clang::SourceRange(op->getOperatorLoc(), op->getOperatorLoc()));
	else
		astMapping()->astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
}

void ExpansionManager::mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode)
{
	correctNamedDecl(clangAstNode, envisionAstNode);

	if (!astMapping()->astMapping_[envisionAstNode].contains(clangAstNode->getSourceRange()))
		astMapping()->astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
}

void ExpansionManager::removeIncompleteExpansions()
{
	QVector<MacroExpansion*> tbrs;
	for (auto expansion : expansions_)
		if (getDefinitionName(expansion->definition).startsWith("END_"))
			tbrs.append(expansion);

	for (auto tbr : tbrs)
		expansions_.remove(expansions_.indexOf(tbr));
}

void ExpansionManager::addMacroDefinition(QString name, const clang::MacroDirective* md)
{
	if (name == "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_ALL") name = "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_BASE";
	if (name == "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_STYLE") name = "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_BASE";

	definitions_[md] = name;
}

void ExpansionManager::addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md,
													const clang::MacroArgs* args)
{
	if (getDefinitionName(md).startsWith("END_"))
	{
		currentXMacroParent = nullptr;
		return;
	}

	auto entry = new MacroExpansion();
	entry->range = sr;
	entry->definition = md;
	entry->parent = getExpansion(sr.getBegin());
	if (entry->parent) entry->parent->children.append(entry);
	entry->metaCall = new OOModel::MetaCallExpression(hashDefinition(entry->definition));

	if (getDefinitionName(md).startsWith("BEGIN_") && !currentXMacroParent)
		currentXMacroParent = entry;
	else if (currentXMacroParent && !entry->parent)
	{
		entry->xMacroParent = currentXMacroParent;
		currentXMacroParent->xMacroChildren.append(entry);
	}

	if (!md->getMacroInfo()->isObjectLike())
	{
		QRegularExpression regex ("\\((.*)\\)", QRegularExpression::DotMatchesEverythingOption);
		auto match = regex.match(getUnexpandedSpelling(sr));
		auto arguments = match.captured(1).split(",");

		for (auto i = 0; i < clang()->getArgumentNames(entry->definition).size(); i++)
		{
			auto actualArg = args->getUnexpArgument((unsigned int)i);
			entry->metaCall->arguments()->append(new OOModel::ReferenceExpression(arguments[i]));
			entry->argumentLocs.append(actualArg->getLocation());
		}
	}

	expansions_.append(entry);
}

QVector<MacroExpansion*> ExpansionManager::getTopLevelExpansions()
{
	QVector<MacroExpansion*> result;
	for (auto expansion : expansions_)
		if (!expansion->parent)
			result.append(expansion);

	return result;
}

MacroExpansion* ExpansionManager::getExpansion(clang::SourceLocation loc)
{
	MacroExpansion* expansion = getImmediateExpansion(loc);
	MacroExpansion* last = expansion;

	if (expansion)
	{
		do
		{
			last = expansion;
			loc = clang()->sourceManager()->getImmediateExpansionRange(loc).first;
			expansion = getImmediateExpansion(loc);
		} while (expansion && expansion->isChildOf(last));
	}

	return last;
}

MacroExpansion* ExpansionManager::getExpansion(OOModel::MetaCallExpression* metaCall)
{
	for (auto expansion : expansions_)
		if (expansion->metaCall == metaCall)
			return expansion;

	return nullptr;
}

MacroExpansion* ExpansionManager::getImmediateExpansion(clang::SourceLocation loc)
{
	auto expansion = clang_.getImmediateMacroLoc(loc);
	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->range.getBegin() == expansion) return expansions_[i];

	expansion = clang_.getImmediateMacroLoc(expansion);
	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->range.getBegin() == expansion) return expansions_[i];

	return nullptr;
}

QSet<MacroExpansion*> ExpansionManager::getExpansion(Model::Node* node)
{
	if (!node) return {}; //TODO: necessary?

	//if (!expansionCache_.contains(node))
	{
		expansionCache_[node] = {};

		if (auto n = astMapping()->closestParentWithAstMapping(node))
			if (astMapping()->astMapping_.contains(n))
				for (auto range : astMapping()->astMapping_[n])
				{
					auto expansion = getExpansion(range.getBegin());
					if (expansion)	expansionCache_[node].insert(expansion);
				}
	}

	return expansionCache_[node];
}

QVector<Model::Node*> ExpansionManager::getTopLevelNodes(MacroExpansion* expansion)
{
	Q_ASSERT(!expansion->parent);

	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : astMapping()->astMapping_.keys())
	{
		for (auto range : astMapping()->astMapping_[node])
			if (clang()->sourceManager()->getExpansionLoc(range.getBegin()) == expansion->range.getBegin())
			{
				allNodesForExpansion.append(node);
				topLevel.insert(node);
				break;
			}
	}

	for (auto node : allNodesForExpansion)
		for (auto other : allNodesForExpansion)
			if (node != other)
				if (node->isAncestorOf(other))
					topLevel.remove(other);

	QVector<Model::Node*> result;
	for (auto node : topLevel)
		result.append(node);

	return result;
}

QString ExpansionManager::getDefinitionName(const clang::MacroDirective* md)
{
	if (!definitions_.contains(md)) return nullptr;

	return definitions_[md];
}

QString ExpansionManager::hashExpansion(MacroExpansion* expansion)
{
	auto presumedLoc = clang()->sourceManager()->getPresumedLoc(expansion->range.getBegin());

	QString hash = QDir(presumedLoc.getFilename()).absolutePath()
			+ QString("|")
			+ hashDefinition(expansion->definition)
			+ QString("|")
			+ QString::number(presumedLoc.getLine())
			+ QString("|")
			+ QString::number(presumedLoc.getColumn());

	return hash;
}

QString ExpansionManager::hashDefinition(const clang::MacroDirective* md)
{
	auto presumedLoc = clang()->sourceManager()->getPresumedLoc(md->getMacroInfo()->getDefinitionLoc());

	auto suffix = QDir(presumedLoc.getFilename()).absolutePath().right(1) == "h" ? "_H" : "_CPP";

	QString hash = getDefinitionName(md) + suffix;

	return hash;
}

void ExpansionManager::applyLexicalTransformations(Model::Node* node, NodeMapping* mapping)
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

bool ExpansionManager::shouldCreateMetaCall(MacroExpansion* expansion)
{
	auto hash = hashExpansion(expansion);

	if (!metaCallDuplicationPrevention_.contains(hash))
	{
		metaCallDuplicationPrevention_.insert(hash, expansion->metaCall);
		return true;
	}

	SAFE_DELETE(expansion->metaCall);
	expansion->metaCall = metaCallDuplicationPrevention_.value(hash);
	return false;
}

void ExpansionManager::orderNodes(QVector<Model::Node*>& input)
{
	qSort(input.begin(), input.end(),
			[](Model::Node* e1, Model::Node* e2)
			{
				if (auto commonAncestor = e1->lowestCommonAncestor(e2))
					if (auto list = DCast<Model::List>(commonAncestor))
					{
						int index1 = -1;
						for (auto c : list->children())
							if (c == e1 || c->isAncestorOf(e1))
							{
								index1 = list->indexOf(c);
								break;
							}

						int index2 = -1;
						for (auto c : list->children())
							if (c == e2 || c->isAncestorOf(e2))
							{
								index2 = list->indexOf(c);
								break;
							}

						return index1 < index2;
					}

				return true;
			});
}

QVector<Model::Node*> ExpansionManager::getNodes(MacroExpansion* expansion,
																  NodeMapping* mapping)
{
	Q_ASSERT(expansion);

	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : astMapping()->astMapping_.keys())
		if (getExpansion(node).contains(expansion))
		{
			allNodesForExpansion.append(node);
			topLevel.insert(node);
		}

	for (auto node : allNodesForExpansion)
		for (auto other : allNodesForExpansion)
			if (node != other)
				if (node->isAncestorOf(other))
					topLevel.remove(other);

	QVector<Model::Node*> unorderedOriginalResult;
	for (auto node : topLevel)
		unorderedOriginalResult.append(node);

	orderNodes(unorderedOriginalResult);

	QVector<Model::Node*> orderedClonedResult;
	for (auto node : unorderedOriginalResult)
		orderedClonedResult.append(mapping->clone(node));

	return orderedClonedResult;
}

bool ExpansionManager::validContext(Model::Node* node)
{
	if (DCast<OOModel::Project>(node))
		return true;
	else if (DCast<OOModel::Module>(node))
		return true;
	else if (DCast<OOModel::Class>(node))
		return true;
	else if (DCast<OOModel::Method>(node))
		return true;
	else
		return false;
}

OOModel::Declaration* ExpansionManager::getActualContext(MacroExpansion* expansion)
{
	Q_ASSERT(!expansion->parent);

	QVector<OOModel::Declaration*> candidates;
	for (auto i = astMapping()->astMapping_.begin();
		  i != astMapping()->astMapping_.end(); i++)
		for (auto range : i.value())
			if (clang()->contains(range, expansion->range))
				if (validContext(i.key()))
				{
					candidates.append(DCast<OOModel::Declaration>(i.key()));
					break;
				}

	if (candidates.empty())
		return root_;

	auto result = candidates.first();

	for (auto candidate : candidates)
		if (result->isAncestorOf(candidate))
			result = candidate;

	return result;
}

bool ExpansionManager::getUnexpandedNameWithQualifiers(clang::SourceLocation loc, QString* result)
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

bool ExpansionManager::nameSeparator(QString candidate)
{
	return candidate == "::" || candidate == "." || candidate == "<" || candidate == ">" || candidate == "->";
}

bool ExpansionManager::isExpansionception(clang::SourceLocation loc)
{
	if (loc.isMacroID())
		if (auto immediateExpansion = getImmediateExpansion(loc))
			return clang()->sourceManager()->getImmediateExpansionRange(loc).first != immediateExpansion->range.getBegin();

	return false;
}

ClangHelper::Token ExpansionManager::getUnexpToken(clang::SourceLocation start)
{
	auto loc = isExpansionception(start) ? clang()->sourceManager()->getImmediateExpansionRange(start).first : start;

	return ClangHelper::Token(&clang_, clang()->sourceManager()->getSpellingLoc(loc));
}

QString ExpansionManager::getUnexpandedSpelling(clang::SourceRange range)
{
	clang::SourceLocation start, end;

	if (isExpansionception(range.getBegin()))
		start = clang()->sourceManager()->getImmediateExpansionRange(range.getBegin()).first;
	else
		start = range.getBegin();

	if (isExpansionception(range.getEnd()))
		end = clang()->sourceManager()->getImmediateExpansionRange(range.getEnd()).second;
	else
		end = range.getEnd();

	return clang()->getSpelling(start, end);
}

void ExpansionManager::correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* original)
{
	if (!clang()->isMacroRange(namedDecl->getSourceRange())) return;

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

void ExpansionManager::correctFormalResultType(clang::FunctionDecl* method,
																OOModel::FormalResult* original)
{
	if (!clang()->isMacroRange(method->getReturnTypeSourceRange())) return;

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

void ExpansionManager::correctMethodCall(clang::Expr* expr, OOModel::MethodCallExpression* methodCall)
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

void ExpansionManager::correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* original)
{
	if (loc.isMacroID())
		if (isExpansionception(loc))
		{
			auto token = getUnexpToken(loc);
			lexicalTransform_.insert(original, token.toString(token.identifier()));
		}
}

void ExpansionManager::correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
																	 OOModel::ReferenceExpression* original)
{
	if (!clang()->isMacroRange(specializationDecl->getSourceRange()))	return;

	auto spelling = clang()->getSpelling(specializationDecl->getLocation(),
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
		qDebug() << match.captured(2);
		lexicalTransform_.insert(typeArg, match.captured(2));
	}
	else
	{
		qDebug() << "could not correct explicit template instantiation: " << spelling;
	}
}

void ExpansionManager::correctStringLiteral(clang::StringLiteral* strLit, OOModel::StringLiteral* original)
{
	if (!strLit->getLocStart().isMacroID()) return;

	auto rawValue = getUnexpandedSpelling(strLit->getSourceRange());

	lexicalTransform_.insert(original, rawValue);
}

void ExpansionManager::correctIntegerLiteral(clang::IntegerLiteral* intLit, OOModel::IntegerLiteral* original)
{
	if (!intLit->getLocation().isMacroID()) return;

	auto token = getUnexpToken(intLit->getLocStart());

	lexicalTransform_.insert(original, token.value());
}

void ExpansionManager::correctCastType(clang::Expr* expr, OOModel::CastExpression* original)
{
	if (!clang()->isMacroRange(expr->getSourceRange())) return;

	auto spelling = clang()->getSpelling(expr->getSourceRange());

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

void ExpansionManager::correctNamedDecl(clang::Decl* decl, Model::Node* node)
{
	if (!clang()->isMacroRange(decl->getSourceRange())) return;

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

QVector<MacroArgumentLocation> ExpansionManager::getArgumentHistory(clang::SourceRange range)
{
	QVector<MacroArgumentLocation> result;

	if (clang()->sourceManager()->isMacroArgExpansion(range.getBegin()) &&
		 clang()->sourceManager()->isMacroArgExpansion(range.getEnd()))
	{
		QVector<clang::SourceLocation> spellingHistory;
		clang()->getImmediateSpellingHistory(range.getBegin(), &spellingHistory);

		for (auto argumentLoc : spellingHistory)
			for (auto expansion : expansions_)
				for (auto i = 0; i < expansion->argumentLocs.size(); i++)
					if (expansion->argumentLocs[i] == argumentLoc)
						result.append(MacroArgumentLocation(expansion, i));
	}

	return result;
}

QVector<MacroArgumentLocation> ExpansionManager::getArgumentHistory(Model::Node* node)
{
	QVector<MacroArgumentLocation> result;
	if (astMapping()->astMapping_.contains(node))
			result = getArgumentHistory(astMapping()->astMapping_[node].first());
	return result;
}

void ExpansionManager::getAllArguments(Model::Node* node, QVector<MacroArgumentInfo>* result, NodeMapping* mapping)
{
	auto argLoc = getArgumentHistory(mapping->original(node));

	if (!argLoc.empty())
	{
		result->append(MacroArgumentInfo(argLoc, node));
		return;
	}

	for (auto child : node->children())
		getAllArguments(child, result, mapping);
}

}
