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
	else
		astMapping()->astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
}

void ExpansionManager::mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode)
{
	correctNamedDecl(clangAstNode, envisionAstNode);

	if (!astMapping()->astMapping_[envisionAstNode].contains(clangAstNode->getSourceRange()))
		astMapping()->astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
}

void ExpansionManager::addMacroDefinition(QString name, const clang::MacroDirective* md)
{
	definitions_[md] = name;
}

void ExpansionManager::addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md,
													const clang::MacroArgs* args)
{
	auto entry = new MacroExpansion();
	entry->range = sr;
	entry->definition = md;
	entry->parent = getExpansion(sr.getBegin());
	if (entry->parent) entry->parent->children.append(entry);
	entry->metaCall = new OOModel::MetaCallExpression(getDefinitionName(entry->definition));

	qDebug() << "*" << getDefinitionName(entry->definition);

	for (auto i = 0; i < clang()->getArgumentNames(entry->definition).size(); i++)
	{
		auto actualArg = args->getUnexpArgument((unsigned int)i);

		QString unexpandedName;
		if (getUnexpandedNameWithQualifiers(actualArg->getLocation(), &unexpandedName))
		{
			entry->metaCall->arguments()->append(new OOModel::ReferenceExpression(unexpandedName));
		}
		else if (actualArg->getIdentifierInfo())
		{
			auto argText = QString::fromStdString(actualArg->getIdentifierInfo()->getName().str());
			entry->metaCall->arguments()->append(new OOModel::ReferenceExpression(argText));
			qDebug() << "using identifier info to build meta call argument (this should not happen ideally)";
		}
		else
			entry->metaCall->arguments()->append(new OOModel::EmptyExpression());

		entry->argumentLocs.append(actualArg->getLocation());
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

	if (!expansionCache_.contains(node))
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
			+ getDefinitionName(expansion->definition)
			+ QString("|")
			+ QString::number(presumedLoc.getLine())
			+ QString("|")
			+ QString::number(presumedLoc.getColumn());

	return hash;
}

bool ExpansionManager::shouldCreateMetaCall(MacroExpansion* expansion)
{
	auto hash = hashExpansion(expansion);

	if (!metaCallDuplicationPrevention_.contains(hash))
	{
		metaCallDuplicationPrevention_.insert(hash);
		return true;
	}

	return false;
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

	qSort(unorderedOriginalResult.begin(), unorderedOriginalResult.end(),
			[](Model::Node* e1, Model::Node* e2)
			{
				if (auto commonAncestor = e1->lowestCommonAncestor(e2))
					if (auto list = DCast<Model::List>(commonAncestor))
						for (auto i = 0; i < list->size(); i++)
						{
							if (e1->lowestCommonAncestor(list->at(i)))
								return true;

							if (e2->lowestCommonAncestor(list->at(i)))
								return false;
						}

				return true;
			});

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
	return candidate == "::" || candidate == "." || candidate == "<" || candidate == ">";
}

ClangHelper::Token ExpansionManager::getUnexpToken(clang::SourceLocation start)
{
	auto s = clang()->sourceManager()->getImmediateExpansionRange(start).first;
	auto immediateExpansion = getImmediateExpansion(start);

	if (immediateExpansion && s != immediateExpansion->range.getBegin())
		start = s;

	return ClangHelper::Token(&clang_, clang()->sourceManager()->getSpellingLoc(start));
}

void ExpansionManager::correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* arg)
{
	if (!clang()->isMacroRange(namedDecl->getSourceRange())) return;

	auto token = getUnexpToken(namedDecl->getSourceRange().getBegin());

	if (token.next().value() == "*")
		arg->setTypeExpression(new OOModel::PointerTypeExpression(new OOModel::ReferenceExpression(token.value())));
	else
		arg->setTypeExpression(new OOModel::ReferenceExpression(token.value()));
}

OOModel::FormalResult* ExpansionManager::correctFormalResultType(clang::FunctionDecl* method)
{
	if (!clang()->isMacroRange(method->getReturnTypeSourceRange())) return nullptr;

	auto token = getUnexpToken(method->getReturnTypeSourceRange().getBegin());

	OOModel::FormalResult* correctedResult = new OOModel::FormalResult();

	if (token.next().value() == "*")
		correctedResult->setTypeExpression(new OOModel::PointerTypeExpression(
													  new OOModel::ReferenceExpression(token.value())));
	else
		correctedResult->setTypeExpression(new OOModel::ReferenceExpression(token.value()));

	return correctedResult;
}

void ExpansionManager::correctMethodCall(clang::Expr* expr, OOModel::MethodCallExpression* methodCall)
{
	if (!expr->getExprLoc().isMacroID()) return;

	auto ref = DCast<OOModel::ReferenceExpression>(methodCall->callee());
	if (!ref) return;

	auto qString = ref->name() == "QString";

	QStack<OOModel::ReferenceExpression*> refs;
	while (true)
	{
		for (auto i = ref->typeArguments()->size() - 1; i >= 0; i--)
			if (auto r = DCast<OOModel::ReferenceExpression>(ref->typeArguments()->at(i)))
				refs.push(r);
			else
			{
				qDebug() << "could not correct methodcall" << methodCall;
				return;
			}

		refs.push(ref);


		if (ref->prefix())
		{
			ref = DCast<OOModel::ReferenceExpression>(ref->prefix());
			if (!ref)
			{
				qDebug() << "could not correct methodcall" << methodCall;
				return;
			}
		}
		else
			break;
	}

	auto token = getUnexpToken(expr->getExprLoc());

	if (nameSeparator(token.value()))
		token = token.next();

	refs.pop()->setName(token.value());

	while (!refs.empty())
	{
		do
			token = token.next();
		while (nameSeparator(token.value()));

		refs.pop()->setName(token.value());
	}

	if (qString && ref->name().startsWith("#"))
	{
		Q_ASSERT(methodCall->arguments()->size() == 1);
		auto sLit = DCast<OOModel::StringLiteral>(methodCall->arguments()->first());
		Q_ASSERT(sLit);
		sLit->setValue(ref->name());
		auto newLit = new OOModel::ReferenceExpression(ref->name());
		methodCall->arguments()->replaceChild(sLit,
														  newLit);
		if (astMapping()->astMapping_.contains(sLit))
		{
			astMapping()->astMapping_.insert(newLit, astMapping()->astMapping_[sLit]);
			astMapping()->astMapping_.remove(sLit);
		}
		ref->setName("QString");
	}
}

void ExpansionManager::correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* reference)
{
	if (loc.isMacroID())
		reference->setName(getUnexpToken(loc).value());
}

void ExpansionManager::correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
																	 OOModel::ReferenceExpression* ref)
{
	if (!clang()->isMacroRange(specializationDecl->getSourceRange()))	return;

	auto spelling = clang()->getSpelling(specializationDecl->getLocation(),
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

OOModel::Expression* ExpansionManager::correctStringLiteral(clang::StringLiteral* strLit)
{
	if (strLit->getLocStart().isMacroID())
	{
		auto token = getUnexpToken(strLit->getLocStart());

		if (token.value() == "__FILE__")
			return new OOModel::MetaCallExpression(token.value());
	}

	return new OOModel::StringLiteral(QString::fromStdString(strLit->getBytes().str()));
}

OOModel::Expression* ExpansionManager::correctIntegerLiteral(clang::IntegerLiteral* intLit)
{
	if (intLit->getLocation().isMacroID())
	{
		auto token = getUnexpToken(intLit->getLocStart());

		if (token.value() == "__LINE__")
			return new OOModel::MetaCallExpression(token.value());
	}

	return new OOModel::IntegerLiteral(intLit->getValue().getLimitedValue());
}

void ExpansionManager::correctCastType(clang::Expr* expr, OOModel::CastExpression* cast)
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
			typeExpression.replace(typeExpression.length() - 1, 1, "");
			Q_ASSERT(!typeExpression.contains("*"));
			cast->setType(new OOModel::PointerTypeExpression(new OOModel::ReferenceExpression(typeExpression)));
		}
		else
			cast->setType(new OOModel::ReferenceExpression(typeExpression));
	}
}

void ExpansionManager::correctNamedDecl(clang::Decl* decl, Model::Node* node)
{
	if (!clang()->isMacroRange(decl->getSourceRange())) return;

	if (auto ooDecl = DCast<OOModel::Declaration>(node))
	{
		if (auto namedDecl = clang::dyn_cast<clang::NamedDecl>(decl))
			ooDecl->setName(getUnexpToken(namedDecl->getLocation()).value());
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
