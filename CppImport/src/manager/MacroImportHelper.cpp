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

#include "MacroImportHelper.h"

namespace CppImport {

void MacroImportHelper::setSourceManager(const clang::SourceManager* sourceManager)
{
	sourceManager_ = sourceManager;
}

void MacroImportHelper::setPreprocessor(const clang::Preprocessor* preprocessor)
{
	preprocessor_ = preprocessor;
}

void MacroImportHelper::addMacroDefinition(QString name, const clang::MacroDirective* md)
{
	definitions_[md] = name;
}

void MacroImportHelper::addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md,
													const clang::MacroArgs* args)
{
	auto entry = new ExpansionEntry();
	entry->range = sr;
	entry->definition = md;
	entry->parent = getExpansion(sr.getBegin());
	if (entry->parent) entry->parent->children.append(entry);
	entry->metaCall = new OOModel::MetaCallExpression(getDefinitionName(entry->definition));

	if (args)
	{
		for (auto i = 0; i < (int)args->getNumArguments() - 1; i++)
		{
			auto actualArg = args->getUnexpArgument((unsigned int)i);

			if (actualArg->getLocation().isInvalid()) continue;
			if (!actualArg->getIdentifierInfo()) continue;

			auto argText = QString::fromStdString(actualArg->getIdentifierInfo()->getName().str());
			entry->metaCall->arguments()->append(new OOModel::ReferenceExpression(argText + "YOLO"));

			entry->arguments.append(actualArg->getLocation());
		}
	}

	expansions_.append(entry);
}

void MacroImportHelper::mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode)
{
	if (auto bop = clang::dyn_cast<clang::BinaryOperator>(clangAstNode))
	{
		astMapping_[envisionAstNode] = clang::SourceRange(bop->getOperatorLoc(), bop->getOperatorLoc());
	}
	else if (auto strLiteral = clang::dyn_cast<clang::StringLiteral>(clangAstNode))
	{
		stringLiteralMapping_[envisionAstNode] = strLiteral;
		astMapping_[envisionAstNode] = strLiteral->getSourceRange();
	}
	else
		astMapping_[envisionAstNode] = clangAstNode->getSourceRange();
}

void MacroImportHelper::mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode)
{
	if (auto ooMethod = DCast<OOModel::Method>(envisionAstNode))
	{
		if (auto methodDecl = clang::dyn_cast<clang::FunctionDecl>(clangAstNode))
			astMapping_[ooMethod->nameNode()] = methodDecl->getNameInfo().getSourceRange();
		else
			Q_ASSERT(false);
	}

	astMapping_[envisionAstNode] = clangAstNode->getSourceRange();
}

QString MacroImportHelper::getDefinitionName(const clang::MacroDirective* md)
{
	if (!definitions_.contains(md)) return nullptr;

	return definitions_[md];
}

clang::SourceLocation MacroImportHelper::getImmediateMacroLoc(clang::SourceLocation Loc)
{
	if (Loc.isMacroID())
	{
		while (1)
		{
			auto FID = sourceManager_->getFileID(Loc);
			const clang::SrcMgr::SLocEntry *E = &sourceManager_->getSLocEntry(FID);
			const clang::SrcMgr::ExpansionInfo &Expansion = E->getExpansion();
			Loc = Expansion.getExpansionLocStart();
			if (!Expansion.isMacroArgExpansion())
				break;

			Loc = sourceManager_->getImmediateExpansionRange(Loc).first;
			auto SpellLoc = Expansion.getSpellingLoc();
			if (SpellLoc.isFileID())
				break;

			auto MacroFID = sourceManager_->getFileID(Loc);
			if (sourceManager_->isInFileID(SpellLoc, MacroFID))
				break;

			Loc = SpellLoc;
		}
	}

	return Loc;
}

QVector<clang::SourceLocation> MacroImportHelper::getMacroExpansionStack(clang::SourceLocation loc)
{
	QVector<clang::SourceLocation> result;
	clang::SourceLocation last;

	loc = getImmediateMacroLoc(loc);

	do
	{
		last = loc;
		result.append(loc);
		loc = getImmediateMacroLoc(loc);
	} while (last != loc);

	return result;
}

MacroImportHelper::ExpansionEntry* MacroImportHelper::getImmediateExpansion(clang::SourceLocation loc)
{
	auto expansion = getImmediateMacroLoc(loc);

	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->range.getBegin() == expansion) return expansions_[i];

	// TODO: this is for concatenation only
	auto retry = getImmediateMacroLoc(expansion);
	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->range.getBegin() == retry) return expansions_[i];

	return nullptr;
}

void MacroImportHelper::calculateMetaDefParents()
{
	for (auto expansion : expansions_)
		if (!metaDefParents_.contains(expansion->definition))
			metaDefParents_[expansion->definition]	= getMetaDefParent(expansion);
}

MacroImportHelper::ExpansionEntry* MacroImportHelper::getExpansion(clang::SourceLocation loc)
{
	MacroImportHelper::ExpansionEntry* expansion = getImmediateExpansion(loc);
	MacroImportHelper::ExpansionEntry* last = expansion;

	if (expansion)
	{
		do
		{
			last = expansion;
			loc = sourceManager_->getImmediateExpansionRange(loc).first;
			expansion = getImmediateExpansion(loc);
		} while (expansion && expansion->isChildOf(last));
	}

	return last;
}

QVector<MacroImportHelper::MacroArgumentLocation> MacroImportHelper::getArgumentLocation(clang::SourceRange range)
{
	QVector<MacroImportHelper::MacroArgumentLocation> result;

	if (sourceManager_->isMacroArgExpansion(range.getBegin()) &&
		 sourceManager_->isMacroArgExpansion(range.getEnd()))
	{
		QVector<clang::SourceLocation> spellingHistory;
		getImmediateSpellingHistory(range.getBegin(), &spellingHistory);

		for (auto argumentLoc : spellingHistory)
			for (auto expansion : expansions_)
				for (auto i = 0; i < expansion->arguments.size(); i++)
					if (expansion->arguments[i] == argumentLoc)
						result.append(MacroArgumentLocation(expansion, i));
	}

	return result;
}

void MacroImportHelper::getImmediateSpellingHistory(clang::SourceLocation loc, QVector<clang::SourceLocation>* result)
{
	result->append(loc);

	auto next = sourceManager_->getImmediateSpellingLoc(loc);

	if (next != loc)
		getImmediateSpellingHistory(next, result);
}

QVector<MacroImportHelper::MacroArgumentLocation> MacroImportHelper::getArgumentLocation(Model::Node* node)
{
	return getArgumentLocation(astMapping_[node]);
}

MacroImportHelper::ExpansionEntry* MacroImportHelper::getExpansion(Model::Node* node)
{
	if (!node) return nullptr;

	if (!expansionCache_.contains(node))
	{
		if (auto n = closestParentWithAstMapping(node))
		{
			expansionCache_[node] = getExpansion(astMapping_[n].getBegin());
		}
		else
			expansionCache_[node] = nullptr;
	}

	return expansionCache_[node];
}

Model::Node* MacroImportHelper::closestParentWithAstMapping(Model::Node* node)
{
	if (!node) return nullptr;
	if (astMapping_.contains(node)) return node;
	if (node->parent()) return closestParentWithAstMapping(node->parent());

	return nullptr;
}

void MacroImportHelper::getAllNodes(ExpansionEntry* expansion, QVector<Model::Node*>* result)
{
	auto nodes = getNodes(expansion);

	if (!nodes.empty())
	{
		for (auto node : nodes)
			result->append(node);
	}
	else
	{
		for (auto child : expansion->children)
			getAllNodes(child, result);
	}
}

QVector<MacroImportHelper::ExpansionEntry*> MacroImportHelper::getTopLevelExpansions()
{
	QVector<MacroImportHelper::ExpansionEntry*> result;
	for (auto expansion : expansions_)
		if (!expansion->parent)
			result.append(expansion);

	return result;
}

QVector<Model::Node*> MacroImportHelper::getNodes(MacroImportHelper::ExpansionEntry* expansion)
{
	Q_ASSERT(expansion);

	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : astMapping_.keys())
		if (getExpansion(node) == expansion)
		{
			allNodesForExpansion.append(node);
			topLevel.insert(node);
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

void MacroImportHelper::nodeReplaced(Model::Node* node, Model::Node* replacement)
{
	if (!astMapping_.contains(node)) return;

	auto range = astMapping_[node];
	astMapping_.remove(node);
	expansionCache_.remove(node);
	astMapping_[replacement] = range;
}

QVector<OOModel::FormalMetaArgument*> MacroImportHelper::generateFormalArguments(
																										const clang::MacroDirective* definition)
{
	QVector<OOModel::FormalMetaArgument*> result;

	for (auto argName : getArgumentNames(definition))
		result.append(new OOModel::FormalMetaArgument(argName));

	return result;
}

QVector<QString> MacroImportHelper::getArgumentNames(const clang::MacroDirective* definition)
{
	QVector<QString> result;

	for (auto i = definition->getMacroInfo()->arg_begin(); i != definition->getMacroInfo()->arg_end(); i++)
		result.append(QString::fromStdString((*i)->getName().str()));

	return result;
}

OOModel::Declaration* MacroImportHelper::getMetaDefParent(ExpansionEntry* expansion)
{
	for (auto child : expansion->children)
		if (!getNodes(child).empty())
			return getNodes(child).first()->firstAncestorOfType<OOModel::Project>();

	while (getNodes(expansion).empty()) expansion = expansion->parent;
	return getNodes(expansion).first()->firstAncestorOfType<OOModel::Project>();
}

void MacroImportHelper::createMetaDef(QVector<Model::Node*> nodes, ExpansionEntry* expansion)
{
	auto metaDefName = getDefinitionName(expansion->definition);
	if (metaDefinitions_.contains(metaDefName)) return;

	auto metaDef = new OOModel::MetaDefinition(metaDefName);
	metaDefinitions_[metaDefName] = metaDef;

	auto metaDefParent = metaDefParents_[expansion->definition];

	for (auto formalArgument : generateFormalArguments(expansion->definition))
		metaDef->arguments()->append(formalArgument);

	if (nodes.size() > 0)
	{
		metaDef->setContext(createContext(nodes.first()));

		for (auto n : nodes)
		{
			auto cloned = cloneRetainingMetaCallExpansionMapping(n);

			if (auto ooExpression = DCast<OOModel::Expression>(cloned))
			{
				if (auto context = DCast<OOModel::Method>(metaDef->context()))
					context->items()->append(new OOModel::ExpressionStatement(ooExpression));
				else
					Q_ASSERT(false);
			}
			else if (auto ooDeclaration = DCast<OOModel::Declaration>(cloned))
			{
				if (auto context = DCast<OOModel::Project>(metaDef->context()))
					context->subDeclarations()->append(ooDeclaration);
				else if (auto context = DCast<OOModel::Module>(metaDef->context()))
					context->subDeclarations()->append(ooDeclaration);
				else if (auto context = DCast<OOModel::Class>(metaDef->context()))
					context->subDeclarations()->append(ooDeclaration);
				else if (auto context = DCast<OOModel::Method>(metaDef->context()))
					context->subDeclarations()->append(ooDeclaration);
				else
					Q_ASSERT(false);
			}
		}
	}
	else
	{
		auto childCall = expansion->children.first()->metaCall;

		childCall->parent()->replaceChild(childCall, expansion->metaCall);
		nodeReplaced(childCall, expansion->metaCall);

		auto context = new OOModel::Method("Context");
		for (auto childExpansion : expansion->children)
		{
			context->items()->append(new OOModel::ExpressionStatement(childExpansion->metaCall));
		}

		metaDef->setContext(context);
	}

	metaDefParent->subDeclarations()->append(metaDef);
}

MacroImportHelper::ExpansionEntry* MacroImportHelper::getExpansion(OOModel::MetaCallExpression* metaCall)
{
	for (auto expansion : expansions_)
		if (expansion->metaCall == metaCall)
			return expansion;

	return nullptr;
}

OOModel::Declaration* MacroImportHelper::getActualContext(Model::Node* node)
{
	auto current = node->parent();

	while (current)
	{
		if (auto result = DCast<OOModel::Project>(current))
			return result;
		else if (auto result = DCast<OOModel::Module>(current))
			return result;
		else if (auto result = DCast<OOModel::Class>(current))
			return result;
		else if (auto result = DCast<OOModel::Method>(current))
			return result;
		else
			current = current->parent();
	}

	Q_ASSERT(false);
}


OOModel::Declaration* MacroImportHelper::createContext(Model::Node* node)
{
	auto actualContext = getActualContext(node);

	if (DCast<OOModel::Project>(actualContext))
		return new OOModel::Project("Context");
	else if (DCast<OOModel::Module>(actualContext))
		return new OOModel::Module("Context");
	else if (DCast<OOModel::Class>(actualContext))
		return new OOModel::Class("Context");
	else if (DCast<OOModel::Method>(actualContext))
		return new OOModel::Method("Context");

	Q_ASSERT(false);
}

Model::Node* MacroImportHelper::cloneRetainingMetaCallExpansionMapping(Model::Node* node)
{
	QList<MacroImportHelper::ExpansionEntry*> info;
	buildMetaCallExpansionMappingInfo(node, &info);

	auto clone = node->clone();

	useMetaCallExpansionMappingInfo(clone, &info);
	Q_ASSERT(info.size() == 0);

	return clone;
}

void MacroImportHelper::buildMetaCallExpansionMappingInfo(Model::Node* node,
																			  QList<MacroImportHelper::ExpansionEntry*>* info)
{
	if (auto metaCall = DCast<OOModel::MetaCallExpression>(node))
	{
		auto expansion = getExpansion(metaCall);
		Q_ASSERT(expansion);

		info->push_back(expansion);
	}

	for (auto child : node->children())
		buildMetaCallExpansionMappingInfo(child, info);
}

void MacroImportHelper::useMetaCallExpansionMappingInfo(Model::Node* node,
																			  QList<MacroImportHelper::ExpansionEntry*>* info)
{
	if (auto metaCall = DCast<OOModel::MetaCallExpression>(node))
	{
		Q_ASSERT(info->size() > 0);

		auto expansion = info->front();
		info->pop_front();
		expansion->metaCall = metaCall;
	}

	for (auto child : node->children())
		useMetaCallExpansionMappingInfo(child, info);
}

std::pair<clang::SourceLocation, clang::SourceLocation>
MacroImportHelper::getStringLiteralSpellingLoc(clang::StringLiteral* stringLiteral)
{
	auto s = stringLiteral->getLocStart();
	auto se = sourceManager_->getImmediateExpansionRange(s).first;
	if (se.isMacroID()) s = se;

	auto e = stringLiteral->getLocEnd();
	auto ee = sourceManager_->getImmediateExpansionRange(e).second;
	if (ee.isMacroID()) e = ee;

	return std::make_pair(sourceManager_->getSpellingLoc(s), sourceManager_->getSpellingLoc(e));
}

QString MacroImportHelper::getSpelling(clang::SourceLocation start, clang::SourceLocation end)
{
	clang::SourceLocation b(start), _e(end);
	clang::SourceLocation e(clang::Lexer::getLocForEndOfToken(_e, 0, *sourceManager_, preprocessor_->getLangOpts()));

	auto length = sourceManager_->getCharacterData(e) -	sourceManager_->getCharacterData(b);

	return length > 0 ? QString::fromStdString(std::string(sourceManager_->getCharacterData(b), length)) : "";
}

void MacroImportHelper::handleIdentifierConcatentation(Model::Node* node)
{
	if (astMapping_.contains(node))
	{
		if (DCast<OOModel::ReferenceExpression>(node) ||
			 DCast<Model::NameText>(node))
		{
			auto s = sourceManager_->getImmediateExpansionRange(astMapping_[node].getBegin()).first;
			auto e = sourceManager_->getImmediateExpansionRange(astMapping_[node].getEnd()).second;

			auto s1 = sourceManager_->getSpellingLoc(s);
			auto e1 = sourceManager_->getSpellingLoc(e);

			auto value = getSpelling(s1, e1);

			if (value.contains("##"))
			{
				if (auto ooReference = DCast<OOModel::ReferenceExpression>(node))
					ooReference->setName(value);
				else if (auto name = DCast<Model::NameText>(node))
					name->set(value);
			}
		}
	}

	for (auto child : node->children())
		handleIdentifierConcatentation(child);
}

void MacroImportHelper::handleStringifycation(Model::Node* node)
{
	if (stringLiteralMapping_.contains(node))
	{
		auto stringLiteral = stringLiteralMapping_[node];

		auto loc = getStringLiteralSpellingLoc(stringLiteral);

		auto value = getSpelling(loc.first, loc.second);

		if (auto ooStringLiteral = DCast<OOModel::StringLiteral>(node))
			ooStringLiteral->setValue(value);
		else
			Q_ASSERT(false);
	}

	for (auto child : node->children())
		handleStringifycation(child);
}

QString MacroImportHelper::print(clang::SourceLocation loc)
{
	return (loc.isFileID() ? QString("F") : QString("")) +
			(loc.isMacroID() ? QString("M") : QString("")) + (loc.isValid() ? QString("V") : QString(""));
}

void MacroImportHelper::getAllArguments(Model::Node* node,
									QVector<std::pair<QVector<MacroImportHelper::MacroArgumentLocation>, Model::Node*>>* result)
{
	auto argLoc = getArgumentLocation(node);

	if (!argLoc.empty())
	{
		result->append(make_pair(argLoc, node));
		return;
	}

	for (auto child : node->children())
		getAllArguments(child, result);
}

bool MacroImportHelper::ExpansionEntry::isChildOf(MacroImportHelper::ExpansionEntry* entry)
{
	auto current = this;
	while (current && current != entry)
		current = current->parent;
	return current;
}

}
