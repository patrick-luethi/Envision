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

bool MacroImportHelper::isIncompleteDefinition(const clang::MacroDirective* md)
{
	auto name = getDefinitionName(md);
	return name.endsWith("_BEGIN") || name.endsWith("_END");
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

	for (auto i = 0; i < getArgumentNames(entry->definition).size(); i++)
	{
		auto actualArg = args->getUnexpArgument((unsigned int)i);

		QString unexpandedName;
		if (getUnexpandedNameWithQualifiers(actualArg->getLocation(), &unexpandedName))
		{
			entry->metaCall->arguments()->append(new OOModel::ReferenceExpression(unexpandedName));
		}
		else if (actualArg->getIdentifierInfo())
		{
			//auto argText = QString::fromStdString(actualArg->getIdentifierInfo()->getName().str());
			entry->metaCall->arguments()->append(new OOModel::ReferenceExpression("argText"));
		}
		else
			entry->metaCall->arguments()->append(new OOModel::EmptyExpression());

		entry->argumentLocs.append(actualArg->getLocation());
	}

	expansions_.append(entry);
}

void MacroImportHelper::mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode)
{
	if (auto bop = clang::dyn_cast<clang::BinaryOperator>(clangAstNode))
	{
		astMapping_[envisionAstNode].append(clang::SourceRange(bop->getOperatorLoc(), bop->getOperatorLoc()));
	}
	else if (auto strLiteral = clang::dyn_cast<clang::StringLiteral>(clangAstNode))
	{
		stringLiteralMapping_[envisionAstNode] = strLiteral;
		astMapping_[envisionAstNode].append(strLiteral->getSourceRange());
	}
	else
	{
		astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
	}
}

void MacroImportHelper::mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode)
{
	handleIdentifierConcatentation(clangAstNode, envisionAstNode);

	if (!astMapping_[envisionAstNode].contains(clangAstNode->getSourceRange()))
	{
		astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
	}
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
			if (!E->isExpansion())
				break;
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
	{
		auto name = getDefinitionName(expansion->definition);

		if (!metaDefParents_.contains(name)) // TODO: should maybe use directives as keys
			metaDefParents_[name] = getMetaDefParent(expansion);
	}
}

bool MacroImportHelper::getUnexpandedCode(clang::SourceLocation loc, QString* result,
														clang::SourceLocation* outStart, clang::SourceLocation* outEnd)
{
	return getUnexpandedCode(loc, loc, result, outStart, outEnd);
}

bool MacroImportHelper::getUnexpandedCode(clang::SourceRange range, QString* result,
														clang::SourceLocation* outStart, clang::SourceLocation* outEnd)
{
	return getUnexpandedCode(range.getEnd(), range.getBegin(), result, outStart, outEnd);
}

bool MacroImportHelper::getUnexpandedNameWithQualifiers(clang::SourceLocation loc, QString* result)
{
	clang::SourceLocation current;
	if (!getUnexpandedCode(loc, result, nullptr, &current))
	{
		*result = getSpelling(loc);
		current = getLocForEndOfToken(loc);
	}

	QRegularExpression regularExpression("^\\w*$");
	auto match = regularExpression.match(*result);
	if (!match.hasMatch())
	{
		*result = "";
		return false;
	}

	while (true)
	{
		auto sepa = getSpelling(current);
		if (!nameSeparator(sepa))
			break;
		current = getLocForEndOfToken(current);

		auto identifier = getSpelling(current);
		if (identifier == ",")
			break;

		*result += sepa + identifier;
		current = getLocForEndOfToken(current);
	}

	return true;
}

bool MacroImportHelper::nameSeparator(QString candidate)
{
	return candidate == "::" || candidate == "." || candidate == "<" || candidate == ">";
}

bool MacroImportHelper::getUnexpandedCode(clang::SourceLocation start, clang::SourceLocation end, QString* result,
														clang::SourceLocation* outStart, clang::SourceLocation* outEnd)
{
	auto s = sourceManager_->getImmediateExpansionRange(start).first;
	auto immediateExpansion = getImmediateExpansion(start);

	if (immediateExpansion &&
		 s != immediateExpansion->range.getBegin())
	{
		auto e = sourceManager_->getImmediateExpansionRange(end).second;

		*result = getSpelling(s, e);

		if (outStart) *outStart = sourceManager_->getSpellingLoc(s);
		if (outEnd) *outEnd = getLocForEndOfToken(sourceManager_->getSpellingLoc(e));

		return true;
	}

	return false;
}

void MacroImportHelper::calculateMetaCallArguments()
{
	/*for (auto expansion : expansions_)
		for (auto i = 0; i < expansion->argumentLocs.size(); i++)
			if (auto ooReference = DCast<OOModel::ReferenceExpression>(expansion->metaCall->arguments()->at(i)))
			{
				continue;
				QString unexpandedCode;
				if (getUnexpandedCode(expansion->argumentLocs[i], &unexpandedCode))
				{
					//qDebug() << unexpandedCode << ooReference;
					//ooReference->setName(unexpandedCode);
				}
			}*/
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

QVector<MacroImportHelper::MacroArgumentLocation> MacroImportHelper::getArgumentHistory(clang::SourceRange range)
{
	QVector<MacroArgumentLocation> result;

	if (sourceManager_->isMacroArgExpansion(range.getBegin()) &&
		 sourceManager_->isMacroArgExpansion(range.getEnd()))
	{
		QVector<clang::SourceLocation> spellingHistory;
		getImmediateSpellingHistory(range.getBegin(), &spellingHistory);

		for (auto argumentLoc : spellingHistory)
			for (auto expansion : expansions_)
				for (auto i = 0; i < expansion->argumentLocs.size(); i++)
					if (expansion->argumentLocs[i] == argumentLoc)
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

QVector<MacroImportHelper::MacroArgumentLocation> MacroImportHelper::getArgumentHistory(Model::Node* node)
{
	QVector<MacroArgumentLocation> result;
	if (astMapping_.contains(node))
			result = getArgumentHistory(astMapping_[node].first());
	return result;
}

QSet<MacroImportHelper::ExpansionEntry*> MacroImportHelper::getExpansion(Model::Node* node)
{
	if (!node) return {}; //TODO: necessary?

	if (!expansionCache_.contains(node))
	{
		expansionCache_[node] = {};

		if (auto n = closestParentWithAstMapping(node))
			if (astMapping_.contains(n))
				for (auto range : astMapping_[n])
				{
					auto expansion = getExpansion(range.getBegin());
					if (expansion)	expansionCache_[node].insert(expansion);
				}
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

QVector<Model::Node*> MacroImportHelper::getAllNodes(ExpansionEntry* expansion)
{
	QVector<Model::Node*> result;
	getAllNodes(expansion, &result);
	return result;
}

void MacroImportHelper::getAllNodes(ExpansionEntry*, QVector<Model::Node*>*)
{
	/*auto nodes = getNodes(expansion);

	if (!nodes.empty())
	{
		for (auto node : nodes)
			result->append(node);
	}
	else
	{
		for (auto child : expansion->children)
			getAllNodes(child, result);
	}*/
}

QVector<MacroImportHelper::ExpansionEntry*> MacroImportHelper::getTopLevelExpansions()
{
	QVector<MacroImportHelper::ExpansionEntry*> result;
	for (auto expansion : expansions_)
		if (!expansion->parent)
			result.append(expansion);

	return result;
}

QVector<Model::Node*> MacroImportHelper::getTopLevelNodes(MacroImportHelper::ExpansionEntry* expansion)
{
	Q_ASSERT(!expansion->parent);

	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : astMapping_.keys())
	{
		for (auto range : astMapping_[node])
			if (sourceManager_->getExpansionLoc(range.getBegin()) == expansion->range.getBegin())
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

void MacroImportHelper::getChildrenNotBelongingToExpansion(Model::Node* node,
																								MacroImportHelper::ExpansionEntry* expansion,
																								NodeMapping* mapping,
																								QVector<Model::Node*>* result)
{
	Q_ASSERT(expansion);

	if (getExpansion(mapping->original(node)).contains(expansion))
	{
		for (auto child : node->children())
		{
			getChildrenNotBelongingToExpansion(child, expansion, mapping, result);
		}
	}
	else
	{
		result->append(node);
	}
}

QVector<Model::Node*> MacroImportHelper::getNodes(MacroImportHelper::ExpansionEntry* expansion,
																  NodeMapping* mapping)
{
	Q_ASSERT(expansion);

	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : astMapping_.keys())
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

	QVector<Model::Node*> result;
	for (auto node : topLevel)
		result.append(mapping->clone(node));

	return result;
}

QVector<Model::Node*> MacroImportHelper::getNodesWithNoAncestor(QVector<Model::Node*> nodes)
{
	QSet<Model::Node*> nodesWithNoAncestor;
	for (auto node : nodes)
		nodesWithNoAncestor.insert(node);

	for (auto node : nodes)
		for (auto other : nodes)
			if (node != other)
				if (node->isAncestorOf(other))
					nodesWithNoAncestor.remove(other);

	QVector<Model::Node*> result;
	for (auto node : nodesWithNoAncestor)
		result.append(node);

	return result;
}

QVector<Model::Node*> MacroImportHelper::getClones(QVector<Model::Node*> nodes, NodeMapping* mapping)
{
	QVector<Model::Node*> result;
	for (auto node : nodes)
		result.append(mapping->clone(node));
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

QVector<QString> MacroImportHelper::getArgumentNames(const clang::MacroDirective* definition)
{
	QVector<QString> result;

	for (auto i = definition->getMacroInfo()->arg_begin(); i != definition->getMacroInfo()->arg_end(); i++)
		result.append(QString::fromStdString((*i)->getName().str()));

	return result;
}

OOModel::Declaration* MacroImportHelper::getMetaDefParent(ExpansionEntry*)
{
	return root;

	/*for (auto child : expansion->children)
		if (!getNodes(child).empty())
			return getNodes(child).first()->firstAncestorOfType<OOModel::Project>();

	while (getNodes(expansion).empty()) expansion = expansion->parent;
	return getNodes(expansion).first()->firstAncestorOfType<OOModel::Project>();*/
}

void MacroImportHelper::createMetaDef(QVector<Model::Node*> nodes, ExpansionEntry* expansion,
												  NodeMapping* mapping)
{
	auto metaDefName = getDefinitionName(expansion->definition);
	if (metaDefinitions_.contains(metaDefName)) return;

	auto metaDef = new OOModel::MetaDefinition(metaDefName);
	metaDefinitions_[metaDefName] = metaDef;

	auto metaDefParent = metaDefParents_[metaDefName];

	for (auto argName : getArgumentNames(expansion->definition))
		metaDef->arguments()->append(new OOModel::FormalMetaArgument(argName));

	if (nodes.size() > 0)
	{
		auto actualContext = getActualContext(mapping->original(nodes.first()));

		metaDef->setContext(createContext(actualContext));

		for (auto n : nodes)
		{
			NodeMapping childMapping;
			//auto cloned = cloneWithMapping(mapping->original(n), &childMapping);
			auto cloned = cloneWithMapping(n, mapping, &childMapping);

			QVector<Model::Node*> tbrs;
			getChildrenNotBelongingToExpansion(cloned, expansion, &childMapping, &tbrs);

			if (tbrs.contains(cloned)) continue;

			for (auto tbr : tbrs)
				removeNode(tbr);

			if (auto ooExpression = DCast<OOModel::Expression>(cloned))
			{
				if (auto context = DCast<OOModel::Method>(metaDef->context()))
					context->items()->append(new OOModel::ExpressionStatement(ooExpression));
				else
				{
					if (DCast<OOModel::ReferenceExpression>(cloned))
					{
						continue; // TODO: skips important stuff has to be removed
					}
					Q_ASSERT(false);
				}
			}
			else if (auto ooStatement = DCast<OOModel::Statement>(cloned))
			{
				if (auto context = DCast<OOModel::Method>(metaDef->context()))
					context->items()->append(ooStatement);
				else
					Q_ASSERT(false);
			}
			else if (auto ooDeclaration = DCast<OOModel::Declaration>(cloned))
			{
				if (auto ooClass = DCast<OOModel::Class>(cloned))
				{
					if (auto context = DCast<OOModel::Project>(metaDef->context()))
						context->classes()->append(ooClass);
					else if (auto context = DCast<OOModel::Module>(metaDef->context()))
						context->classes()->append(ooClass);
					else if (auto context = DCast<OOModel::Class>(metaDef->context()))
						context->classes()->append(ooClass);
					else
						Q_ASSERT(false);
				}
				else if (auto ooField = DCast<OOModel::Field>(cloned))
				{
					if (auto context = DCast<OOModel::Project>(metaDef->context()))
						context->fields()->append(ooField);
					else if (auto context = DCast<OOModel::Module>(metaDef->context()))
						context->fields()->append(ooField);
					else if (auto context = DCast<OOModel::Class>(metaDef->context()))
						context->fields()->append(ooField);
					else
						Q_ASSERT(false);
				}
				else if (auto ooMethod = DCast<OOModel::Method>(cloned))
				{
					if (auto context = DCast<OOModel::Project>(metaDef->context()))
						context->methods()->append(ooMethod);
					else if (auto context = DCast<OOModel::Module>(metaDef->context()))
						context->methods()->append(ooMethod);
					else if (auto context = DCast<OOModel::Class>(metaDef->context()))
						context->methods()->append(ooMethod);
					else
						Q_ASSERT(false);
				}
				else
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
			else
				Q_ASSERT(false && "not implemented");
		}
	}

	for (auto childExpansion : expansion->children)
	{
		metaDef->context()->metaCalls()->append(childExpansion->metaCall);
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

OOModel::Declaration* MacroImportHelper::getActualContext(ExpansionEntry* expansion)
{
	Q_ASSERT(!expansion->parent);

	QVector<OOModel::Declaration*> candidates;
	for (auto i = astMapping_.begin(); i != astMapping_.end(); i++)
		for (auto range : i.value())
			if (contains(range, expansion->range))
				if (validContext(i.key()))
				{
					candidates.append(DCast<OOModel::Declaration>(i.key()));
					break;
				}

	if (candidates.empty())
	{
		qDebug() << "ROOT";
		return root;
	}

	auto result = candidates.first();

	for (auto candidate : candidates)
		if (result->isAncestorOf(candidate))
			result = candidate;

	return result;
}

bool MacroImportHelper::contains(clang::SourceRange range, clang::SourceRange other)
{
	auto s = sourceManager_->getSpellingLoc(range.getBegin()).getPtrEncoding();
	auto e = sourceManager_->getSpellingLoc(range.getEnd()).getPtrEncoding();
	auto os = sourceManager_->getSpellingLoc(other.getBegin()).getPtrEncoding();
	//auto oe = sourceManager_->getSpellingLoc(other.getEnd()).getPtrEncoding();

	return s <= os && os <= e;
	/*return !sourceManager_->isBeforeInTranslationUnit(other.getBegin(), range.getBegin()) &&
				!sourceManager_->isBeforeInTranslationUnit(range.getEnd(), other.getEnd());*/
}

bool MacroImportHelper::validContext(Model::Node* node)
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
	return createContext(getActualContext(node));
}

OOModel::Declaration* MacroImportHelper::createContext(OOModel::Declaration* actualContext)
{
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

Model::Node* MacroImportHelper::cloneWithMapping(Model::Node* node, NodeMapping* mapping)
{
	auto clone = node->clone();

	QList<Model::Node*> info;
	buildMappingInfo(node, &info);
	useMappingInfo(clone, &info, mapping);

	return clone;
}

Model::Node* MacroImportHelper::cloneWithMapping(Model::Node* node, NodeMapping* master, NodeMapping* mapping)
{
	auto clone = node->clone();

	QList<Model::Node*> info;
	buildMappingInfo(node, &info, master);
	useMappingInfo(clone, &info, mapping);

	return clone;
}

void MacroImportHelper::buildMappingInfo(Model::Node* node, QList<Model::Node*>* info, NodeMapping* master)
{
	info->push_back(master->original(node));

	for (auto child : node->children())
		buildMappingInfo(child, info);
}

void MacroImportHelper::buildMappingInfo(Model::Node* node, QList<Model::Node*>* info)
{
	info->push_back(node);

	for (auto child : node->children())
		buildMappingInfo(child, info);
}

void MacroImportHelper::useMappingInfo(Model::Node* node,
													  QList<Model::Node*>* info,
													  NodeMapping* mapping)
{
	mapping->add(info->front(), node);
	info->pop_front();

	for (auto child : node->children())
		useMappingInfo(child, info, mapping);
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

	return std::make_pair(s, e);
}

QString MacroImportHelper::getSpelling(clang::SourceLocation loc)
{
	return getSpelling(loc, loc);
}

QString MacroImportHelper::getSpelling(clang::SourceRange range)
{
	return getSpelling(range.getBegin(), range.getEnd());
}

QString MacroImportHelper::getSpellingField(clang::SourceLocation start)
{
	auto end = start.getLocWithOffset(1);
	while (!getSpelling(start, end).endsWith(";") && getSpelling(start, end) != "SPELLING_ERROR")
		end = end.getLocWithOffset(1);
	return getSpelling(start, end);
}

clang::SourceLocation MacroImportHelper::getLocForEndOfToken(clang::SourceLocation loc)
{
	return clang::Lexer::getLocForEndOfToken(loc, 0, *sourceManager_, preprocessor_->getLangOpts());
}

QString MacroImportHelper::getSpelling(clang::SourceLocation start, clang::SourceLocation end)
{
	clang::SourceLocation b = sourceManager_->getSpellingLoc(start);
	clang::SourceLocation e = getLocForEndOfToken(sourceManager_->getSpellingLoc(end));

	auto length = sourceManager_->getCharacterData(e) - sourceManager_->getCharacterData(b);

	return 0 < length ?
				QString::fromStdString(std::string(sourceManager_->getCharacterData(b), length)) : "";
}

void MacroImportHelper::correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* arg)
{
	QString unexpandedCode;
	clang::SourceLocation s, e;

	if (getUnexpandedCode(namedDecl->getSourceRange().getBegin(), &unexpandedCode, &s, &e))
	{
		auto nextToken = getSpelling(e, e);

		if (nextToken == "*")
			arg->setTypeExpression(new OOModel::PointerTypeExpression(new OOModel::ReferenceExpression(unexpandedCode)));
		else
			arg->setTypeExpression(new OOModel::ReferenceExpression(unexpandedCode));
	}
}

void MacroImportHelper::correctFormalResultType(clang::FunctionDecl* method, OOModel::Method* ooMethod)
{
	QString unexpandedCode;
	clang::SourceLocation e;
	if (getUnexpandedCode(method->getReturnTypeSourceRange().getBegin(), &unexpandedCode, nullptr, &e))
	{
		ooMethod->results()->clear();

		OOModel::FormalResult* methodResult = new OOModel::FormalResult();

		auto nextToken = getSpelling(e, e);

		if (nextToken == "*")
			methodResult->setTypeExpression(new OOModel::PointerTypeExpression(
														  new OOModel::ReferenceExpression(unexpandedCode)));
		else
			methodResult->setTypeExpression(new OOModel::ReferenceExpression(unexpandedCode));

		ooMethod->results()->append(methodResult);
	}
}

void MacroImportHelper::correctMethodCall(clang::Expr* expr, OOModel::MethodCallExpression* methodCall)
{
	if (!expr->getExprLoc().isMacroID()) return;

	auto ref = DCast<OOModel::ReferenceExpression>(methodCall->callee());
	if (!ref) return;

	QStack<OOModel::ReferenceExpression*> refs;
	while (true)
	{
		for (auto i = ref->typeArguments()->size() - 1; i >= 0; i--)
		{
			if (auto r = DCast<OOModel::ReferenceExpression>(ref->typeArguments()->at(i)))
			{
				refs.push(r);
			}
			else
			{
				qDebug() << "could not correct methodcall" << methodCall;
				return;
			}
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

	QString unexpandedCode;
	clang::SourceLocation e;
	if (!getUnexpandedCode(expr->getExprLoc(), &unexpandedCode, nullptr, &e))
	{
		e = sourceManager_->getSpellingLoc(expr->getExprLoc());
		unexpandedCode = getSpelling(e, e);
		e = getLocForEndOfToken(sourceManager_->getSpellingLoc(e));
	}

	if (nameSeparator(unexpandedCode))
	{
		unexpandedCode = getSpelling(e, e);
		e = getLocForEndOfToken(sourceManager_->getSpellingLoc(e)); // next separator
	}

	refs.pop()->setName(unexpandedCode);

	while (!refs.empty())
	{
		QString next;
		do
		{
			e = getLocForEndOfToken(sourceManager_->getSpellingLoc(e)); // skip separator
			next = getSpelling(e, e);
		}
		while (nameSeparator(next));

		refs.pop()->setName(next);
	}
}

void MacroImportHelper::correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* reference)
{
	QString unexpandedCode;
	if (getUnexpandedCode(loc, &unexpandedCode))
	{
		reference->setName(unexpandedCode);
	}
}

void MacroImportHelper::correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
																	 OOModel::ReferenceExpression* ref)
{
	if (!specializationDecl->getSourceRange().getBegin().isMacroID() ||
		 !specializationDecl->getSourceRange().getEnd().isMacroID())
		return;

	auto spelling = getSpelling(specializationDecl->getLocation(), specializationDecl->getSourceRange().getEnd());

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

OOModel::Expression* MacroImportHelper::correctIntegerLiteral(clang::IntegerLiteral* intLit)
{
	if (intLit->getLocation().isMacroID())
	{
		QString unexpandedCode;
		if (getUnexpandedCode(intLit->getLocation(), &unexpandedCode))
			if (unexpandedCode.startsWith("_"))
				return new OOModel::ReferenceExpression(unexpandedCode);
	}

	return new OOModel::IntegerLiteral(intLit->getValue().getLimitedValue());
}

void MacroImportHelper::correctCastType(clang::Expr* expr, OOModel::CastExpression* cast)
{
	if (!expr->getSourceRange().getBegin().isMacroID() ||
		 !expr->getSourceRange().getEnd().isMacroID()) return;

	auto spelling = getSpelling(expr->getSourceRange());

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

void MacroImportHelper::handleIdentifierConcatentation(clang::Decl* decl, Model::Node* node)
{
	if (!decl->getSourceRange().getBegin().isMacroID() ||
		 !decl->getSourceRange().getEnd().isMacroID()) return;

	if (auto ooDecl = DCast<OOModel::Declaration>(node))
	{
		if (auto namedDecl = clang::dyn_cast<clang::NamedDecl>(decl))
		{
			QString unexpandedCode;
			if (getUnexpandedCode(namedDecl->getLocation(), &unexpandedCode))
				ooDecl->setName(unexpandedCode);
		}
		else
			Q_ASSERT(false && "not implemented");
	}
	else
		Q_ASSERT(false && "not implemented");
}

void MacroImportHelper::handleIdentifierConcatentation(Model::Node* node)
{
	return;

	if (astMapping_.contains(node))
	{
		if (auto ooReference = DCast<OOModel::ReferenceExpression>(node))
		{
			QString unexpandedCode;
			if (getUnexpandedCode(astMapping_[node].first(), &unexpandedCode))
				ooReference->setName("PLACEHOLDER");
		}
	}

	for (auto child : node->children())
		handleIdentifierConcatentation(child);
}

void MacroImportHelper::handleStringifycation(Model::Node* node, NodeMapping* mapping)
{
	if (stringLiteralMapping_.contains(mapping->original(node)))
	{
		auto stringLiteral = stringLiteralMapping_[mapping->original(node)];

		auto loc = getStringLiteralSpellingLoc(stringLiteral);

		auto value = getSpelling(loc.first, loc.second);

		if (auto ooStringLiteral = DCast<OOModel::StringLiteral>(node))
			ooStringLiteral->setValue(value);
		else
			Q_ASSERT(false);
	}

	for (auto child : node->children())
		handleStringifycation(child, mapping);
}

void MacroImportHelper::getAllArguments(Model::Node* node, QVector<MacroArgumentInfo>* result, NodeMapping* mapping)
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

bool MacroImportHelper::ExpansionEntry::isChildOf(MacroImportHelper::ExpansionEntry* entry)
{
	auto current = this;
	while (current && current != entry)
		current = current->parent;
	return current;
}

void MacroImportHelper::clear()
{
	definitions_.clear();
	metaDefParents_.clear();
	//metaDefinitions_.clear();
	astMapping_.clear();
	stringLiteralMapping_.clear();
	expansionCache_.clear();
	expansions_.clear();
}

void MacroImportHelper::setProject(OOModel::Project* project)
{
	root = project;
}

void MacroImportHelper::setTranslUnit(QString v)
{
	translUnit_ = v;
}

QString MacroImportHelper::getNamedDeclName(clang::NamedDecl* decl)
{
	auto loc = decl->getLocation();

	if (loc.isMacroID())
	{
		QString unexpandedCode;
		if (getUnexpandedCode(loc, &unexpandedCode))
			return unexpandedCode;
	}

	return QString::fromStdString(decl->getNameAsString());
}

void MacroImportHelper::macroGeneration()
{
	calculateMetaDefParents();
	calculateMetaCallArguments();

	for (auto expansion : getTopLevelExpansions())
	{
		NodeMapping mapping;
		QVector<Model::Node*> generatedNodes;
		for (auto node : getTopLevelNodes(expansion))
			generatedNodes.append(cloneWithMapping(node, &mapping));

		for (auto generatedNode : generatedNodes)
			handleStringifycation(generatedNode, &mapping);

		QVector<MacroArgumentInfo> allArguments;
		for (auto node : generatedNodes)
			getAllArguments(node, &allArguments, &mapping);

		for (auto argument : allArguments)
		{
			auto spliceLoc = argument.history.first();

			auto argName = getArgumentNames(spliceLoc.expansion->definition).at(spliceLoc.argumentNumber);
			auto newNode = new OOModel::ReferenceExpression(argName);

			argument.node->parent()->replaceChild(argument.node, newNode);
			mapping.add(mapping.original(argument.node), newNode);
		}

		handleMacroExpansion(generatedNodes, expansion, &mapping);

		for (auto argument : allArguments)
		{
			if (argument.history.empty()) continue;

			for (auto i = 0; i < argument.history.size() - 1; i++)
			{
				auto currentLoc = argument.history[i];
				auto nextLoc = argument.history[i + 1];

				auto currentArg = currentLoc.expansion->metaCall->arguments()->at(currentLoc.argumentNumber);
				auto newArgValue = getArgumentNames(nextLoc.expansion->definition).at(nextLoc.argumentNumber);
				auto newArg = new OOModel::ReferenceExpression(newArgValue);

				currentLoc.expansion->metaCall->arguments()->replaceChild(currentArg, newArg);
			}

			auto lastLoc = argument.history.last();
			auto currentArg = lastLoc.expansion->metaCall->arguments()->at(lastLoc.argumentNumber);
			auto newArg = argument.node->clone();

			expansion->metaCall->arguments()->replaceChild(currentArg, newArg);
		}
	}
}

void MacroImportHelper::removeStuff()
{
	for (auto tbr : this->toBeRemoved_)
		removeNode(tbr);
}

void MacroImportHelper::removeNode(Model::Node* node)
{
	if (!node->parent()) return;

	if (auto ooList = DCast<Model::List>(node->parent()))
		ooList->remove(ooList->indexOf(node));
	else
		qDebug() << "cannot remove" << node->parent()->typeName();
}

bool MacroImportHelper::shouldCreateMetaCall(ExpansionEntry* expansion)
{
	auto hash = hashExpansion(expansion);

	if (!metaCallDuplicationPrevention_.contains(hash))
	{
		metaCallDuplicationPrevention_.insert(hash);
		return true;
	}

	return false;
}

QString MacroImportHelper::hashExpansion(ExpansionEntry* expansion)
{
	auto presumedLoc = sourceManager_->getPresumedLoc(expansion->range.getBegin());

	QString hash = QDir(presumedLoc.getFilename()).absolutePath()
			+ QString("|")
			+ getDefinitionName(expansion->definition)
			+ QString("|")
			+ QString::number(presumedLoc.getLine())
			+ QString("|")
			+ QString::number(presumedLoc.getColumn());

	return hash;
}

void MacroImportHelper::handleMacroExpansion(QVector<Model::Node*> nodes, MacroImportHelper::ExpansionEntry* expansion,
															NodeMapping* mapping)
{
	for (auto childExpansion : expansion->children)
		handleMacroExpansion(getNodes(childExpansion, mapping), childExpansion, mapping);

	if (!isIncompleteDefinition(expansion->definition))
	{
		createMetaDef(nodes, expansion, mapping);

		if (!expansion->parent)
		{
			QVector<Model::Node*> topLevelNodes;
			for (auto node : nodes)
				if (!node->parent())
					topLevelNodes.append(node);

			if (shouldCreateMetaCall(expansion))
			{
				OOModel::Declaration* actualContext;

				if (nodes.size() > 0)
					actualContext = getActualContext(mapping->original(topLevelNodes.first()));
				else
					actualContext = getActualContext(expansion);

				if (!DCast<OOModel::Method>(actualContext))
				{
					actualContext->metaCalls()->append(expansion->metaCall);
				}
			}

			for (auto node : topLevelNodes)
				toBeRemoved_.insert(mapping->original(node));
		}

		/*
		if (nodes.size() > 0)
		{
			if (auto ooClass = DCast<OOModel::Class>(nodes.first()))
				getActualContext(ooClass)->metaCalls()->append(expansion->metaCall);
			else if (auto ooMethod = DCast<OOModel::Method>(nodes.first()))
			{
				// TODO: skips important stuff
				//getActualContext(ooMethod)->metaCalls()->append(expansion->metaCall);
			}
			else
			{
				if (nodes.first()->parent())
				{
					if (DCast<OOModel::Statement>(nodes.first()))
					{
						if (!expansion->metaCall->parent())
						nodes.first()->parent()->replaceChild(nodes.first(),
																			new OOModel::ExpressionStatement(expansion->metaCall));
					}
					else if (DCast<OOModel::VariableDeclaration>(nodes.first()))
					{
						Q_ASSERT(DCast<OOModel::VariableDeclarationExpression>(nodes.first()->parent()));
						if (!expansion->metaCall->parent())
							nodes.first()->parent()->parent()->replaceChild(nodes.first()->parent(), expansion->metaCall);
					}
					else
					{
						if (!expansion->metaCall->parent())
							nodes.first()->parent()->replaceChild(nodes.first(), expansion->metaCall);
					}
				}
			}

			nodeReplaced(nodes.first(), expansion->metaCall);
		}

		for (auto node : nodes)
			if (auto ooList = DCast<Model::List>(node->parent()))
				ooList->remove(ooList->indexOf(node));*/
	}
}

void MacroImportHelper::DebugStmt(clang::Stmt* S)
{
	return;

	if (!S) return;

	auto expansion = getExpansion(S->getLocStart());

	qDebug() << (void*)S
				<< S->getStmtClassName()
				<< S->getLocStart().getPtrEncoding()
				<< S->getLocEnd().getPtrEncoding()
				<< "|"
				<< (expansion ? getDefinitionName(expansion->definition) : "-")
				<< "|";
}

}
