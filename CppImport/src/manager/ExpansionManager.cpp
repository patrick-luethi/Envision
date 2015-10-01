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

#include "MiscHelper.h"

namespace CppImport {

ExpansionManager::ExpansionManager(RawMacroInfo* rawMacroInfo, LexicalHelper* lexicalHelper)
{
	rawMacroInfo_ = rawMacroInfo;
	lexicalHelper_ = lexicalHelper;

	for (auto expansion : rawMacroInfo->expansions_)
	{
		if (rawMacroInfo_->isIncompleteMacroEnd(expansion->definition()))
		{
			currentXMacroParent = nullptr;
			continue;
		}

		auto entry = new MacroExpansion(rawMacroInfo_, expansion, lexicalHelper_);
		entry->parent = getExpansion(entry->range().getBegin());
		if (entry->parent) entry->parent->children().append(entry);

		if (rawMacroInfo_->isIncompleteMacroBegin(entry->definition()) && !currentXMacroParent)
			currentXMacroParent = entry;
		else if (currentXMacroParent && !entry->parent)
		{
			entry->xMacroParent = currentXMacroParent;
			currentXMacroParent->xMacroChildren().append(entry);
		}

		expansions_.append(entry);
	}
}

void ExpansionManager::getChildrenNotBelongingToExpansion(Model::Node* node,
																				MacroExpansion* expansion,
																				NodeMapping* mapping,
																				QVector<Model::Node*>* result)
{
	Q_ASSERT(expansion);

	if (DCast<OOModel::MetaCallExpression>(node)) return;

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

bool ExpansionManager::removeUnownedNodes(Model::Node* cloned,
															MacroExpansion* expansion,
															NodeMapping* mapping)
{
	QVector<Model::Node*> tbrs;
	getChildrenNotBelongingToExpansion(cloned, expansion, mapping, &tbrs);

	if (tbrs.contains(cloned)) return true;

	QSet<Model::Node*> topLevel;
	for (auto entry : tbrs) topLevel.insert(entry);

	for (Model::Node* entry : tbrs)
		for (auto other : tbrs)
			if (entry != other)
				if (entry->isAncestorOf(other))
					topLevel.remove(other);

	for (auto tbr : topLevel)
		rawMacroInfo_->removeNode(tbr);

	return false;
}

void ExpansionManager::removeIncompleteExpansions()
{
	QVector<MacroExpansion*> tbrs;
	for (auto expansion : expansions_)
		if (rawMacroInfo_->isIncompleteMacroEnd(expansion->definition()))
			tbrs.append(expansion);

	for (auto tbr : tbrs)
		expansions_.remove(expansions_.indexOf(tbr));
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
			loc = rawMacroInfo_->clang()->sourceManager()->getImmediateExpansionRange(loc).first;
			expansion = getImmediateExpansion(loc);
		} while (expansion && expansion->isChildOf(last));
	}

	return last;
}

MacroExpansion* ExpansionManager::getExpansionForRawExpansion(RawMacroInfo::RawExpansion* rawExpansion)
{
	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->rawExpansion() == rawExpansion) return expansions_[i];

	return nullptr;
}

MacroExpansion* ExpansionManager::getImmediateExpansion(clang::SourceLocation loc)
{
	if (auto rawExpansion = rawMacroInfo_->getImmediateExpansion(loc))
		return getExpansionForRawExpansion(rawExpansion);

	return nullptr;
}

QSet<MacroExpansion*> ExpansionManager::getExpansion(Model::Node* node)
{
	if (!node) return {}; //TODO: necessary?

	//if (!expansionCache_.contains(node))
	{
		expansionCache_[node] = {};

		if (auto n = rawMacroInfo_->astMapping()->closestParentWithAstMapping(node))
			if (rawMacroInfo_->astMapping()->astMapping_.contains(n))
				for (auto range : rawMacroInfo_->astMapping()->astMapping_[n])
				{
					auto expansion = getExpansion(range.getBegin());
					if (expansion)	expansionCache_[node].insert(expansion);
				}
	}

	return expansionCache_[node];
}

QVector<Model::Node*> ExpansionManager::getNodes(MacroExpansion* expansion,
																  NodeMapping* mapping)
{
	Q_ASSERT(expansion);

	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : rawMacroInfo_->astMapping()->astMapping_.keys())
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

	MiscHelper::orderNodes(unorderedOriginalResult);

	QVector<Model::Node*> orderedClonedResult;
	for (auto node : unorderedOriginalResult)
		orderedClonedResult.append(mapping->clone(node));

	return orderedClonedResult;
}

MacroExpansion* ExpansionManager::getMatchingXMacroExpansion(Model::Node* node)
{
	if (auto metaCall = DCast<OOModel::MetaCallExpression>(node))
	{
		for (auto expansion : expansions_)
			if (!expansion->xMacroChildren().empty())
				if (expansion->metaCall() == metaCall)
					return expansion;
	}

	for (auto child : node->children())
		if (auto expansion = getMatchingXMacroExpansion(child))
			return expansion;

	return nullptr;
}

QVector<MacroArgumentLocation> ExpansionManager::getArgumentHistory(clang::SourceRange range)
{
	QVector<MacroArgumentLocation> result;

	if (rawMacroInfo_->clang()->sourceManager()->isMacroArgExpansion(range.getBegin()) &&
		 rawMacroInfo_->clang()->sourceManager()->isMacroArgExpansion(range.getEnd()))
	{
		QVector<clang::SourceLocation> spellingHistory;
		rawMacroInfo_->clang()->getImmediateSpellingHistory(range.getBegin(), &spellingHistory);

		for (auto argumentLoc : spellingHistory)
			for (auto expansion : expansions_)
				for (auto i = 0; i < expansion->argumentLocs().size(); i++)
					if (expansion->argumentLocs().at(i) == argumentLoc)
						result.append(MacroArgumentLocation(expansion, i));
	}

	return result;
}

QVector<MacroArgumentLocation> ExpansionManager::getArgumentHistory(Model::Node* node)
{
	QVector<MacroArgumentLocation> result;
	if (rawMacroInfo_->astMapping()->astMapping_.contains(node))
			result = getArgumentHistory(rawMacroInfo_->astMapping()->astMapping_[node].first());
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
