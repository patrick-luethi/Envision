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
#include "StaticStuff.h"

namespace CppImport {

ExpansionManager::ExpansionManager(ClangHelper* c, AstMapping* a,
											  DefinitionManager* d, LexicalHelper* lex)
	: c_(c), a_(a), d_(d), lex_(lex) {}

void ExpansionManager::addMacroExpansion(clang::SourceRange sr,
													  const clang::MacroDirective* md, const clang::MacroArgs* args)
{
	if (myDefinitionManager()->isPartialEnd(md))
	{
		currentXMacroParent = nullptr;
		return;
	}

	auto entry = new MacroExpansion();
	entry->range = sr;
	entry->definition = md;
	entry->parent = getExpansion(sr.getBegin());
	if (entry->parent) entry->parent->children.append(entry);

	if (myDefinitionManager()->isPartialBegin(md) && !currentXMacroParent)
		currentXMacroParent = entry;
	else if (currentXMacroParent && !entry->parent)
	{
		entry->xMacroParent = currentXMacroParent;
		currentXMacroParent->xMacroChildren.append(entry);
	}

	entry->metaCall =
			new OOModel::MetaCallExpression(myDefinitionManager()->hashDefinition(entry->definition));

	if (!md->getMacroInfo()->isObjectLike())
	{
		QRegularExpression regex ("\\((.*)\\)", QRegularExpression::DotMatchesEverythingOption);
		auto argumentsString = myLexicalHelper()->getUnexpandedSpelling(sr);
		auto match = regex.match(argumentsString);
		auto arguments = match.captured(1).split(",");

		for (auto i = 0; i < myClang()->getArgumentNames(entry->definition).size(); i++)
		{
			auto actualArg = args->getUnexpArgument((unsigned int)i);
			entry->metaCall->arguments()->append(new OOModel::ReferenceExpression(arguments[i]));
			entry->argumentLocs.append(actualArg->getLocation());
		}
	}

	expansions_.append(entry);
}


void ExpansionManager::clear()
{
	expansionCache_.clear();
	expansions_.clear();
}


QString ExpansionManager::hashExpansion(MacroExpansion* expansion)
{
	auto presumedLoc = myClang()->sourceManager()->getPresumedLoc(expansion->range.getBegin());

	QString hash = QDir(presumedLoc.getFilename()).absolutePath()
			+ QString("|")
			+ myDefinitionManager()->hashDefinition(expansion->definition)
			+ QString("|")
			+ QString::number(presumedLoc.getLine())
			+ QString("|")
			+ QString::number(presumedLoc.getColumn());

	return hash;
}


QVector<MacroExpansion*> ExpansionManager::getTopLevelExpansions()
{
	QVector<MacroExpansion*> result;
	for (auto expansion : expansions_)
		if (!expansion->parent)
			result.append(expansion);

	return result;
}


MacroExpansion*ExpansionManager::getExpansion(clang::SourceLocation loc)
{
	MacroExpansion* expansion = getImmediateExpansion(loc);
	MacroExpansion* last = expansion;

	if (expansion)
	{
		do
		{
			last = expansion;
			loc = myClang()->sourceManager()->getImmediateExpansionRange(loc).first;
			expansion = getImmediateExpansion(loc);
		} while (expansion && expansion->isChildOf(last));
	}

	return last;
}


MacroExpansion*ExpansionManager::getExpansion(OOModel::MetaCallExpression* metaCall)
{
	for (auto expansion : expansions_)
		if (expansion->metaCall == metaCall)
			return expansion;

	return nullptr;
}


MacroExpansion*ExpansionManager::getImmediateExpansion(clang::SourceLocation loc)
{
	auto expansion = myClang()->getImmediateMacroLoc(loc);
	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->range.getBegin() == expansion) return expansions_[i];

	expansion = myClang()->getImmediateMacroLoc(expansion);
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

		if (auto n = myAstMapping()->closestParentWithAstMapping(node))
			if (myAstMapping()->contains(n))
				for (auto range : myAstMapping()->get(n))
				{
					auto expansion = getExpansion(range.getBegin());
					if (expansion)	expansionCache_[node].insert(expansion);
				}
	}

	return expansionCache_[node];
}


QVector<Model::Node*> ExpansionManager::getTLExpansionTLNodes(MacroExpansion* expansion)
{
	Q_ASSERT(expansion);
	Q_ASSERT(!expansion->parent); // ensure TLExpansion

	QVector<Model::Node*> allTLExpansionNodes;
	for (auto node : myAstMapping()->nodes())
		for (auto range : myAstMapping()->get(node))
			if (myClang()->sourceManager()->getExpansionLoc(range.getBegin()) == expansion->range.getBegin())
			{
				allTLExpansionNodes.append(node);
				break;
			}

	QVector<Model::Node*> result = StaticStuff::topLevelNodes(allTLExpansionNodes);
	StaticStuff::orderNodes(result);
	return result;
}


QVector<Model::Node*> ExpansionManager::getNTLExpansionTLNodes(MacroExpansion* expansion)
{
	Q_ASSERT(expansion);

	QVector<Model::Node*> allNTLExpansionNodes;
	for (auto node : myAstMapping()->nodes())
		if (getExpansion(node).contains(expansion))
			allNTLExpansionNodes.append(node);

	QVector<Model::Node*> result = StaticStuff::topLevelNodes(allNTLExpansionNodes);
	StaticStuff::orderNodes(result);
	return result;
}

}
