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

#include "MacroExpansion.h"

#include "MiscHelper.h"

namespace CppImport {

MacroExpansion::MacroExpansion(RawMacroInfo* rawMacroInfo, RawMacroInfo::RawExpansion* rawExpansion,
										 LexicalHelper* lexicalHelper)
	: rawMacroInfo_(rawMacroInfo), rawExpansion_(rawExpansion)
{
	metaCall_ = new OOModel::MetaCallExpression(rawMacroInfo_->hashDefinition(definition()));

	if (!definition()->getMacroInfo()->isObjectLike())
	{
		QRegularExpression regex ("\\((.*)\\)", QRegularExpression::DotMatchesEverythingOption);
		auto match = regex.match(lexicalHelper->getUnexpandedSpelling(range()));
		auto arguments = match.captured(1).split(",");

		for (auto i = 0; i < rawMacroInfo_->clang()->getArgumentNames(definition()).size(); i++)
		{
			auto actualArg = rawExpansion_->args()->getUnexpArgument((unsigned int)i);
			metaCall()->arguments()->append(new OOModel::ReferenceExpression(arguments[i]));
			argumentLocs().append(actualArg->getLocation());
		}
	}
}

bool MacroExpansion::isChildOf(MacroExpansion* entry)
{
	auto current = this;
	while (current && current != entry)
		current = current->parent;
	return current;
}

QString MacroExpansion::hash()
{
	auto presumedLoc = rawMacroInfo_->clang()->sourceManager()->getPresumedLoc(range().getBegin());

	QString hash = QDir(presumedLoc.getFilename()).absolutePath()
			+ QString("|")
			+ rawMacroInfo_->hashDefinition(definition())
			+ QString("|")
			+ QString::number(presumedLoc.getLine())
			+ QString("|")
			+ QString::number(presumedLoc.getColumn());

	return hash;
}

QVector<Model::Node*> MacroExpansion::getTopLevelNodes()
{
	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : rawMacroInfo_->astMapping()->astMapping_.keys())
	{
		for (auto range : rawMacroInfo_->astMapping()->astMapping_[node])
			if (rawMacroInfo_->clang()->sourceManager()->getExpansionLoc(range.getBegin()) == range.getBegin())
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

OOModel::Declaration* MacroExpansion::getActualContext()
{
	Q_ASSERT(!parent);

	QVector<OOModel::Declaration*> candidates;
	for (auto i = rawMacroInfo_->astMapping()->astMapping_.begin();
		  i != rawMacroInfo_->astMapping()->astMapping_.end(); i++)
		for (auto r : i.value())
			if (rawMacroInfo_->clang()->contains(r, range()))
				if (MiscHelper::validContext(i.key()))
				{
					candidates.append(DCast<OOModel::Declaration>(i.key()));
					break;
				}

	if (candidates.empty())
		return nullptr;

	auto result = candidates.first();

	for (auto candidate : candidates)
		if (result->isAncestorOf(candidate))
			result = candidate;

	return result;
}

}
