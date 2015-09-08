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

#include "ClangMacroInfo.h"

namespace CppImport {

void ClangMacroInfo::test(clang::Stmt* S)
{
	qDebug() << "analysis" << S->getStmtClassName();

	auto exp = sourceManager_->getImmediateExpansionRange(S->getLocStart());

	for (auto expansion : expansions_)
	{
		qDebug() << this->getDefinitionName(expansion.definition)
					<< sourceManager_->isBeforeInTranslationUnit(expansion.expansion.getEnd(), exp.first )
					<< sourceManager_->isBeforeInTranslationUnit(exp.second, expansion.expansion.getBegin())
					<< sourceManager_->isBeforeInSLocAddrSpace (expansion.expansion.getEnd(), exp.first)
					<< sourceManager_->isBeforeInSLocAddrSpace (exp.second, expansion.expansion.getBegin());

	}
}

void ClangMacroInfo::setSourceManager(const clang::SourceManager* sourceManager)
{
	sourceManager_ = sourceManager;
}

void ClangMacroInfo::addMacroDefinition(QString name, const clang::MacroDirective* md)
{
	definitions_[md] = name;
}

void ClangMacroInfo::addMacroExpansion(clang::SourceRange expansion, const clang::MacroDirective* md,
													const clang::MacroArgs* args, const clang::MacroDirective* parent)
{
	ExpansionEntry entry;
	entry.expansion = expansion;
	entry.definition = md;
	entry.parent = parent;

	if (args)
	{
		auto numArguments = (int)args->getNumArguments() - 1;

		for (auto i = 0; i < numArguments; i++)
		{
			auto formalArg = args->getUnexpArgument((unsigned int)i);
			auto formalArgLoc = sourceManager_->getSpellingLoc(formalArg->getLocation());
			entry.arguments.append(formalArgLoc);
		}
	}

	this->expansions_.append(entry);
}

QString ClangMacroInfo::getDefinitionName(const clang::MacroDirective* md)
{
	if (!definitions_.contains(md)) return nullptr;

	return definitions_[md];
}

const clang::MacroDirective* ClangMacroInfo::getMacroDefinitionForArgument(ClangAstNodeInfo nodeInfo,
																								  int* argumentNumber)
{
	if (auto expansion = getMacroExpansion(nodeInfo))
	{
		auto actualArgLoc = sourceManager_->getSpellingLoc(
									sourceManager_->getMacroArgExpandedLocation(nodeInfo.sourceRange_.getBegin()));

		for (auto i = 0; i < expansion->arguments.size(); i++)
		{
			if (actualArgLoc == expansion->arguments[i])
			{
				*argumentNumber = i;
				break;
			}
		}

		return expansion->definition;
	}

	return nullptr;
}

const clang::MacroDirective* ClangMacroInfo::getMDForArg(clang::IdentifierInfo* arg)
{
	for (auto i = definitions_.begin(); i != definitions_.end(); i++)
	{
		auto num = i.key()->getMacroInfo()->getArgumentNum(arg);
		qDebug() << num;
		if (num >= 0) return i.key();
	}

	return nullptr;
}

void ClangMacroInfo::calculateMacroChildren()
{
	for (auto i = 0; i < expansions_.size(); i++)
		for (auto j = 0; j < expansions_.size(); j++)
			if (expansions_[i].parent == expansions_[j].definition)
				expansions_[j].children.append(&expansions_[i]);
}

ClangMacroInfo::ExpansionEntry* ClangMacroInfo::getMacroExpansion(ClangAstNodeInfo nodeInfo)
{
	auto begin = sourceManager_->getSpellingLoc(nodeInfo.sourceRange_.getBegin());
	auto end = sourceManager_->getSpellingLoc(nodeInfo.sourceRange_.getEnd());

	for (auto i = 0; i < expansions_.size(); i++)
	{
		auto entryBegin = sourceManager_->getSpellingLoc(expansions_[i].expansion.getBegin());
		auto entryEnd = sourceManager_->getSpellingLoc(expansions_[i].expansion.getEnd());

		if (entryBegin.getRawEncoding() <= begin.getRawEncoding() && end.getRawEncoding() <= entryEnd.getRawEncoding())
		{
			return &expansions_[i];
		}
	}

	return nullptr;
}

ClangMacroInfo::ExpansionEntry* ClangMacroInfo::getExpansionInfo(ClangAstNodeInfo nodeInfo)
{
	if (nodeInfo.sourceRange_.getBegin().isMacroID() && nodeInfo.sourceRange_.getEnd().isMacroID())
	{
		for (auto i = 0; i < expansions_.size(); i++)
		{
			auto expansionLocation = sourceManager_->getExpansionLoc(nodeInfo.sourceRange_.getBegin());

			if (expansions_[i].expansion.getBegin() == expansionLocation)
			{
				return &expansions_[i];
			}
		}
	}

	return nullptr;
}

ClangMacroInfo::ExpansionEntry* ClangMacroInfo::getExpansion(clang::SourceLocation loc)
{
	for (auto i = 0; i < expansions_.size(); i++)
	{
		if (loc == expansions_[i].expansion.getBegin()) return &expansions_[i];
	}

	return nullptr;
}

}
