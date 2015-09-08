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
	entry->parent = getExpansion(sr);

	if (args)
	{
		auto numArguments = (int)args->getNumArguments() - 1;

		for (auto i = 0; i < numArguments; i++)
		{
			auto formalArg = args->getUnexpArgument((unsigned int)i);
			auto formalArgLoc = sourceManager_->getSpellingLoc(formalArg->getLocation());
			entry->arguments.append(formalArgLoc);
		}
	}

	expansions_.append(entry);
	if (entry->parent) entry->parent->children.append(entry);
}

void MacroImportHelper::mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode)
{
	astMapping_[envisionAstNode] = clangAstNode->getSourceRange();
}

void MacroImportHelper::mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode)
{
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

bool MacroImportHelper::calculateJoin(QVector<clang::SourceLocation> hl1, QVector<clang::SourceLocation> hl2,
												  clang::SourceLocation* result)
{
	int i1 = hl1.size() - 1;
	int i2 = hl2.size() - 1;

	if (hl1[i1] == hl2[i2])
	{
		while (hl1[i1] == hl2[i2])
		{
			if (i1 == 0 || i2 == 0)
				break;
			i1--;
			i2--;
		}

		*result = hl1[i1];
		return true;
	}

	return false;
}

MacroImportHelper::ExpansionEntry* MacroImportHelper::getExpansion(clang::SourceRange range)
{
	auto l1 = range.getBegin();
	auto l2 = range.getEnd();

	if (!l1.isMacroID() || !l2.isMacroID()) return nullptr;

	clang::SourceLocation expansionLocation;
	if (calculateJoin(getMacroExpansionStack(l1), getMacroExpansionStack(l2), &expansionLocation))
		for (auto i = 0; i < expansions_.size(); i++)
			if (expansions_[i]->range.getBegin() == expansionLocation) return expansions_[i];

	return nullptr;
}

MacroImportHelper::ExpansionEntry* MacroImportHelper::getExpansion(Model::Node* node)
{
	if (!node) return nullptr;

	if (!expansionCache_.contains(node))
	{
		if (auto n = closestParentWithAstMapping(node))
		{
			expansionCache_[node] = getExpansion(astMapping_[n]);
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

QVector<Model::Node*> MacroImportHelper::getTopLevelMacroExpansionNodes()
{
	QVector<Model::Node*> result;
	for (auto it = astMapping_.begin(); it != astMapping_.end(); it++)
	{
		auto node = it.key();

		auto expansion = getExpansion(node);
		if (!expansion || expansion->parent != nullptr) continue;

		auto parent = closestParentWithAstMapping(node->parent());
		if (!parent || !getExpansion(parent)) result.append(node);
	}

	return result;
}

Model::Node* MacroImportHelper::getNode(MacroImportHelper::ExpansionEntry* expansion)
{
	Q_ASSERT(expansion);

	for (auto node : astMapping_.keys())
	{
		if (getExpansion(node) != expansion) continue;

		auto current = node;
		auto last = current;
		while (getExpansion(current) == expansion)
		{
			last = current;
			current = closestParentWithAstMapping(current->parent());
		}

		return last;
	}

	return nullptr;
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

	for (auto i = definition->getMacroInfo()->arg_begin(); i != definition->getMacroInfo()->arg_end(); i++)
		result.append(new OOModel::FormalMetaArgument(QString::fromStdString((*i)->getName().str())));

	return result;
}

Model::Node* MacroImportHelper::calculateAnchor(Model::Node* node, MacroImportHelper::ExpansionEntry* expansion,
																  bool up)
{
	Model::Node* result = node;

	if (!node)
	{
		// can only reach here if we are in a purely delegating macro
		Q_ASSERT(expansion->children.size() > 0);

		auto exp = up ? expansion->parent : expansion->children.first();
		while (!getNode(exp))
			exp = up ? exp->parent : exp->children.first();

		result = getNode(exp);
	}

	Q_ASSERT(result);
	return result;
}

}
