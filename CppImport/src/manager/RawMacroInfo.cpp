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

#include "RawMacroInfo.h"

namespace CppImport {

QString RawMacroInfo::getDefinitionName(const clang::MacroDirective* md)
{
	if (!definitions_.contains(md)) return nullptr;

	return definitions_[md];
}

QString RawMacroInfo::hashDefinition(const clang::MacroDirective* md)
{
	auto presumedLoc = clang()->sourceManager()->getPresumedLoc(md->getMacroInfo()->getDefinitionLoc());

	auto suffix = QDir(presumedLoc.getFilename()).absolutePath().right(1) == "h" ? "_H" : "_CPP";

	QString hash = getDefinitionName(md) + suffix;

	return hash;
}

RawMacroInfo::RawExpansion* RawMacroInfo::getImmediateExpansion(clang::SourceLocation loc)
{
	auto expansion = clang()->getImmediateMacroLoc(loc);
	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->range().getBegin() == expansion) return expansions_[i];

	expansion = clang()->getImmediateMacroLoc(expansion);
	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->range().getBegin() == expansion) return expansions_[i];

	return nullptr;
}

bool RawMacroInfo::isExpansionception(clang::SourceLocation loc)
{
	if (loc.isMacroID())
		if (auto immediateExpansion = getImmediateExpansion(loc))
			return clang()->sourceManager()->getImmediateExpansionRange(loc).first !=
					immediateExpansion->range().getBegin();

	return false;
}

void RawMacroInfo::removeNode(Model::Node* node)
{
	if (!node || !node->parent()) return;

	while (auto metaCall = containsMetaCall(node))
	{
		return;

		if (auto declaration = DCast<OOModel::Declaration>(metaCall->parent()->parent()))
		{
			auto newDeclaration = node->firstAncestorOfType<OOModel::Declaration>();

			declaration->metaCalls()->remove(declaration->metaCalls()->indexOf(metaCall));
			newDeclaration->metaCalls()->append(metaCall);
		}
	}

	if (auto ooList = DCast<Model::List>(node->parent()))
		ooList->remove(ooList->indexOf(node));
	else if (auto ooVarDecl = DCast<OOModel::VariableDeclaration>(node->parent()))
	{
		if (ooVarDecl->initialValue() == node)
			ooVarDecl->setInitialValue(nullptr);
	}
	else if (auto skip = DCast<OOModel::VariableDeclarationExpression>(node->parent()))
		removeNode(skip);
	else if (auto skip = DCast<OOModel::ExpressionStatement>(node->parent()))
		removeNode(skip);
	else
		qDebug() << "not removed" << node->typeName() << "in" << node->parent()->typeName();
}

OOModel::MetaCallExpression* RawMacroInfo::containsMetaCall(Model::Node* node)
{
	if (auto metaCall = DCast<OOModel::MetaCallExpression>(node))
		return metaCall;

	for (auto child : node->children())
		if (auto metaCall = containsMetaCall(child))
			return metaCall;

	return nullptr;
}

}
