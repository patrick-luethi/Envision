/***********************************************************************************************************************
 **
 ** Copyright (c) 2011, 2014 ETH Zurich
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

#include "ClangAstConsumer.h"

namespace CppImport {

ClangAstConsumer::ClangAstConsumer(ClangAstVisitor* visitor)
	: clang::ASTConsumer(), astVisitor_(visitor)
{}

void ClangAstConsumer::HandleTranslationUnit(clang::ASTContext& astContext)
{
	astVisitor_->TraverseDecl(astContext.getTranslationUnitDecl());

	macroGeneration();
}

void ClangAstConsumer::macroGeneration()
{
	auto mih = &astVisitor_->macroImportHelper_;

	for (auto generatedNode : mih->getTopLevelMacroExpansionNodes())
		handleMacroExpansion(generatedNode, mih->getExpansion(generatedNode));
}

void ClangAstConsumer::handleMacroExpansion(Model::Node* node, MacroImportHelper::ExpansionEntry* expansion)
{
	auto mih = &astVisitor_->macroImportHelper_;

	for (auto childExpansion : expansion->children)
	{
		auto cENode = mih->getNode(childExpansion);
		handleMacroExpansion(cENode, childExpansion);
	}

	auto definitionName = mih->getDefinitionName(expansion->definition);

	auto anchorUp = mih->calculateAnchor(node, expansion, true);
	auto anchorDown = mih->calculateAnchor(node, expansion, false);

	auto metaDefParent = anchorUp->firstAncestorOfType<OOModel::Project>();
	auto metaDef = new OOModel::MetaDefinition(definitionName);

	for (auto formalArgument : mih->generateFormalArguments(expansion->definition))
		metaDef->arguments()->append(formalArgument);

	if (node)
	{
		if (auto ooExpression = DCast<OOModel::Expression>(node->clone()))
		{
			auto context = new OOModel::Method("Context");
			auto cloned = new OOModel::ExpressionStatement(ooExpression);
			context->items()->append(cloned);
			metaDef->setContext(context);
		}
	}
	else
	{
		auto context = new OOModel::Method("Context");
		for (auto childExpansion : expansion->children)
		{
			context->items()->append(new OOModel::ExpressionStatement(
												 new OOModel::MetaCallExpression(mih->getDefinitionName(childExpansion->definition))));
		}
		metaDef->setContext(context);
	}

	metaDefParent->subDeclarations()->append(metaDef);

	auto metaCall = new OOModel::MetaCallExpression(definitionName);
	anchorDown->parent()->replaceChild(anchorDown, metaCall);
	mih->nodeReplaced(anchorDown, metaCall);
}

void ClangAstConsumer::oldGeneration()
{
	//auto sourceManager_ = astVisitor_->sourceManager_;
	auto trMngr_ = astVisitor_->trMngr_;
	auto macroInfo = astVisitor_->importResult_;

	macroInfo.calculateMacroChildren();

	QSet<QString> duplicatePrevention;
	for (auto it = trMngr_->mapping2_.begin(); it != trMngr_->mapping2_.end(); it++)
	{
		auto ooNode = it.key();
		auto ooNodeParent = ooNode->parent();
		auto clangNodeInfo = it.value();

		auto expansionInfo = macroInfo.getExpansionInfo(clangNodeInfo);
		if (expansionInfo == nullptr) continue;

		auto parentExpansionInfo = trMngr_->mapping2_.contains(ooNodeParent) ?
					macroInfo.getExpansionInfo(trMngr_->mapping2_[ooNodeParent]) : nullptr;

		bool createMetaCall = expansionInfo != nullptr &&
				(parentExpansionInfo == nullptr ||
				 (parentExpansionInfo != nullptr && parentExpansionInfo != expansionInfo));

		if (createMetaCall) generateMetaCall(expansionInfo,
														 duplicatePrevention,
														 ooNode,
														 ooNodeParent);
	}
}

void ClangAstConsumer::generateMetaCall(ClangMacroInfo::ExpansionEntry* expansionInfo,
													 QSet<QString>& duplicatePrevention,
													 Model::Node* ooNode,
													 Model::Node* ooNodeParent)
{
	auto trMngr_ = astVisitor_->trMngr_;
	auto macroInfo = astVisitor_->importResult_;

	auto definitionName = macroInfo.getDefinitionName(expansionInfo->definition);
	bool createMetaDef = !duplicatePrevention.contains(definitionName);

	auto metaCall = new OOModel::MetaCallExpression(definitionName);

	QVector<Model::Node*> arguments;
	getArguments(ooNode, arguments);

	for (auto argument : arguments)
	{
		int argNumber;
		auto macroDef = macroInfo.getMacroDefinitionForArgument(trMngr_->mapping2_[argument], &argNumber);
		if (macroDef != expansionInfo->definition) continue;
		while (metaCall->arguments()->size() < argNumber)
			metaCall->arguments()->append(new OOModel::EmptyExpression());

		metaCall->arguments()->insert(argNumber, argument->clone());
	}

	if (createMetaDef) generateMetaDef(expansionInfo,
												  duplicatePrevention,
												  definitionName, ooNode,
												  arguments);

	ooNodeParent->replaceChild(ooNode, metaCall);
}

void ClangAstConsumer::generateMetaDef(ClangMacroInfo::ExpansionEntry* expansionInfo,
													QSet<QString>& duplicatePrevention,
													QString definitionName, Model::Node* ooNode,
													QVector<Model::Node*>& arguments)
{
	auto trMngr_ = astVisitor_->trMngr_;
	auto macroInfo = astVisitor_->importResult_;

	duplicatePrevention.insert(definitionName);

	auto metaDefParent = ooNode->firstAncestorOfType<OOModel::Project>();

	auto metaDef = new OOModel::MetaDefinition(definitionName);
	for (auto i = expansionInfo->definition->getMacroInfo()->arg_begin();
		  i != expansionInfo->definition->getMacroInfo()->arg_end(); i++)
	{
		QString argName = QString::fromStdString((*i)->getName().str());
		metaDef->arguments()->append(new OOModel::FormalMetaArgument(argName));
	}

	Model::Node* metaDefBody = nullptr;

	constructMetaDefBody(ooNode, expansionInfo);

	for (auto argument : arguments)
	{
		int argNumber;
		auto macroDef = macroInfo.getMacroDefinitionForArgument(trMngr_->mapping2_[argument], &argNumber);
		if (macroDef != expansionInfo->definition) continue;

		auto formalArgument = metaDef->arguments()->at(argNumber);

		auto splice = new OOModel::ReferenceExpression(formalArgument->name());
		if (ooNode == argument)
		{
			metaDefBody = splice;
		}
		else
		{
			ooNode->parent()->replaceChild(argument, splice);
		}
	}

	if (!metaDefBody) metaDefBody = ooNode->clone();

	if (auto ooExpression = DCast<OOModel::Expression>(metaDefBody))
	{
		auto context = new OOModel::Method("Context");
		auto cloned = new OOModel::ExpressionStatement(ooExpression);
		context->items()->append(cloned);
		metaDef->setContext(context);
	}

	metaDefParent->subDeclarations()->append(metaDef);
}

void ClangAstConsumer::setCompilerInstance(const clang::CompilerInstance* compilerInstance)
{
	Q_ASSERT(compilerInstance);
	clang::SourceManager* mngr = &compilerInstance->getSourceManager();
	Q_ASSERT(mngr);
	astVisitor_->setSourceManager(mngr);
	astVisitor_->setPreprocessor(&compilerInstance->getPreprocessor());
}

void ClangAstConsumer::getArguments(Model::Node* node, QVector<Model::Node*>& result)
{
	if (astVisitor_->trMngr_->mapping2_.contains(node))
	{
		auto clangNodeInfo = astVisitor_->trMngr_->mapping2_[node];

		if (astVisitor_->sourceManager_->isMacroArgExpansion(clangNodeInfo.sourceRange_.getBegin()) &&
			 astVisitor_->sourceManager_->isMacroArgExpansion(clangNodeInfo.sourceRange_.getEnd()))
		{
			result.append(node);
		}
	}

	for (auto child : node->children())
		getArguments(child, result);
}

void ClangAstConsumer::constructMetaDefBody(Model::Node* node, ClangMacroInfo::ExpansionEntry* entry)
{
	QVector<Model::Node*> childNodesWithAstInfo;
	getChildNodesWithAstInfo(node, childNodesWithAstInfo);

	for (auto child : entry->children)
	{
		auto cBegin = astVisitor_->sourceManager_->getSpellingLoc(child->expansion.getBegin()).getRawEncoding();
		auto cEnd = astVisitor_->sourceManager_->getSpellingLoc(child->expansion.getEnd()).getRawEncoding();

		QVector<Model::Node*> body;
		for (auto c : childNodesWithAstInfo)
		{
			if (c == node) continue;

			auto b = astVisitor_->sourceManager_->getSpellingLoc(
						astVisitor_->trMngr_->mapping2_[c].sourceRange_.getBegin()).getRawEncoding();
			auto e = astVisitor_->sourceManager_->getSpellingLoc(
						astVisitor_->trMngr_->mapping2_[c].sourceRange_.getEnd()).getRawEncoding();

			if (cBegin <= b && e <= cEnd) body.append(c);
		}

		if (body.size() > 0)
		{
			constructMetaDefBody(body[0], child);

			QSet<QString> set;
			generateMetaCall(child, set, body[0], body[0]->parent());
		}
	}
}

void ClangAstConsumer::getChildNodesWithAstInfo(Model::Node* node, QVector<Model::Node*>& result)
{
	if (astVisitor_->trMngr_->mapping2_.contains(node)) result.append(node);
	for (auto child : node->children()) getChildNodesWithAstInfo(child, result);
}

}
