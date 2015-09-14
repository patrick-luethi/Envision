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

void ClangAstConsumer::setCompilerInstance(const clang::CompilerInstance* compilerInstance)
{
	Q_ASSERT(compilerInstance);
	clang::SourceManager* mngr = &compilerInstance->getSourceManager();
	Q_ASSERT(mngr);
	astVisitor_->setSourceManager(mngr);
	astVisitor_->setPreprocessor(&compilerInstance->getPreprocessor());
}

void ClangAstConsumer::HandleTranslationUnit(clang::ASTContext& astContext)
{
	astVisitor_->TraverseDecl(astContext.getTranslationUnitDecl());

	macroGeneration();
}

void ClangAstConsumer::macroGeneration()
{
	auto mih = &astVisitor_->macroImportHelper_;

	mih->calculateMetaDefParents();

	for (auto expansion : mih->getTopLevelExpansions())
	{
		auto generatedNodes = mih->getNodes(expansion);

		qDebug() << "toplevel" << mih->getDefinitionName(expansion->definition);

		for (auto generatedNode : generatedNodes)
		{
			mih->handleStringifycation(generatedNode);
			mih->handleIdentifierConcatentation(generatedNode);
		}

		QVector<Model::Node*> allNodes;
		mih->getAllNodes(expansion, &allNodes);
		QVector<std::pair<QVector<MacroImportHelper::MacroArgumentLocation>, Model::Node*>> allArguments;
		for (auto node : allNodes)
			mih->getAllArguments(node, &allArguments);

		for (std::pair<QVector<MacroImportHelper::MacroArgumentLocation>, Model::Node*> argument : allArguments)
		{
			auto argHistory = argument.first;
			auto expansion = argHistory.first().first;
			auto argNum = argHistory.first().second;
			auto node = argument.second;

			auto argName = mih->getArgumentNames(expansion->definition).at(argNum);

			auto newNode = new OOModel::ReferenceExpression(argName);
			node->parent()->replaceChild(node, newNode);
			mih->nodeReplaced(node, newNode);
		}

		handleMacroExpansion(generatedNodes, expansion);

		for (std::pair<QVector<MacroImportHelper::MacroArgumentLocation>, Model::Node*> argument : allArguments)
		{
			auto argHistory = argument.first;
			auto node = argument.second;

			for (auto i = 0; i < argHistory.size(); i++)
			{
				auto expansion = argHistory[i].first;
				auto argNum = argHistory[i].second;
				auto actualArg = i == argHistory.size() - 1 ?
										node->clone() :
										new OOModel::ReferenceExpression(
											mih->getArgumentNames(argHistory[i + 1].first->definition)
																		.at(argHistory[i + 1].second));

				qDebug() << mih->getDefinitionName(expansion->definition)
							<< argNum
							<< expansion->metaCall
							<< expansion->metaCall->parent()
							<< actualArg->typeName();

				expansion->metaCall->arguments()->replaceChild(expansion->metaCall->arguments()->at(argNum), actualArg);
			}
		}
	}
}

void ClangAstConsumer::handleMacroExpansion(QVector<Model::Node*> nodes, MacroImportHelper::ExpansionEntry* expansion)
{
	auto mih = &astVisitor_->macroImportHelper_;

	for (auto childExpansion : expansion->children)
		handleMacroExpansion(mih->getNodes(childExpansion), childExpansion);

	mih->createMetaDef(nodes, expansion);

	if (nodes.size() > 0)
	{
		nodes.first()->parent()->replaceChild(nodes.first(), expansion->metaCall);
		mih->nodeReplaced(nodes.first(), expansion->metaCall);
	}

	//else if (DCast<Model::List>(anchorDown->parent()))
	//	actualContext->metaCalls()->append(expansion->metaCall);
	//else
	//	Q_ASSERT(false && "debug notification: found anchorDown->parent() which is not a list");

	/*
	mih->nodeReplaced(anchorDown, expansion->metaCall);

	for (auto n : nodes)
		if (auto ooList = DCast<Model::List>(n->parent()))
			ooList->remove(ooList->indexOf(n));*/
}

}
