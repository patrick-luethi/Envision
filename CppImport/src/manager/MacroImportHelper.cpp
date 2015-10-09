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

#include "StaticStuff.h"

namespace CppImport {

void MacroImportHelper::clear()
{
	astMapping_.clear();
	definitionManager_.clear();
	expansionManager_.clear();
}

MacroImportHelper::MacroImportHelper(OOModel::Project* project)
	:	root_(project),
	  definitionManager_(&clang_),
	  expansionManager_(&clang_, &astMapping_, &definitionManager_, &lexicalHelper_),
	  lexicalHelper_(&clang_, &expansionManager_),
	  xMacroManager_(&definitionManager_, &expansionManager_, &metaDefinitionManager_),
	  metaDefinitionManager_(project, &clang_, &definitionManager_, &expansionManager_, &lexicalHelper_, &xMacroManager_)
	  {}

void MacroImportHelper::macroGeneration()
{
	QHash<MacroExpansion*, Model::Node*> splices;
	for (auto expansion : expansionManager_.getTopLevelExpansions())
	{
		NodeMapping mapping;
		QVector<Model::Node*> generatedNodes;
		QVector<MacroArgumentInfo> allArguments;
		for (auto node : expansionManager_.getTLExpansionTLNodes(expansion))
		{
			auto generatedNode = StaticStuff::cloneWithMapping(node, &mapping);

			getAllArguments(generatedNode, &allArguments, &mapping);

			generatedNodes.append(generatedNode);
		}

		handleMacroExpansion(generatedNodes, expansion, &mapping, allArguments, &splices);

		if (insertMetaCall(expansion))
		{
			OOModel::Declaration* actualContext;

			if (generatedNodes.size() > 0)
				actualContext = StaticStuff::getActualContext(mapping.original(generatedNodes.first()));
			else
				actualContext = getActualContext(expansion);

			if (!expansion->xMacroParent)
			{
				if (!DCast<OOModel::Method>(actualContext))
				{
					actualContext->metaCalls()->append(expansion->metaCall);
				}
				else
				{
					if (auto splice = splices.value(expansion))
						finalizationMetaCalls.insert(splice, expansion);
				}
			}
		}

		for (auto node : generatedNodes)
		{
			Q_ASSERT(!node->parent());

			if (astMapping_.contains(mapping.original(node)))
			{
				bool found = false;
				for (auto range : astMapping_.get(mapping.original(node)))
					if (!clang_.isMacroRange(range))
					{
						found = true;
						break;
					}

				if (found) continue;
			}

			finalizationNodes.append(mapping.original(node));
		}

		for (auto argument : allArguments)
		{
			if (argument.history.empty()) continue;

			for (auto i = 0; i < argument.history.size() - 1; i++)
			{
				auto currentLoc = argument.history[i];
				auto nextLoc = argument.history[i + 1];

				auto currentArg = currentLoc.expansion->metaCall->arguments()->at(currentLoc.argumentNumber);
				auto newArgValue = clang_.getArgumentNames(nextLoc.expansion->definition)
																				.at(nextLoc.argumentNumber);
				auto newArg = new OOModel::ReferenceExpression(newArgValue);

				currentLoc.expansion->metaCall->arguments()->replaceChild(currentArg, newArg);
			}

			auto lastLoc = argument.history.last();
			auto lastArg = lastLoc.expansion->metaCall->arguments()->at(lastLoc.argumentNumber);

			if (auto currentArg = DCast<OOModel::ReferenceExpression>(lastArg))
			{
				auto newArg = argument.node->clone();

				if (!currentArg->name().startsWith("#"))
					lastLoc.expansion->metaCall->arguments()->replaceChild(currentArg, newArg);
			}
		}
	}

	xMacroManager_.handleXMacros();

	clear();
}

void MacroImportHelper::finalize()
{
	for (auto i = finalizationMetaCalls.begin(); i != finalizationMetaCalls.end(); i++)
		if (DCast<OOModel::Statement>(i.key()))
			i.key()->parent()->replaceChild(i.key(), new OOModel::ExpressionStatement(i.value()->metaCall));
		else if (DCast<OOModel::Expression>(i.key()))
			i.key()->parent()->replaceChild(i.key(), i.value()->metaCall);
		else if (DCast<OOModel::VariableDeclaration>(i.key()) &&
					DCast<OOModel::VariableDeclarationExpression>(i.key()->parent()))
			i.key()->parent()->parent()->replaceChild(i.key()->parent(), i.value()->metaCall);
		else
			qDebug() << "not inserted top level metacall" << i.key()->typeName();

	StaticStuff::removeNodes(finalizationNodes);
}

void MacroImportHelper::setSourceManager(const clang::SourceManager* sourceManager)
{
	clang_.setSourceManager(sourceManager);
}

void MacroImportHelper::setPreprocessor(const clang::Preprocessor* preprocessor)
{
	clang_.setPreprocessor(preprocessor);
}

void MacroImportHelper::handleMacroExpansion(QVector<Model::Node*> nodes,
															MacroExpansion* expansion,
															NodeMapping* mapping, QVector<MacroArgumentInfo>& arguments,
																		QHash<MacroExpansion*, Model::Node*>* splices)
{
	for (auto childExpansion : expansion->children)
	{
		auto tlNodes = expansionManager_.getNTLExpansionTLNodes(childExpansion);
		handleMacroExpansion(mapping->clone(tlNodes), childExpansion, mapping, arguments, splices);
	}

	if (nodes.size() > 0)
		splices->insert(expansion, mapping->original(nodes.first()));

	metaDefinitionManager_.createMetaDef(nodes, expansion, mapping, arguments, splices);
}

void MacroImportHelper::mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode)
{
	lexicalHelper_.correctNode(clangAstNode, envisionAstNode);

	astMapping_.mapAst(clangAstNode, envisionAstNode);
}

void MacroImportHelper::mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode)
{
	lexicalHelper_.correctNode(clangAstNode, envisionAstNode);

	astMapping_.mapAst(clangAstNode, envisionAstNode);
}

void MacroImportHelper::addMacroDefinition(QString name, const clang::MacroDirective* md)
{
	definitionManager_.addMacroDefinition(name, md);
}

void MacroImportHelper::addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md,
														const clang::MacroArgs* args)
{
	expansionManager_.addMacroExpansion(sr, md, args);
}

bool MacroImportHelper::insertMetaCall(MacroExpansion* expansion)
{
	auto presumedLoc = clang_.sourceManager()->getPresumedLoc(expansion->range.getBegin());

	QString hash = QDir(presumedLoc.getFilename()).absolutePath()
			+ QString("|")
			+ definitionManager_.hash(expansion->definition)
			+ QString("|")
			+ QString::number(presumedLoc.getLine())
			+ QString("|")
			+ QString::number(presumedLoc.getColumn());

	if (!metaCalls_.contains(hash))
	{
		metaCalls_.insert(hash, expansion->metaCall);
		return true;
	}

	SAFE_DELETE(expansion->metaCall);
	expansion->metaCall = metaCalls_.value(hash);
	return false;
}

OOModel::Declaration* MacroImportHelper::getActualContext(MacroExpansion* expansion)
{
	Q_ASSERT(!expansion->parent);

	QVector<OOModel::Declaration*> candidates;
	for (auto i = astMapping_.begin(); i != astMapping_.end(); i++)
		for (auto range : i.value())
			if (clang_.contains(range, expansion->range))
				if (StaticStuff::validContext(i.key()))
				{
					candidates.append(DCast<OOModel::Declaration>(i.key()));
					break;
				}

	if (candidates.empty())
		return root_;

	auto result = candidates.first();

	for (auto candidate : candidates)
		if (result->isAncestorOf(candidate))
			result = candidate;

	return result;
}

QVector<MacroArgumentLocation> MacroImportHelper::getArgumentHistory(clang::SourceRange range)
{
	QVector<MacroArgumentLocation> result;

	if (clang_.sourceManager()->isMacroArgExpansion(range.getBegin()) &&
		 clang_.sourceManager()->isMacroArgExpansion(range.getEnd()))
	{
		QVector<clang::SourceLocation> spellingHistory;
		clang_.getImmediateSpellingHistory(range.getBegin(), &spellingHistory);

		for (auto argumentLoc : spellingHistory)
			for (auto expansion : expansionManager_.expansions())
				for (auto i = 0; i < expansion->argumentLocs.size(); i++)
					if (expansion->argumentLocs[i] == argumentLoc)
						result.append(MacroArgumentLocation(expansion, i));
	}

	return result;
}

QVector<MacroArgumentLocation> MacroImportHelper::getArgumentHistory(Model::Node* node)
{
	QVector<MacroArgumentLocation> result;
	if (astMapping_.contains(node))
			result = getArgumentHistory(astMapping_.get(node).first());
	return result;
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

}
