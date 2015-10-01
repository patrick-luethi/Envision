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

#include "MiscHelper.h"

namespace CppImport {

void MacroImportHelper::setSourceManager(const clang::SourceManager* sourceManager)
{
	rawMacroInfo_.clang()->setSourceManager(sourceManager);
}

void MacroImportHelper::setPreprocessor(const clang::Preprocessor* preprocessor)
{
	rawMacroInfo_.clang()->setPreprocessor(preprocessor);
}

void MacroImportHelper::createMetaDef(ExpansionManager* expansionManager_,
													QVector<Model::Node*> nodes, MacroExpansion* expansion,
												  NodeMapping* mapping, QVector<MacroArgumentInfo>& arguments,
												  QHash<MacroExpansion*, Model::Node*>* splices)
{
	auto metaDefName = rawMacroInfo_.hashDefinition(expansion->definition());
	if (metaDefinitions_.contains(metaDefName)) return;

	auto metaDef = new OOModel::MetaDefinition(metaDefName);
	metaDefinitions_[metaDefName] = metaDef;

	for (auto argName : rawMacroInfo_.clang()->getArgumentNames(expansion->definition()))
		metaDef->arguments()->append(new OOModel::FormalMetaArgument(argName));

	if (nodes.size() > 0)
	{
		auto actualContext = MiscHelper::getActualContext(mapping->original(nodes.first()));
		metaDef->setContext(MiscHelper::createContext(actualContext));

		for (auto n : nodes)
		{
			NodeMapping childMapping;
			auto cloned = MiscHelper::cloneWithMapping(mapping->original(n), &childMapping);

			addChildMetaCalls(metaDef, expansion, &childMapping, splices);

			if (expansionManager_->removeUnownedNodes(cloned, expansion, &childMapping))
				continue;

			insertArgumentSplices(mapping, &childMapping, arguments);

			addNodeToMetaDef(cloned, metaDef);
		}
	}

	for (auto childExpansion : expansion->children())
		if (!childExpansion->metaCall()->parent())
			metaDef->context()->metaCalls()->append(childExpansion->metaCall());


	root_->subDeclarations()->append(metaDef);
}

void MacroImportHelper::addChildMetaCalls(OOModel::MetaDefinition* metaDef,
														MacroExpansion* expansion,
														NodeMapping* childMapping,
														QHash<MacroExpansion*, Model::Node*>* splices)
{
	for (auto childExpansion : expansion->children())
	{
		if (childExpansion->xMacroParent) continue;

		if (auto splice = splices->value(childExpansion))
		{
			if (auto clonedSplice = childMapping->clone(splice))
			{
				if (DCast<OOModel::Declaration>(clonedSplice))
				{
					if (auto parentDecl = clonedSplice->firstAncestorOfType<OOModel::Declaration>())
						parentDecl->metaCalls()->append(childExpansion->metaCall());
					else
						metaDef->context()->metaCalls()->append(childExpansion->metaCall());

					rawMacroInfo_.removeNode(clonedSplice);
				}
				else
				{
					qDebug() << "not inserted metacall" << clonedSplice->typeName();
				}
			}
		}
	}
}

void MacroImportHelper::insertArgumentSplices(NodeMapping* mapping, NodeMapping* childMapping,
															 QVector<MacroArgumentInfo>& arguments)
{
	for (auto argument : arguments)
	{
		auto original = mapping->original(argument.node);

		if (auto child = childMapping->clone(original))
		{
			auto spliceLoc = argument.history.first();

			auto argName = rawMacroInfo_.clang()->getArgumentNames(spliceLoc.expansion->definition())
																	.at(spliceLoc.argumentNumber);
			auto newNode = new OOModel::ReferenceExpression(argName);

			child->parent()->replaceChild(child, newNode);
			childMapping->add(original, newNode);
		}
	}
}

void MacroImportHelper::addNodeToMetaDef(Model::Node* cloned, OOModel::MetaDefinition* metaDef)
{
	if (auto ooExpression = DCast<OOModel::Expression>(cloned))
	{
		if (auto context = DCast<OOModel::Method>(metaDef->context()))
			context->items()->append(new OOModel::ExpressionStatement(ooExpression));
		else
			Q_ASSERT(false);
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
		else if (auto ooVarDecl = DCast<OOModel::VariableDeclaration>(cloned))
		{
			if (auto context = DCast<OOModel::Method>(metaDef->context()))
				context->items()->append(new OOModel::ExpressionStatement(
							new OOModel::VariableDeclarationExpression(ooVarDecl)));
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

void MacroImportHelper::addMacroDefinition(QString name, const clang::MacroDirective* md)
{
	rawMacroInfo_.definitions_[md] = name;
}

void MacroImportHelper::addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md,
													const clang::MacroArgs* args)
{
	rawMacroInfo_.expansions_.append(new RawMacroInfo::RawExpansion(sr, md, args));
}

void MacroImportHelper::mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode)
{
	if (auto de = DCast<OOModel::ReferenceExpression>(envisionAstNode))
		if (!de->prefix() && de->typeArguments()->size() == 0)
			de->setName(lexicalHelper_.getUnexpandedSpelling(clangAstNode->getSourceRange()));

	rawMacroInfo_.astMapping()->mapAst(clangAstNode, envisionAstNode);
}

void MacroImportHelper::mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode)
{
	lexicalHelper_.correctNamedDecl(clangAstNode, envisionAstNode);

	rawMacroInfo_.astMapping()->mapAst(clangAstNode, envisionAstNode);
}

void MacroImportHelper::macroGeneration()
{
	ExpansionManager expansionManager_ (&rawMacroInfo_, &lexicalHelper_);
	expansionManager_.removeIncompleteExpansions();

	QHash<MacroExpansion*, Model::Node*> splices;
	for (auto expansion : expansionManager_.getTopLevelExpansions())
	{
		NodeMapping mapping;
		QVector<Model::Node*> generatedNodes;
		QVector<MacroArgumentInfo> allArguments;
		for (auto node : expansion->getTopLevelNodes())
		{
			auto generatedNode = MiscHelper::cloneWithMapping(node, &mapping);

			expansionManager_.getAllArguments(generatedNode, &allArguments, &mapping);

			generatedNodes.append(generatedNode);
		}

		MiscHelper::orderNodes(generatedNodes);

		handleMacroExpansion(&expansionManager_, generatedNodes, expansion, &mapping, allArguments, &splices);

		QVector<Model::Node*> topLevelNodes;
		for (auto node : generatedNodes)
			if (!node->parent())
				topLevelNodes.append(node);

		if (shouldCreateMetaCall(expansion))
		{
			OOModel::Declaration* actualContext;

			if (topLevelNodes.size() > 0)
				actualContext = MiscHelper::getActualContext(mapping.original(topLevelNodes.first()));
			else
				actualContext = expansion->getActualContext();

			if (!actualContext) actualContext = root_;

			if (!expansion->xMacroParent)
			{
				if (!DCast<OOModel::Method>(actualContext))
				{
					actualContext->metaCalls()->append(expansion->metaCall());
				}
				else
				{
					if (auto splice = splices.value(expansion))
						finalizationInfo.metaCalls.insert(splice, expansion);
				}
			}
		}

		for (auto node : topLevelNodes)
		{
			if (rawMacroInfo_.astMapping()->astMapping_.contains(mapping.original(node)))
			{
				bool found = false;
				for (auto range : rawMacroInfo_.astMapping()->astMapping_[mapping.original(node)])
					if (!rawMacroInfo_.clang()->isMacroRange(range))
					{
						qDebug() << "real occurence found of" << node->typeName();
						found = true;
						break;
					}

				if (found) continue;
			}

			finalizationInfo.nodes.insert(mapping.original(node));
		}

		for (auto argument : allArguments)
		{
			if (argument.history.empty()) continue;

			for (auto i = 0; i < argument.history.size() - 1; i++)
			{
				auto currentLoc = argument.history[i];
				auto nextLoc = argument.history[i + 1];

				auto currentArg = currentLoc.expansion->metaCall()->arguments()->at(currentLoc.argumentNumber);
				auto newArgValue = rawMacroInfo_.clang()->getArgumentNames(nextLoc.expansion->definition())
																				.at(nextLoc.argumentNumber);
				auto newArg = new OOModel::ReferenceExpression(newArgValue);

				currentLoc.expansion->metaCall()->arguments()->replaceChild(currentArg, newArg);
			}

			auto lastLoc = argument.history.last();
			auto lastArg = lastLoc.expansion->metaCall()->arguments()->at(lastLoc.argumentNumber);

			if (auto currentArg = DCast<OOModel::ReferenceExpression>(lastArg))
			{
				auto newArg = argument.node->clone();

				if (!currentArg->name().startsWith("#"))
					lastLoc.expansion->metaCall()->arguments()->replaceChild(currentArg, newArg);
			}
		}
	}

	for (auto expansion : expansionManager_.expansions())
		if (!expansion->xMacroChildren().empty())
		{
			for (auto node : expansion->getTopLevelNodes())
				if (auto other = expansionManager_.getMatchingXMacroExpansion(node))
				{
					if (auto list = DCast<Model::List>(other->metaCall()->parent()))
						 list->remove(list->indexOf(other->metaCall()));

					auto merged = new OOModel::MetaCallExpression(expansion->getDefinitionName());

					for (auto i = 0; i < expansion->metaCall()->arguments()->size(); i++)
						merged->arguments()->append(expansion->metaCall()->arguments()->at(i)->clone());

					auto list = new OOModel::ArrayInitializer();
					for (auto xMacroChild : expansion->xMacroChildren())
					{
						auto unbound = new OOModel::MetaCallExpression(xMacroChild->getDefinitionName());
						for (auto i = 0; i < xMacroChild->metaCall()->arguments()->size(); i++)
							unbound->arguments()->append(xMacroChild->metaCall()->arguments()->at(i)->clone());

						list->values()->append(unbound);
					}

					merged->arguments()->append(list);

					expansion->metaCall()->parent()->replaceChild(expansion->metaCall(), merged);

					break;
				}
		}

	rawMacroInfo_.clear();
}

void MacroImportHelper::finalize()
{
	for (auto i = finalizationInfo.metaCalls.begin(); i != finalizationInfo.metaCalls.end(); i++)
		if (DCast<OOModel::Statement>(i.key()))
			i.key()->parent()->replaceChild(i.key(), new OOModel::ExpressionStatement(i.value()->metaCall()));
		else if (DCast<OOModel::Expression>(i.key()))
			i.key()->parent()->replaceChild(i.key(), i.value()->metaCall());
		else if (DCast<OOModel::VariableDeclaration>(i.key()) &&
					DCast<OOModel::VariableDeclarationExpression>(i.key()->parent()))
			i.key()->parent()->parent()->replaceChild(i.key()->parent(), i.value()->metaCall());
		else
			qDebug() << "not inserted top level metacall" << i.key()->typeName();

	for (auto tbr : finalizationInfo.nodes)
		rawMacroInfo_.removeNode(tbr);
}

void MacroImportHelper::handleMacroExpansion(ExpansionManager* expansionManager_,
															QVector<Model::Node*> nodes,
																		MacroExpansion* expansion,
																		NodeMapping* mapping, QVector<MacroArgumentInfo>& arguments,
																		QHash<MacroExpansion*, Model::Node*>* splices)
{
	for (auto childExpansion : expansion->children())
		handleMacroExpansion(expansionManager_, expansionManager_->getNodes(childExpansion, mapping),
									childExpansion, mapping, arguments, splices);

	if (nodes.size() > 0)
		splices->insert(expansion, mapping->original(nodes.first()));

	createMetaDef(expansionManager_, nodes, expansion, mapping, arguments, splices);
}


bool MacroImportHelper::shouldCreateMetaCall(MacroExpansion* expansion)
{
	auto hash = expansion->hash();

	if (!metaCallDuplicationPrevention_.contains(hash))
	{
		metaCallDuplicationPrevention_.insert(hash);
		return true;
	}

	return false;
}

}
