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
	expansionManager_.clang()->setSourceManager(sourceManager);
}

void MacroImportHelper::setPreprocessor(const clang::Preprocessor* preprocessor)
{
	expansionManager_.clang()->setPreprocessor(preprocessor);
}

void MacroImportHelper::getChildrenNotBelongingToExpansion(Model::Node* node,
																				MacroExpansion* expansion,
																				NodeMapping* mapping,
																				QVector<Model::Node*>* result,
																				QHash<MacroExpansion*, Model::Node*>* splices)
{
	Q_ASSERT(expansion);

	if (DCast<OOModel::MetaCallExpression>(node)) return;

	if (expansionManager_.getExpansion(mapping->original(node)).contains(expansion))
	{
		for (auto child : node->children())
		{
			getChildrenNotBelongingToExpansion(child, expansion, mapping, result, splices);
		}
	}
	else
	{
		result->append(node);
	}
}

void MacroImportHelper::createMetaDef(QVector<Model::Node*> nodes, MacroExpansion* expansion,
												  NodeMapping* mapping, QVector<MacroArgumentInfo>& arguments,
												  QHash<MacroExpansion*, Model::Node*>* splices)
{
	auto metaDefName = expansionManager_.getDefinitionName(expansion->definition);
	if (metaDefinitions_.contains(metaDefName)) return;

	auto metaDef = new OOModel::MetaDefinition(metaDefName);
	metaDefinitions_[metaDefName] = metaDef;

	auto metaDefParent = expansionManager_.root_;

	for (auto argName : expansionManager_.clang()->getArgumentNames(expansion->definition))
		metaDef->arguments()->append(new OOModel::FormalMetaArgument(argName));

	if (nodes.size() > 0)
	{
		auto actualContext = getActualContext(mapping->original(nodes.first()));
		metaDef->setContext(createContext(actualContext));

		for (auto n : nodes)
		{
			NodeMapping childMapping;
			auto cloned = cloneWithMapping(mapping->original(n), &childMapping);

			addChildMetaCalls(metaDef, expansion, &childMapping, splices);

			if (removeUnownedNodes(cloned, expansion, &childMapping, splices))
				continue;

			insertArgumentSplices(mapping, &childMapping, arguments);

			addNodeToMetaDef(cloned, metaDef);
		}
	}

	for (auto childExpansion : expansion->children)
		if (!childExpansion->metaCall->parent())
			metaDef->context()->metaCalls()->append(childExpansion->metaCall);

	metaDefParent->subDeclarations()->append(metaDef);
}

void MacroImportHelper::addChildMetaCalls(OOModel::MetaDefinition* metaDef,
														MacroExpansion* expansion,
														NodeMapping* childMapping,
														QHash<MacroExpansion*, Model::Node*>* splices)
{
	for (auto childExpansion : expansion->children)
		if (auto splice = splices->value(childExpansion))
		{
			if (auto clonedSplice = childMapping->clone(splice))
			{
				if (DCast<OOModel::Declaration>(clonedSplice))
				{
					if (auto parentDecl = clonedSplice->firstAncestorOfType<OOModel::Declaration>())
						parentDecl->metaCalls()->append(childExpansion->metaCall);
					else
						metaDef->context()->metaCalls()->append(childExpansion->metaCall);

					removeNode(clonedSplice);
				}
				else
				{
					qDebug() << "not inserted metacall" << clonedSplice->typeName();
				}
			}
		}
}

bool MacroImportHelper::removeUnownedNodes(Model::Node* cloned,
															MacroExpansion* expansion,
															NodeMapping* mapping,
															QHash<MacroExpansion*, Model::Node*>* splices)
{
	QVector<Model::Node*> tbrs;
	getChildrenNotBelongingToExpansion(cloned, expansion, mapping, &tbrs, splices);

	if (tbrs.contains(cloned)) return true;

	QSet<Model::Node*> topLevel;
	for (auto entry : tbrs) topLevel.insert(entry);

	for (Model::Node* entry : tbrs)
		for (auto other : tbrs)
			if (entry != other)
				if (entry->isAncestorOf(other))
					topLevel.remove(other);

	for (auto tbr : topLevel)
		removeNode(tbr);

	return false;
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

			auto argName = expansionManager_.clang()->getArgumentNames(spliceLoc.expansion->definition)
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

OOModel::Declaration* MacroImportHelper::getActualContext(Model::Node* node)
{
	auto current = node->parent();

	while (current)
	{
		if (auto result = DCast<OOModel::Project>(current))
			return result;
		else if (auto result = DCast<OOModel::Module>(current))
			return result;
		else if (auto result = DCast<OOModel::Class>(current))
			return result;
		else if (auto result = DCast<OOModel::Method>(current))
			return result;
		else
			current = current->parent();
	}

	Q_ASSERT(false);
}

OOModel::Declaration* MacroImportHelper::createContext(OOModel::Declaration* actualContext)
{
	if (DCast<OOModel::Project>(actualContext))
		return new OOModel::Project("Context");
	else if (DCast<OOModel::Module>(actualContext))
		return new OOModel::Module("Context");
	else if (DCast<OOModel::Class>(actualContext))
		return new OOModel::Class("Context");
	else if (DCast<OOModel::Method>(actualContext))
		return new OOModel::Method("Context");

	Q_ASSERT(false);
}

Model::Node* MacroImportHelper::cloneWithMapping(Model::Node* node, NodeMapping* mapping)
{
	auto clone = node->clone();

	QList<Model::Node*> info;
	buildMappingInfo(node, &info);
	useMappingInfo(clone, &info, mapping);

	return clone;
}

void MacroImportHelper::buildMappingInfo(Model::Node* node, QList<Model::Node*>* info, NodeMapping* master)
{
	info->push_back(master->original(node));

	for (auto child : node->children())
		buildMappingInfo(child, info);
}

void MacroImportHelper::buildMappingInfo(Model::Node* node, QList<Model::Node*>* info)
{
	info->push_back(node);

	for (auto child : node->children())
		buildMappingInfo(child, info);
}

void MacroImportHelper::useMappingInfo(Model::Node* node,
													  QList<Model::Node*>* info,
													  NodeMapping* mapping)
{
	mapping->add(info->front(), node);
	info->pop_front();

	for (auto child : node->children())
		useMappingInfo(child, info, mapping);
}

void MacroImportHelper::clear()
{
	expansionManager_.definitions_.clear();
	expansionManager_.astMapping()->astMapping_.clear();
	expansionManager_.expansionCache_.clear();
	expansionManager_.expansions_.clear();
}

void MacroImportHelper::setProject(OOModel::Project* project)
{
	expansionManager_.root_ = project;
}

void MacroImportHelper::macroGeneration()
{
	QHash<MacroExpansion*, Model::Node*> splices;

	for (auto expansion : expansionManager_.getTopLevelExpansions())
	{
		NodeMapping mapping;
		QVector<Model::Node*> generatedNodes;
		QVector<MacroArgumentInfo> allArguments;
		for (auto node : expansionManager_.getTopLevelNodes(expansion))
		{
			auto generatedNode = cloneWithMapping(node, &mapping);

			expansionManager_.getAllArguments(generatedNode, &allArguments, &mapping);

			generatedNodes.append(generatedNode);
		}

		expansionManager_.orderNodes(generatedNodes);

		handleMacroExpansion(generatedNodes, expansion, &mapping, allArguments, &splices);

		QVector<Model::Node*> topLevelNodes;
		for (auto node : generatedNodes)
			if (!node->parent())
				topLevelNodes.append(node);

		if (expansionManager_.shouldCreateMetaCall(expansion))
		{
			OOModel::Declaration* actualContext;

			if (topLevelNodes.size() > 0)
				actualContext = getActualContext(mapping.original(topLevelNodes.first()));
			else
				actualContext = expansionManager_.getActualContext(expansion);

			if (!DCast<OOModel::Method>(actualContext))
			{
				actualContext->metaCalls()->append(expansion->metaCall);
			}
			else
			{
				if (auto splice = splices.value(expansion))
					finalizationInfo.metaCalls.insert(splice, expansion);
			}
		}

		for (auto node : topLevelNodes)
		{
			if (expansionManager_.astMapping()->astMapping_.contains(mapping.original(node)))
			{
				bool found = false;
				for (auto range : expansionManager_.astMapping()->astMapping_[mapping.original(node)])
					if (!expansionManager_.clang()->isMacroRange(range))
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

				auto currentArg = currentLoc.expansion->metaCall->arguments()->at(currentLoc.argumentNumber);
				auto newArgValue = expansionManager_.clang()->getArgumentNames(nextLoc.expansion->definition)
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
					if (!DCast<OOModel::ReferenceExpression>(newArg))
						lastLoc.expansion->metaCall->arguments()->replaceChild(currentArg, newArg);
			}
		}
	}

	clear();
}

void MacroImportHelper::finalize()
{
	for (auto i = finalizationInfo.metaCalls.begin(); i != finalizationInfo.metaCalls.end(); i++)
		if (DCast<OOModel::Statement>(i.key()))
			i.key()->parent()->replaceChild(i.key(), new OOModel::ExpressionStatement(i.value()->metaCall));
		else if (DCast<OOModel::Expression>(i.key()))
			i.key()->parent()->replaceChild(i.key(), i.value()->metaCall);
		else if (DCast<OOModel::VariableDeclaration>(i.key()) &&
					DCast<OOModel::VariableDeclarationExpression>(i.key()->parent()))
			i.key()->parent()->parent()->replaceChild(i.key()->parent(), i.value()->metaCall);
		else
			qDebug() << "not inserte"
							"d top level metacall" << i.key()->typeName();

	for (auto tbr : finalizationInfo.nodes)
		removeNode(tbr);
}

void MacroImportHelper::removeNode(Model::Node* node)
{
	if (!node || !node->parent()) return;

	if (auto ooList = DCast<Model::List>(node->parent()))
		ooList->remove(ooList->indexOf(node));
	else if (auto ooVarDecl = DCast<OOModel::VariableDeclaration>(node->parent()))
	{
		if (ooVarDecl->initialValue() == node)
			ooVarDecl->setInitialValue(nullptr);
	}
	else if (auto skip = DCast<OOModel::VariableDeclarationExpression>(node->parent()))
	{
		removeNode(skip);
	}
	else if (auto skip = DCast<OOModel::ExpressionStatement>(node->parent()))
	{
		removeNode(skip);
	}
	else
	{
		qDebug() << "not removed" << node->typeName() << "in" << node->parent()->typeName();
	}
}

void MacroImportHelper::handleMacroExpansion(QVector<Model::Node*> nodes,
																		MacroExpansion* expansion,
																		NodeMapping* mapping, QVector<MacroArgumentInfo>& arguments,
																		QHash<MacroExpansion*, Model::Node*>* splices)
{
	for (auto childExpansion : expansion->children)
		handleMacroExpansion(expansionManager_.getNodes(childExpansion, mapping),
									childExpansion, mapping, arguments, splices);

	if (nodes.size() > 0)
		splices->insert(expansion, mapping->original(nodes.first()));

	createMetaDef(nodes, expansion, mapping, arguments, splices);
}

}
