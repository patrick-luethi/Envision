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
	clang()->setSourceManager(sourceManager);
}

void MacroImportHelper::setPreprocessor(const clang::Preprocessor* preprocessor)
{
	clang()->setPreprocessor(preprocessor);
}

void MacroImportHelper::getChildrenNotBelongingToExpansion(Model::Node* node,
																				MacroExpansion* expansion,
																				NodeMapping* mapping,
																				QVector<Model::Node*>* result,
																				QHash<MacroExpansion*, Model::Node*>* splices)
{
	Q_ASSERT(expansion);

	if (DCast<OOModel::MetaCallExpression>(node)) return;

	if (getExpansion(mapping->original(node)).contains(expansion))
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

void MacroImportHelper::getChildrenBelongingToExpansion(MacroExpansion* expansion,
																		QVector<Model::Node*>* result)
{
	Q_ASSERT(expansion);


	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : astMapping()->astMapping_.keys())
		if (getExpansion(node).contains(expansion))
		{
			if (getExpansion(node).size() == 1)
			{
				allNodesForExpansion.append(node);
				topLevel.insert(node);
			}
		}

	for (auto node : allNodesForExpansion)
		for (auto other : allNodesForExpansion)
			if (node != other)
				if (node->isAncestorOf(other))
					topLevel.remove(other);

	for (auto node : topLevel)
		result->append(node);

	StaticStuff::orderNodes(*result);
}


MacroExpansion* MacroImportHelper::partialBeginMacroChild(MacroExpansion* expansion)
{
	for (auto child : expansion->children)
		if (getDefinitionName(child->definition).startsWith("BEGIN_"))
			return child;

	return nullptr;
}

void MacroImportHelper::createMetaDef(QVector<Model::Node*> nodes, MacroExpansion* expansion,
												  NodeMapping* mapping, QVector<MacroArgumentInfo>& arguments,
												  QHash<MacroExpansion*, Model::Node*>* splices)
{
	auto metaDefName = hashDefinition(expansion->definition);
	if (metaDefinitions_.contains(metaDefName)) return;

	auto metaDef = new OOModel::MetaDefinition(metaDefName);
	metaDefinitions_[metaDefName] = metaDef;

	auto metaDefParent = root_;

	for (auto argName : clang()->getArgumentNames(expansion->definition))
		metaDef->arguments()->append(new OOModel::FormalMetaArgument(argName));

	if (auto beginChild = partialBeginMacroChild(expansion))
	{
		auto list = new Model::List();

		QVector<Model::Node*> statements;
		getChildrenBelongingToExpansion(expansion, &statements);

		for (auto stmt : statements)
		{
			list->append(stmt->clone());
		}

		Q_ASSERT(statements.empty() || expansion->children.size() == 1);
		for (auto child : expansion->children)
			if (child != beginChild)
			{
				list->append(child->metaCall);
			}

		if (!statements.empty() || expansion->children.size() > 1)
		{
			auto childDef = metaDefinitions_.value(hashDefinition(beginChild->definition));

			if (childDef->arguments()->size() == beginChild->metaCall->arguments()->size())
			{
				if (childDef->name().endsWith("_H"))
				{
					childDef->context()->metaCalls()->append(new OOModel::ReferenceExpression("specSplfice"));
				}
				else
				{
					auto classContext = DCast<OOModel::Class>(childDef->context());
					Q_ASSERT(classContext);

					if (classContext->methods()->size() > 0)
					{
						classContext->methods()->last()->items()->append(new OOModel::ExpressionStatement(
									new OOModel::ReferenceExpression("specSplice")));
					}
					else
					{
						Q_ASSERT(childDef->context()->metaCalls()->size() == 1);

						auto childDefInnerMetaCall =
								DCast<OOModel::MetaCallExpression>(childDef->context()->metaCalls()->first());
						Q_ASSERT(childDefInnerMetaCall);

						auto innerList = DCast<Model::List>(childDefInnerMetaCall->arguments()->last());
						Q_ASSERT(innerList);

						innerList->append(new OOModel::ReferenceExpression("specSplice"));
					}
				}

				childDef->arguments()->append(new OOModel::FormalMetaArgument("specSplice"));
			}
		}

		beginChild->metaCall->arguments()->append(list);

		metaDef->context()->metaCalls()->append(beginChild->metaCall);
	}
	else
	{
		if (nodes.size() > 0)
		{
			auto actualContext = StaticStuff::getActualContext(mapping->original(nodes.first()));
			metaDef->setContext(StaticStuff::createContext(actualContext));

			for (auto n : nodes)
			{
				NodeMapping childMapping;
				auto cloned = StaticStuff::cloneWithMapping(mapping->original(n), &childMapping);

				//applyLexicalTransformations(cloned, &childMapping);

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
	}

	metaDefParent->subDeclarations()->append(metaDef);
}

void MacroImportHelper::addChildMetaCalls(OOModel::MetaDefinition* metaDef,
														MacroExpansion* expansion,
														NodeMapping* childMapping,
														QHash<MacroExpansion*, Model::Node*>* splices)
{
	for (auto childExpansion : expansion->children)
	{
		if (childExpansion->xMacroParent) continue;

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

			auto argName = clang()->getArgumentNames(spliceLoc.expansion->definition)
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

void MacroImportHelper::clear()
{
	definitions_.clear();
	astMapping()->astMapping_.clear();
	expansionCache_.clear();
	expansions_.clear();
}

void MacroImportHelper::macroGeneration()
{
	removeIncompleteExpansions();

	QHash<MacroExpansion*, Model::Node*> splices;
	for (auto expansion : getTopLevelExpansions())
	{
		NodeMapping mapping;
		QVector<Model::Node*> generatedNodes;
		QVector<MacroArgumentInfo> allArguments;
		for (auto node : getTopLevelNodes(expansion))
		{
			auto generatedNode = StaticStuff::cloneWithMapping(node, &mapping);

			getAllArguments(generatedNode, &allArguments, &mapping);

			generatedNodes.append(generatedNode);
		}

		StaticStuff::orderNodes(generatedNodes);

		handleMacroExpansion(generatedNodes, expansion, &mapping, allArguments, &splices);

		QVector<Model::Node*> topLevelNodes;
		for (auto node : generatedNodes)
			if (!node->parent())
				topLevelNodes.append(node);

		if (shouldCreateMetaCall(expansion))
		{
			OOModel::Declaration* actualContext;

			if (topLevelNodes.size() > 0)
				actualContext = StaticStuff::getActualContext(mapping.original(topLevelNodes.first()));
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
						finalizationInfo.metaCalls.insert(splice, expansion);
				}
			}
		}

		for (auto node : topLevelNodes)
		{
			if (astMapping()->astMapping_.contains(mapping.original(node)))
			{
				bool found = false;
				for (auto range : astMapping()->astMapping_[mapping.original(node)])
					if (!clang()->isMacroRange(range))
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
				auto newArgValue = clang()->getArgumentNames(nextLoc.expansion->definition)
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

	for (auto expansion : expansions_)
		if (!expansion->xMacroChildren.empty())
		{
			for (auto node : getTopLevelNodes(expansion))
			{
				if (auto other = getMatchingXMacroExpansion(node))
				{
					if (auto list = DCast<Model::List>(other->metaCall->parent()))
						 list->remove(list->indexOf(other->metaCall));

					auto merged = new OOModel::MetaCallExpression(
								getDefinitionName(expansion->definition));

					for (auto i = 0; i < expansion->metaCall->arguments()->size(); i++)
						merged->arguments()->append(expansion->metaCall->arguments()->at(i)->clone());

					auto list = new Model::List();
					for (auto xMacroChild : expansion->xMacroChildren)
					{
						auto unbound = new OOModel::MetaCallExpression(
									getDefinitionName(xMacroChild->definition));
						for (auto i = 0; i < xMacroChild->metaCall->arguments()->size(); i++)
							unbound->arguments()->append(xMacroChild->metaCall->arguments()->at(i)->clone());

						list->append(unbound);
					}

					merged->arguments()->append(list);

					expansion->metaCall->parent()->replaceChild(expansion->metaCall, merged);

					auto metaDef = createXMacroMetaDef(expansion, other);

					for (auto i = 0; i < expansion->xMacroChildren.size(); i++)
					{
						auto xMacroChildH = expansion->xMacroChildren[i];
						auto xMacroChildCpp = other->xMacroChildren[i];

						auto unbound = getDefinitionName(xMacroChildH->definition);

						auto binding1 = metaDef->metaBindings()->at(0);
						auto binding2 = metaDef->metaBindings()->at(1);

						bool alreadyHasUnbound = false;
						for (auto j = 0; j < binding1->mappings()->size(); j++)
							if (binding1->mappings()->at(j)->name() == unbound)
							{
								alreadyHasUnbound = true;
								break;
							}
						if (alreadyHasUnbound) continue;

						auto mapping1 = new OOModel::MetaCallMapping(unbound);
						mapping1->setValue(new OOModel::ReferenceExpression(
													 hashDefinition(xMacroChildH->definition)));
						binding1->mappings()->append(mapping1);

						auto mapping2 = new OOModel::MetaCallMapping(unbound);
						mapping2->setValue(new OOModel::ReferenceExpression(
													 hashDefinition(xMacroChildCpp->definition)));
						binding2->mappings()->append(mapping2);
					}

					break;
				}
			}
		}

	clear();
}

OOModel::MetaDefinition* MacroImportHelper::createXMacroMetaDef(MacroExpansion* xMacroExpansionH_input,
														  MacroExpansion* xMacroExpansionCpp_input)
{
	auto xMacroExpansionH = xMacroExpansionH_input;
	auto xMacroExpansionCpp = xMacroExpansionCpp_input;
	while (!xMacroExpansionH->children.empty())
	{
		bool found = false;

		for (auto child : xMacroExpansionH->children)
			if (getDefinitionName(child->definition).startsWith("BEGIN_"))
			{
				xMacroExpansionH = child;
				found = true;
			}

		if (!found) break;
	}

	while (!xMacroExpansionCpp->children.empty())
	{
		bool found = false;

		for (auto child : xMacroExpansionCpp->children)
			if (getDefinitionName(child->definition).startsWith("BEGIN_"))
			{
				xMacroExpansionCpp = child;
				found = true;
			}

		if (!found) break;
	}

	auto metaDefName = getDefinitionName(xMacroExpansionH->definition);
	if (metaDefinitions_.contains(metaDefName)) return metaDefinitions_[metaDefName];

	auto xMacroDefH = metaDefinitions_.value(hashDefinition(xMacroExpansionH->definition));
	auto xMacroDefCpp = metaDefinitions_.value(hashDefinition(xMacroExpansionCpp->definition));

	auto metaDef = xMacroDefH->clone();
	metaDefinitions_[metaDefName] = metaDef;

	metaDef->setName(metaDefName);

	if (auto moduleContextH = DCast<OOModel::Module>(metaDef->context()))
	if (auto classH = DCast<OOModel::Class>(moduleContextH->classes()->first()))
	if (auto classCpp = DCast<OOModel::Class>(xMacroDefCpp->context()))
	{
		for (auto k = 0; k < classCpp->metaCalls()->size(); k++)
			classH->metaCalls()->append(classCpp->metaCalls()->at(k)->clone());

		for (auto i = 0; i < classH->methods()->size(); i++)
		for (auto j = 0; j < classCpp->methods()->size(); j++)
		{
			auto methodH = classH->methods()->at(i);
			auto methodCpp = classCpp->methods()->at(j);

			if (methodH->name() == methodCpp->name())
			{
				for (auto k = 0; k < methodCpp->items()->size(); k++)
					methodH->items()->append(methodCpp->items()->at(k)->clone());

				methodH->memberInitializers()->clear();
				for (auto k = 0; k < methodCpp->memberInitializers()->size(); k++)
					methodH->memberInitializers()->append(methodCpp->memberInitializers()->at(k)->clone());
			}
		}

		classH->metaCalls()->append(new OOModel::ReferenceExpression("list1"));
		classH->methods()->last()->items()->append(new OOModel::ExpressionStatement(
																 new OOModel::ReferenceExpression("list2")));
	}

	auto binding1 = new OOModel::MetaBinding("list1");
	binding1->setInput(new OOModel::ReferenceExpression("metaBindingInput"));
	metaDef->metaBindings()->append(binding1);
	auto binding2 = new OOModel::MetaBinding("list2");
	binding2->setInput(new OOModel::ReferenceExpression("metaBindingInput"));
	metaDef->metaBindings()->append(binding2);

	metaDef->arguments()->append(new OOModel::FormalMetaArgument("metaBindingInput"));

	root_->subDeclarations()->append(metaDef);

	return metaDef;
}

MacroExpansion* MacroImportHelper::getMatchingXMacroExpansion(Model::Node* node)
{
	if (auto metaCall = DCast<OOModel::MetaCallExpression>(node))
	{
		for (auto expansion : expansions_)
			if (!expansion->xMacroChildren.empty())
				if (expansion->metaCall == metaCall)
					return expansion;
	}

	for (auto child : node->children())
		if (auto expansion = getMatchingXMacroExpansion(child))
			return expansion;

	return nullptr;
}

void MacroImportHelper::finalize()
{
	return;

	for (auto i = finalizationInfo.metaCalls.begin(); i != finalizationInfo.metaCalls.end(); i++)
		if (DCast<OOModel::Statement>(i.key()))
			i.key()->parent()->replaceChild(i.key(), new OOModel::ExpressionStatement(i.value()->metaCall));
		else if (DCast<OOModel::Expression>(i.key()))
			i.key()->parent()->replaceChild(i.key(), i.value()->metaCall);
		else if (DCast<OOModel::VariableDeclaration>(i.key()) &&
					DCast<OOModel::VariableDeclarationExpression>(i.key()->parent()))
			i.key()->parent()->parent()->replaceChild(i.key()->parent(), i.value()->metaCall);
		else
			qDebug() << "not inserted top level metacall" << i.key()->typeName();

	for (auto tbr : finalizationInfo.nodes)
		removeNode(tbr);
}

OOModel::MetaCallExpression* MacroImportHelper::containsMetaCall(Model::Node* node)
{
	if (auto metaCall = DCast<OOModel::MetaCallExpression>(node))
		return metaCall;

	for (auto child : node->children())
		if (auto metaCall = containsMetaCall(child))
			return metaCall;

	return nullptr;
}

void MacroImportHelper::removeNode(Model::Node* node)
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

void MacroImportHelper::handleMacroExpansion(QVector<Model::Node*> nodes,
																		MacroExpansion* expansion,
																		NodeMapping* mapping, QVector<MacroArgumentInfo>& arguments,
																		QHash<MacroExpansion*, Model::Node*>* splices)
{
	for (auto childExpansion : expansion->children)
		handleMacroExpansion(getNodes(childExpansion, mapping),
									childExpansion, mapping, arguments, splices);

	if (nodes.size() > 0)
		splices->insert(expansion, mapping->original(nodes.first()));

	createMetaDef(nodes, expansion, mapping, arguments, splices);
}


ClangHelper* MacroImportHelper::clang()
{
	return &clang_;
}

AstMapping* MacroImportHelper::astMapping()
{
	return &astMapping_;
}

void MacroImportHelper::mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode)
{
	if (auto bop = clang::dyn_cast<clang::BinaryOperator>(clangAstNode))
		astMapping()->astMapping_[envisionAstNode]
				.append(clang::SourceRange(bop->getOperatorLoc(), bop->getOperatorLoc()));
	else if (auto op = clang::dyn_cast<clang::CXXOperatorCallExpr>(clangAstNode))
		astMapping()->astMapping_[envisionAstNode]
				.append(clang::SourceRange(op->getOperatorLoc(), op->getOperatorLoc()));
	else
		astMapping()->astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
}

void MacroImportHelper::mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode)
{
	correctNamedDecl(clangAstNode, envisionAstNode);

	if (!astMapping()->astMapping_[envisionAstNode].contains(clangAstNode->getSourceRange()))
		astMapping()->astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
}

void MacroImportHelper::removeIncompleteExpansions()
{
	QVector<MacroExpansion*> tbrs;
	for (auto expansion : expansions_)
		if (getDefinitionName(expansion->definition).startsWith("END_"))
			tbrs.append(expansion);

	for (auto tbr : tbrs)
		expansions_.remove(expansions_.indexOf(tbr));
}

void MacroImportHelper::addMacroDefinition(QString name, const clang::MacroDirective* md)
{
	if (name == "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_ALL") name = "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_BASE";
	if (name == "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_STYLE") name = "BEGIN_STANDARD_EXPRESSION_VISUALIZATION_BASE";

	definitions_[md] = name;
}

void MacroImportHelper::addMacroExpansion(clang::SourceRange sr, const clang::MacroDirective* md,
													const clang::MacroArgs* args)
{
	if (getDefinitionName(md).startsWith("END_"))
	{
		currentXMacroParent = nullptr;
		return;
	}

	auto entry = new MacroExpansion();
	entry->range = sr;
	entry->definition = md;
	entry->parent = getExpansion(sr.getBegin());
	if (entry->parent) entry->parent->children.append(entry);
	entry->metaCall = new OOModel::MetaCallExpression(hashDefinition(entry->definition));

	if (getDefinitionName(md).startsWith("BEGIN_") && !currentXMacroParent)
		currentXMacroParent = entry;
	else if (currentXMacroParent && !entry->parent)
	{
		entry->xMacroParent = currentXMacroParent;
		currentXMacroParent->xMacroChildren.append(entry);
	}

	if (!md->getMacroInfo()->isObjectLike())
	{
		QRegularExpression regex ("\\((.*)\\)", QRegularExpression::DotMatchesEverythingOption);
		auto match = regex.match(getUnexpandedSpelling(sr));
		auto arguments = match.captured(1).split(",");

		for (auto i = 0; i < clang()->getArgumentNames(entry->definition).size(); i++)
		{
			auto actualArg = args->getUnexpArgument((unsigned int)i);
			entry->metaCall->arguments()->append(new OOModel::ReferenceExpression(arguments[i]));
			entry->argumentLocs.append(actualArg->getLocation());
		}
	}

	expansions_.append(entry);
}

QVector<MacroExpansion*> MacroImportHelper::getTopLevelExpansions()
{
	QVector<MacroExpansion*> result;
	for (auto expansion : expansions_)
		if (!expansion->parent)
			result.append(expansion);

	return result;
}

MacroExpansion* MacroImportHelper::getExpansion(clang::SourceLocation loc)
{
	MacroExpansion* expansion = getImmediateExpansion(loc);
	MacroExpansion* last = expansion;

	if (expansion)
	{
		do
		{
			last = expansion;
			loc = clang()->sourceManager()->getImmediateExpansionRange(loc).first;
			expansion = getImmediateExpansion(loc);
		} while (expansion && expansion->isChildOf(last));
	}

	return last;
}

MacroExpansion* MacroImportHelper::getExpansion(OOModel::MetaCallExpression* metaCall)
{
	for (auto expansion : expansions_)
		if (expansion->metaCall == metaCall)
			return expansion;

	return nullptr;
}

MacroExpansion* MacroImportHelper::getImmediateExpansion(clang::SourceLocation loc)
{
	auto expansion = clang_.getImmediateMacroLoc(loc);
	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->range.getBegin() == expansion) return expansions_[i];

	expansion = clang_.getImmediateMacroLoc(expansion);
	for (auto i = 0; i < expansions_.size(); i++)
		if (expansions_[i]->range.getBegin() == expansion) return expansions_[i];

	return nullptr;
}

QSet<MacroExpansion*> MacroImportHelper::getExpansion(Model::Node* node)
{
	if (!node) return {}; //TODO: necessary?

	//if (!expansionCache_.contains(node))
	{
		expansionCache_[node] = {};

		if (auto n = astMapping()->closestParentWithAstMapping(node))
			if (astMapping()->astMapping_.contains(n))
				for (auto range : astMapping()->astMapping_[n])
				{
					auto expansion = getExpansion(range.getBegin());
					if (expansion)	expansionCache_[node].insert(expansion);
				}
	}

	return expansionCache_[node];
}

QVector<Model::Node*> MacroImportHelper::getTopLevelNodes(MacroExpansion* expansion)
{
	Q_ASSERT(!expansion->parent);

	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : astMapping()->astMapping_.keys())
	{
		for (auto range : astMapping()->astMapping_[node])
			if (clang()->sourceManager()->getExpansionLoc(range.getBegin()) == expansion->range.getBegin())
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

QString MacroImportHelper::getDefinitionName(const clang::MacroDirective* md)
{
	if (!definitions_.contains(md)) return nullptr;

	return definitions_[md];
}

QString MacroImportHelper::hashExpansion(MacroExpansion* expansion)
{
	auto presumedLoc = clang()->sourceManager()->getPresumedLoc(expansion->range.getBegin());

	QString hash = QDir(presumedLoc.getFilename()).absolutePath()
			+ QString("|")
			+ hashDefinition(expansion->definition)
			+ QString("|")
			+ QString::number(presumedLoc.getLine())
			+ QString("|")
			+ QString::number(presumedLoc.getColumn());

	return hash;
}

QString MacroImportHelper::hashDefinition(const clang::MacroDirective* md)
{
	auto presumedLoc = clang()->sourceManager()->getPresumedLoc(md->getMacroInfo()->getDefinitionLoc());

	auto suffix = QDir(presumedLoc.getFilename()).absolutePath().right(1) == "h" ? "_H" : "_CPP";

	QString hash = getDefinitionName(md) + suffix;

	return hash;
}

void MacroImportHelper::applyLexicalTransformations(Model::Node* node, NodeMapping* mapping)
{
	if (lexicalTransform_.contains(mapping->original(node)))
	{
		auto transformed = lexicalTransform_.value(mapping->original(node));

		if (auto ref = DCast<OOModel::ReferenceExpression>(node))
			ref->setName(transformed);
		else if (auto decl = DCast<OOModel::Declaration>(node))
			decl->setName(transformed);
		else if (auto strLit = DCast<OOModel::StringLiteral>(node))
			strLit->setValue(transformed);
		else
			qDebug() << "Unhandled transformed node type" << node->typeName();
	}

	for (auto child : node->children())
		applyLexicalTransformations(child, mapping);
}

bool MacroImportHelper::shouldCreateMetaCall(MacroExpansion* expansion)
{
	auto hash = hashExpansion(expansion);

	if (!metaCallDuplicationPrevention_.contains(hash))
	{
		metaCallDuplicationPrevention_.insert(hash, expansion->metaCall);
		return true;
	}

	SAFE_DELETE(expansion->metaCall);
	expansion->metaCall = metaCallDuplicationPrevention_.value(hash);
	return false;
}

QVector<Model::Node*> MacroImportHelper::getNodes(MacroExpansion* expansion,
																  NodeMapping* mapping)
{
	Q_ASSERT(expansion);

	QVector<Model::Node*> allNodesForExpansion;
	QSet<Model::Node*> topLevel;
	for (auto node : astMapping()->astMapping_.keys())
		if (getExpansion(node).contains(expansion))
		{
			allNodesForExpansion.append(node);
			topLevel.insert(node);
		}

	for (auto node : allNodesForExpansion)
		for (auto other : allNodesForExpansion)
			if (node != other)
				if (node->isAncestorOf(other))
					topLevel.remove(other);

	QVector<Model::Node*> unorderedOriginalResult;
	for (auto node : topLevel)
		unorderedOriginalResult.append(node);

	StaticStuff::orderNodes(unorderedOriginalResult);

	QVector<Model::Node*> orderedClonedResult;
	for (auto node : unorderedOriginalResult)
		orderedClonedResult.append(mapping->clone(node));

	return orderedClonedResult;
}

OOModel::Declaration* MacroImportHelper::getActualContext(MacroExpansion* expansion)
{
	Q_ASSERT(!expansion->parent);

	QVector<OOModel::Declaration*> candidates;
	for (auto i = astMapping()->astMapping_.begin();
		  i != astMapping()->astMapping_.end(); i++)
		for (auto range : i.value())
			if (clang()->contains(range, expansion->range))
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

bool MacroImportHelper::getUnexpandedNameWithQualifiers(clang::SourceLocation loc, QString* result)
{
	auto token = getUnexpToken(loc);

	*result = token.value();
	if (result->startsWith("#")) return true; // TODO: wth?

	QRegularExpression regularExpression("^(\\w|##)*$");
	auto match = regularExpression.match(token.value());
	if (!match.hasMatch())
	{
		*result = "";
		return false;
	}

	while (true)
	{
		auto sepa = token.next();
		if (!nameSeparator(sepa.value()))
			break;

		auto identifier = sepa.next();
		if (identifier.value() == ",")
			break;

		*result += sepa.value() + identifier.value();
		token = identifier.next();
	}

	return true;
}

bool MacroImportHelper::nameSeparator(QString candidate)
{
	return candidate == "::" || candidate == "." || candidate == "<" || candidate == ">" || candidate == "->";
}

bool MacroImportHelper::isExpansionception(clang::SourceLocation loc)
{
	if (loc.isMacroID())
		if (auto immediateExpansion = getImmediateExpansion(loc))
			return clang()->sourceManager()->getImmediateExpansionRange(loc).first != immediateExpansion->range.getBegin();

	return false;
}

ClangHelper::Token MacroImportHelper::getUnexpToken(clang::SourceLocation start)
{
	auto loc = isExpansionception(start) ? clang()->sourceManager()->getImmediateExpansionRange(start).first : start;

	return ClangHelper::Token(&clang_, clang()->sourceManager()->getSpellingLoc(loc));
}

QString MacroImportHelper::getUnexpandedSpelling(clang::SourceRange range)
{
	clang::SourceLocation start, end;

	if (isExpansionception(range.getBegin()))
		start = clang()->sourceManager()->getImmediateExpansionRange(range.getBegin()).first;
	else
		start = range.getBegin();

	if (isExpansionception(range.getEnd()))
		end = clang()->sourceManager()->getImmediateExpansionRange(range.getEnd()).second;
	else
		end = range.getEnd();

	return clang()->getSpelling(start, end);
}

void MacroImportHelper::correctFormalArgType(clang::NamedDecl* namedDecl, OOModel::FormalArgument* original)
{
	if (!clang()->isMacroRange(namedDecl->getSourceRange())) return;

	auto token = getUnexpToken(namedDecl->getSourceRange().getBegin());
	auto typeTokens = token.type();

	auto identifier = token.toString(typeTokens);
	auto nextToken = typeTokens.last()->next();

	while (nextToken.isWhitespace() || nextToken.isEmpty()) nextToken = nextToken.next();

	if (nextToken.value() == "*") // TODO: this won't work with multipointers
	{
		if (auto ptrExpr = DCast<OOModel::PointerTypeExpression>(original->typeExpression()))
		if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
			lexicalTransform_.insert(refExpr, identifier);
	}
	else if (nextToken.value() == "&")
	{
		if (auto ptrExpr = DCast<OOModel::ReferenceTypeExpression>(original->typeExpression()))
		if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
			lexicalTransform_.insert(refExpr, identifier);
	}
	else
		lexicalTransform_.insert(original->typeExpression(), identifier);
}

void MacroImportHelper::correctFormalResultType(clang::FunctionDecl* method,
																OOModel::FormalResult* original)
{
	if (!clang()->isMacroRange(method->getReturnTypeSourceRange())) return;

	auto token = getUnexpToken(method->getReturnTypeSourceRange().getBegin());
	auto typeTokens = token.type();

	auto identifier = token.toString(typeTokens);
	auto nextToken = typeTokens.last()->next();

	while (nextToken.isWhitespace() || nextToken.isEmpty()) nextToken = nextToken.next();

	if (nextToken.value() == "*")
	{
		if (auto ptrExpr = DCast<OOModel::PointerTypeExpression>(original->typeExpression()))
		if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
			lexicalTransform_.insert(refExpr, identifier);
	}
	else if (nextToken.value() == "&")
	{
		if (auto ptrExpr = DCast<OOModel::ReferenceTypeExpression>(original->typeExpression()))
		if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
			lexicalTransform_.insert(refExpr, identifier);
	}
	else
		lexicalTransform_.insert(original->typeExpression(), identifier);
}

void MacroImportHelper::correctMethodCall(clang::Expr* expr, OOModel::MethodCallExpression* methodCall)
{
	if (!expr->getExprLoc().isMacroID()) return;

	auto ref = DCast<OOModel::ReferenceExpression>(methodCall->callee());
	if (!ref) return;

	auto token = getUnexpToken(expr->getSourceRange().getBegin());

	/*
	 * these conditions intend to skip all implict type initializations.
	 * the locations don't map to the actual call in those situations because they don't exist in the source (implicit)
	 * we therefore have to skip them.
	 */
	if (!getUnexpandedSpelling(expr->getSourceRange()).contains("(") && methodCall->arguments()->size() == 0) return;
	if (token.value() == "{") return;

	/*
	 * an exception to the above are implicit QString initializations.
	 * since those might contain vital information we have to handle those.
	 */
	if (ref->name() == "QString" && methodCall->arguments()->size() == 1)
	{
		if (auto sLit = DCast<OOModel::StringLiteral>(methodCall->arguments()->first()))
		{
			lexicalTransform_.insert(sLit, getUnexpandedSpelling(expr->getSourceRange()));
			return;
		}
	}

	QStack<OOModel::ReferenceExpression*> refs;
	while (true)
	{
		for (auto i = ref->typeArguments()->size() - 1; i >= 0; i--)
			if (auto r = DCast<OOModel::ReferenceExpression>(ref->typeArguments()->at(i)))
				refs.push(r);
			else
				return;

		refs.push(ref);

		if (ref->prefix())
		{
			ref = DCast<OOModel::ReferenceExpression>(ref->prefix());
			if (!ref) return;
		}
		else
			break;
	}

	auto identTokens = token.identifier();

	if (!identTokens.empty())
	{
		token = token.identifier().last()->next();
		lexicalTransform_.insert(refs.pop(), token.toString(identTokens));
	}
	else
	{
		// TODO: maybe examine this deeper (only intended to happen for global scope)
		refs.pop();
		token = token.next();
	}

	while (!refs.empty())
	{
		while (nameSeparator(token.value()))
			token = token.next();

		identTokens = token.identifier();

		if (!identTokens.empty())
		{
			lexicalTransform_.insert(refs.pop(), token.toString(identTokens));
			token = identTokens.last()->next();
		}
		else
		{
			// TODO: why?
			refs.pop();
			token = token.next();
		}
	}
}

void MacroImportHelper::correctReferenceExpression(clang::SourceLocation loc, OOModel::ReferenceExpression* original)
{
	if (loc.isMacroID())
		if (isExpansionception(loc))
		{
			auto token = getUnexpToken(loc);
			lexicalTransform_.insert(original, token.toString(token.identifier()));
		}
}

void MacroImportHelper::correctExplicitTemplateInst(clang::ClassTemplateSpecializationDecl* specializationDecl,
																	 OOModel::ReferenceExpression* original)
{
	if (!clang()->isMacroRange(specializationDecl->getSourceRange()))	return;

	auto spelling = clang()->getSpelling(specializationDecl->getLocation(),
														  specializationDecl->getSourceRange().getEnd());

	// TODO: improve parsing to handle more complicated cases.
	QRegularExpression regularExpression("^(\\w+)<(\\w+::\\w+)>$");

	auto match = regularExpression.match(spelling);
	if (match.hasMatch())
	{
		lexicalTransform_.insert(original, match.captured(1));

		Q_ASSERT(original->typeArguments()->size() == 1);

		auto typeArg = DCast<OOModel::ReferenceExpression>(original->typeArguments()->first());
		Q_ASSERT(typeArg);
		Q_ASSERT(!typeArg->prefix());
		qDebug() << match.captured(2);
		lexicalTransform_.insert(typeArg, match.captured(2));
	}
	else
	{
		qDebug() << "could not correct explicit template instantiation: " << spelling;
	}
}

void MacroImportHelper::correctStringLiteral(clang::StringLiteral* strLit, OOModel::StringLiteral* original)
{
	if (!strLit->getLocStart().isMacroID()) return;

	auto rawValue = getUnexpandedSpelling(strLit->getSourceRange());

	lexicalTransform_.insert(original, rawValue);
}

void MacroImportHelper::correctIntegerLiteral(clang::IntegerLiteral* intLit, OOModel::IntegerLiteral* original)
{
	if (!intLit->getLocation().isMacroID()) return;

	auto token = getUnexpToken(intLit->getLocStart());

	lexicalTransform_.insert(original, token.value());
}

void MacroImportHelper::correctCastType(clang::Expr* expr, OOModel::CastExpression* original)
{
	if (!clang()->isMacroRange(expr->getSourceRange())) return;

	auto spelling = clang()->getSpelling(expr->getSourceRange());

	QRegularExpression regularExpression("static_cast<\\s*((\\w|\\*|##)+)\\s*>");
	auto match = regularExpression.match(spelling);

	if (match.hasMatch())
	{
		auto typeExpression = match.captured(1);

		if (typeExpression.endsWith("*"))
		{
			if (auto ptrExpr = DCast<OOModel::PointerTypeExpression>(original->castType()))
			if (auto refExpr = DCast<OOModel::ReferenceExpression>(ptrExpr->typeExpression()))
				lexicalTransform_.insert(refExpr, typeExpression);
		}
		else
			lexicalTransform_.insert(original->castType(), typeExpression);
	}
}

void MacroImportHelper::correctNamedDecl(clang::Decl* decl, Model::Node* node)
{
	if (!clang()->isMacroRange(decl->getSourceRange())) return;

	if (auto ooDecl = DCast<OOModel::Declaration>(node))
	{
		if (auto namedDecl = clang::dyn_cast<clang::NamedDecl>(decl))
		{
			auto token = getUnexpToken(namedDecl->getLocation());

			lexicalTransform_.insert(ooDecl, token.toString(token.qualifiedIdentifier()));
		}
		else
			Q_ASSERT(false && "not implemented");
	}
	else
		Q_ASSERT(false && "not implemented");
}

QVector<MacroArgumentLocation> MacroImportHelper::getArgumentHistory(clang::SourceRange range)
{
	QVector<MacroArgumentLocation> result;

	if (clang()->sourceManager()->isMacroArgExpansion(range.getBegin()) &&
		 clang()->sourceManager()->isMacroArgExpansion(range.getEnd()))
	{
		QVector<clang::SourceLocation> spellingHistory;
		clang()->getImmediateSpellingHistory(range.getBegin(), &spellingHistory);

		for (auto argumentLoc : spellingHistory)
			for (auto expansion : expansions_)
				for (auto i = 0; i < expansion->argumentLocs.size(); i++)
					if (expansion->argumentLocs[i] == argumentLoc)
						result.append(MacroArgumentLocation(expansion, i));
	}

	return result;
}

QVector<MacroArgumentLocation> MacroImportHelper::getArgumentHistory(Model::Node* node)
{
	QVector<MacroArgumentLocation> result;
	if (astMapping()->astMapping_.contains(node))
			result = getArgumentHistory(astMapping()->astMapping_[node].first());
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
