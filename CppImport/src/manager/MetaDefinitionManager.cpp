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

#include "MetaDefinitionManager.h"

#include "MacroImportHelper.h"
#include "StaticStuff.h"

namespace CppImport {

MetaDefinitionManager::MetaDefinitionManager(MacroImportHelper* mih) : mih_(mih) {}

void MetaDefinitionManager::createMetaDef(QVector<Model::Node*> nodes, MacroExpansion* expansion, NodeMapping* mapping,
														QVector<MacroArgumentInfo>& arguments, QHash<MacroExpansion*, Model::Node*>* splices)
{
	auto metaDefName = mih_->definitionManager_.hashDefinition(expansion->definition);
	if (metaDefinitions_.contains(metaDefName)) return;

	auto metaDef = new OOModel::MetaDefinition(metaDefName);
	metaDefinitions_[metaDefName] = metaDef;

	auto metaDefParent = mih_->root_;

	for (auto argName : mih_->clang()->getArgumentNames(expansion->definition))
		metaDef->arguments()->append(new OOModel::FormalMetaArgument(argName));

	if (auto beginChild = partialBeginMacroChild(expansion))
	{
		auto list = new Model::List();

		QVector<Model::Node*> statements = mih_->expansionManager_.getNTLExpansionTLNodes(expansion);

		for (auto stmt : statements)
			list->append(stmt->clone());

		Q_ASSERT(statements.empty() || expansion->children.size() == 1);
		for (auto child : expansion->children)
			if (child != beginChild)
				list->append(child->metaCall);

		if (!statements.empty() || expansion->children.size() > 1)
		{
			auto childDef = metaDefinitions_.value(
						mih_->definitionManager_.hashDefinition(beginChild->definition));

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

				if (removeUnownedNodes(cloned, expansion, &childMapping))
					continue;

				insertArgumentSplices(mapping, &childMapping, arguments);

				StaticStuff::addNodeToDeclaration(cloned, metaDef->context());
			}
		}

		for (auto childExpansion : expansion->children)
			if (!childExpansion->metaCall->parent())
				metaDef->context()->metaCalls()->append(childExpansion->metaCall);
	}

	metaDefParent->subDeclarations()->append(metaDef);
}

OOModel::MetaDefinition*MetaDefinitionManager::createXMacroMetaDef(MacroExpansion* xMacroExpansionH_input,
																						 MacroExpansion* xMacroExpansionCpp_input)
{
	auto xMacroExpansionH = xMacroExpansionH_input;
	auto xMacroExpansionCpp = xMacroExpansionCpp_input;
	while (!xMacroExpansionH->children.empty())
	{
		bool found = false;

		for (auto child : xMacroExpansionH->children)
			if (mih_->definitionManager_.getDefinitionName(child->definition).startsWith("BEGIN_"))
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
			if (mih_->definitionManager_.getDefinitionName(child->definition).startsWith("BEGIN_"))
			{
				xMacroExpansionCpp = child;
				found = true;
			}

		if (!found) break;
	}

	auto metaDefName = mih_->definitionManager_.getDefinitionName(xMacroExpansionH->definition);
	if (metaDefinitions_.contains(metaDefName)) return metaDefinitions_[metaDefName];

	auto xMacroDefH = metaDefinitions_.value(
				mih_->definitionManager_.hashDefinition(xMacroExpansionH->definition));
	auto xMacroDefCpp = metaDefinitions_.value(
				mih_->definitionManager_.hashDefinition(xMacroExpansionCpp->definition));

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

	mih_->root_->subDeclarations()->append(metaDef);

	return metaDef;
}

void MetaDefinitionManager::addChildMetaCalls(OOModel::MetaDefinition* metaDef, MacroExpansion* expansion,
															 NodeMapping* childMapping, QHash<MacroExpansion*, Model::Node*>* splices)
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

					StaticStuff::removeNode(clonedSplice);
				}
				else
				{
					qDebug() << "not inserted metacall" << clonedSplice->typeName();
				}
			}
		}
	}
}

void MetaDefinitionManager::getChildrenNotBelongingToExpansion(Model::Node* node, MacroExpansion* expansion,
																					NodeMapping* mapping, QVector<Model::Node*>* result)
{
	Q_ASSERT(expansion);

	if (DCast<OOModel::MetaCallExpression>(node)) return;

	if (mih_->expansionManager_.getExpansion(mapping->original(node)).contains(expansion))
	{
		for (auto child : node->children())
		{
			getChildrenNotBelongingToExpansion(child, expansion, mapping, result);
		}
	}
	else
	{
		result->append(node);
	}
}

bool MetaDefinitionManager::removeUnownedNodes(Model::Node* cloned, MacroExpansion* expansion, NodeMapping* mapping)
{
	QVector<Model::Node*> tbrs;
	getChildrenNotBelongingToExpansion(cloned, expansion, mapping, &tbrs);

	if (tbrs.contains(cloned)) return true;

	StaticStuff::removeNodes(StaticStuff::topLevelNodes(tbrs));

	return false;
}

void MetaDefinitionManager::insertArgumentSplices(NodeMapping* mapping, NodeMapping* childMapping,
																  QVector<MacroArgumentInfo>& arguments)
{
	for (auto argument : arguments)
	{
		auto original = mapping->original(argument.node);

		if (auto child = childMapping->clone(original))
		{
			auto spliceLoc = argument.history.first();

			auto argName = mih_->clang()->getArgumentNames(spliceLoc.expansion->definition)
					.at(spliceLoc.argumentNumber);
			auto newNode = new OOModel::ReferenceExpression(argName);

			child->parent()->replaceChild(child, newNode);
			childMapping->add(original, newNode);
		}
	}
}

MacroExpansion*MetaDefinitionManager::partialBeginMacroChild(MacroExpansion* expansion)
{
	for (auto child : expansion->children)
		if (mih_->definitionManager_.getDefinitionName(child->definition).startsWith("BEGIN_"))
			return child;

	return nullptr;
}

}
