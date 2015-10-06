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

#include "StaticStuff.h"

namespace CppImport {

void StaticStuff::orderNodes(QVector<Model::Node*>& input)
{
	qSort(input.begin(), input.end(),
			[](Model::Node* e1, Model::Node* e2)
	{
		if (auto commonAncestor = e1->lowestCommonAncestor(e2))
			if (auto list = DCast<Model::List>(commonAncestor))
			{
				int index1 = -1;
				for (auto c : list->children())
					if (c == e1 || c->isAncestorOf(e1))
					{
						index1 = list->indexOf(c);
						break;
					}

				int index2 = -1;
				for (auto c : list->children())
					if (c == e2 || c->isAncestorOf(e2))
					{
						index2 = list->indexOf(c);
						break;
					}

				return index1 < index2;
			}

		return true;
	});
}

bool StaticStuff::validContext(Model::Node* node)
{
	if (DCast<OOModel::Project>(node))
		return true;
	else if (DCast<OOModel::Module>(node))
		return true;
	else if (DCast<OOModel::Class>(node))
		return true;
	else if (DCast<OOModel::Method>(node))
		return true;
	else
		return false;
}

OOModel::Declaration*StaticStuff::getActualContext(Model::Node* node)
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

OOModel::Declaration*StaticStuff::createContext(OOModel::Declaration* actualContext)
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

Model::Node*StaticStuff::cloneWithMapping(Model::Node* node, NodeMapping* mapping)
{
	auto clone = node->clone();

	QList<Model::Node*> info;
	buildMappingInfo(node, &info);
	useMappingInfo(clone, &info, mapping);

	return clone;
}

QVector<Model::Node*> StaticStuff::topLevelNodes(QVector<Model::Node*> input)
{
	QSet<Model::Node*> topLevel;
	for (auto node : input) topLevel.insert(node);

	for (auto node : input)
		for (auto other : input)
			if (node != other)
				if (node->isAncestorOf(other))
					topLevel.remove(other);

	QVector<Model::Node*> result;
	for (auto node : topLevel) result.append(node);
	return result;
}

void StaticStuff::buildMappingInfo(Model::Node* node, QList<Model::Node*>* info)
{
	info->push_back(node);

	for (auto child : node->children())
		buildMappingInfo(child, info);
}

void StaticStuff::useMappingInfo(Model::Node* node, QList<Model::Node*>* info, NodeMapping* mapping)
{
	mapping->add(info->front(), node);
	info->pop_front();

	for (auto child : node->children())
		useMappingInfo(child, info, mapping);
}

}
