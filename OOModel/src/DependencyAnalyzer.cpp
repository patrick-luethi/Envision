/***********************************************************************************************************************
**
** Copyright (c) 2011, 2014 ETH Zurich
** All rights reserved.
**
** Redistribution and use in source and binary forms, with or without modification, are permitted provided that the
** following conditions are met:
**
**    * Redistributions of source code must retain the above copyright notice, this list of conditions and the following
**      disclaimer.
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
***********************************************************************************************************************/

#include "DependencyAnalyzer.h"

#include "src/allOOModelNodes.h"

namespace OOModel {

void DependencyAnalyzer::associateWithFile(Model::Node* node, QString file, QHash<Model::Node*, QString>& nodeToFileMap)
{
	nodeToFileMap.insert(node, file);
}

QString DependencyAnalyzer::file(Model::Node* node, QHash<Model::Node*, QString>& nodeToFileMap)
{
	if (!node) return "NOT_ASSOCIATED";

	auto it = nodeToFileMap.find(node);
	return it != nodeToFileMap.end() ? *it : file(node->parent(), nodeToFileMap);
}

void DependencyAnalyzer::associateNodesWithFiles(Model::Node* current, QString namespaceName,
																 QHash<Model::Node*, QString>& nodeToFileMap)
{
	if (Module* ooModule = DCast<Module>(current))
	{
		if (ooModule->classes()->size() > 0)
		{
			namespaceName = ooModule->name();
		}
		else
		{
			// macro file
			associateWithFile(ooModule, namespaceName + "/" + ooModule->name(), nodeToFileMap);
			return;
		}
	}

	if (Class* ooClass = DCast<Class>(current))
	{
		associateWithFile(ooClass, namespaceName + "/" + ooClass->name(), nodeToFileMap);
		return;
	}

	for (auto child : current->children())
		associateNodesWithFiles(child, namespaceName, nodeToFileMap);
}

void DependencyAnalyzer::handleStuff(Model::Node* node)
{
	auto root = node;
	while (root->parent()) root = root->parent();
	QHash<Model::Node*, QString> nodeToFileMap;
	associateNodesWithFiles(root, "", nodeToFileMap);

	auto thisFile = file(node, nodeToFileMap);

	QVector<ReferenceExpression*> refs;
	getRefs(node, &refs);
	qDebug() << "inside depanalyzer" << node->typeName() << "file:" << thisFile;

	QSet<QString> nameSet;
	for (ReferenceExpression* r : refs)
	{
		if (auto ooMethod = r->firstAncestorOfType<Method>())
			if (ooMethod->items()->isAncestorOf(r))
				continue;

		if (auto t = r->target())
		{
			nameSet.insert((softDependency(r, nodeToFileMap) ? "soft " : "") + file(t, nodeToFileMap));
		}
	}

	nameSet.remove("NOT_ASSOCIATED");
	nameSet.remove(thisFile);
	for (auto e : nameSet)
		qDebug() << e;
}

bool DependencyAnalyzer::softDependency(OOModel::ReferenceExpression* reference,
													 QHash<Model::Node*, QString>& nodeToFileMap)
{
	auto p = reference->parent();
	Q_ASSERT(p);

	if (reference->name() == "libraryRoot")
	{
		qDebug() << "soft dependency?" << reference->name();
		qDebug() << reference->target() << (reference->target() ? reference->target()->typeName() : "");
		qDebug() << (reference->target() ? file(reference->target(), nodeToFileMap) : "unknown");
		qDebug() << reference->firstAncestorOfType<MethodCallExpression>();
		qDebug() << DCast<PointerTypeExpression>(p) << DCast<ReferenceTypeExpression>(p);
		qDebug() << p->typeName() << p->parent()->typeName();
	}

	if (reference->firstAncestorOfType<MethodCallExpression>()) return false;

	if (DCast<TypeQualifierExpression>(p)) p = p->parent();
	if (!DCast<PointerTypeExpression>(p) && !DCast<ReferenceTypeExpression>(p)) return false;

	return true;
}

void DependencyAnalyzer::getRefs(Model::Node* node, QVector<ReferenceExpression*>* result)
{
	if (auto ref = DCast<OOModel::ReferenceExpression>(node))
		result->append(ref);

	for (auto child : node->children())
		getRefs(child, result);
}

OOModel::Declaration* DependencyAnalyzer::getDependency(ReferenceExpression* ref)
{
	if (auto t = ref->target())
	{
		if (auto d = DCast<Declaration>(t))
			return d;

		return t->firstAncestorOfType<Declaration>();
	}

	return nullptr;
}

}
