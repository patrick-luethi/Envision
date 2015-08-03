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

#include "CodeGenerationVisitor.h"

#include "expressions/ReferenceExpression.h"
#include "expressions/MetaCallExpression.h"

#include "ModelBase/src/nodes/NameText.h"

#include "declarations/MetaDefinition.h"
#include "expressions/BooleanLiteral.h"

namespace OOModel {

CodeGenerationVisitor::CodeGenerationVisitor(QMap<QString, Model::Node *> args) : args_{args} {}

void CodeGenerationVisitor::init()
{
	addType<ReferenceExpression>(visitReferenceExpression);
	addType<Model::NameText>(visitNameText);
	addType<MetaCallExpression>(visitMetaCallExpression);
}

void CodeGenerationVisitor::visitChildren(Model::Node* n)
{
	for (auto child : n->children())
	{
		visit(child);
	}
}

void CodeGenerationVisitor::visitReferenceExpression(CodeGenerationVisitor* v, OOModel::ReferenceExpression* n)
{
	auto input = n->name();

	if (!input.contains("##"))
	{
		if (auto argument = v->args_[input])
		{
			auto p = n->parent();
			p->beginModification("change node in code generation visitor");
			p->replaceChild(n, argument->clone());
			p->endModification();
		}
	}
	else
	{
		QStringList parts = input.split("##");

		for (auto i = 0; i < parts.size(); i++)
		{
			if (auto argument = DCast<ReferenceExpression>(v->args_[parts[i]]))
			{
				parts[i] = argument->name();
			}
		}

		n->setName(parts.join(""));
	}
}

void CodeGenerationVisitor::visitNameText(CodeGenerationVisitor* v, Model::NameText* n)
{
	auto input = n->get();

	if (!input.contains("##"))
	{
		if (!v->args_[input]) return;

		if (auto argument = DCast<ReferenceExpression>(v->args_[input]))
		{
			n->set(argument->name());
		}
	}
	else
	{
		QStringList parts = input.split("##");

		for (auto i = 0; i < parts.size(); i++)
		{
			if (auto argument = DCast<ReferenceExpression>(v->args_[parts[i]]))
			{
				parts[i] = argument->name();
			}
		}

		n->set(parts.join(""));
	}
}

void CodeGenerationVisitor::visitMetaCallExpression(CodeGenerationVisitor* v, MetaCallExpression* n)
{
	if (!n->metaDefinition())
		if (auto metaDef = DCast<ReferenceExpression>(n->callee()))
			v->handlePredefinedFunction(metaDef->name(), n);
}

void CodeGenerationVisitor::handlePredefinedFunction(QString function, MetaCallExpression* n)
{
	if (function == "SET_OVERRIDE_FLAG")
	{
		if (n->arguments()->size() != 1)
		{
			qDebug() << function << "#arguments != 1";
			return;
		}

		if (auto argument = DCast<ReferenceExpression>(n->arguments()->first()))
			if (auto flag = DCast<BooleanLiteral>(args_[argument->name()]))
				if (auto p = n->firstAncestorOfType<Declaration>())
					p->modifiers()->set(Modifier::Override, flag->value());
	}
}

}