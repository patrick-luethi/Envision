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

#include "expressions/VariableDeclarationExpression.h"
#include "../types/Type.h"

#include "ModelBase/src/nodes/TypedListDefinition.h"
DEFINE_TYPED_LIST(OOModel::VariableDeclarationExpression)

namespace OOModel {

COMPOSITENODE_DEFINE_EMPTY_CONSTRUCTORS(VariableDeclarationExpression)
COMPOSITENODE_DEFINE_TYPE_REGISTRATION_METHODS(VariableDeclarationExpression)

REGISTER_ATTRIBUTE(VariableDeclarationExpression, decl, VariableDeclaration, false, false, true)

VariableDeclarationExpression::VariableDeclarationExpression(VariableDeclaration* variableDeclaration)
: Super(nullptr, VariableDeclarationExpression::getMetaData())
{
	setDecl(variableDeclaration);
}

VariableDeclarationExpression::VariableDeclarationExpression(const QString& name, Expression* type)
: VariableDeclarationExpression(new VariableDeclaration(name, type))
{}

VariableDeclarationExpression::VariableDeclarationExpression
		(const QString& name, Expression* type, Expression* initialValue)
: VariableDeclarationExpression(new VariableDeclaration(name, type, initialValue))
{}

VariableDeclarationExpression::VariableDeclarationExpression
		(const QString& name, Expression* type, Modifier::Modifiers mod, Expression* initialValue)
: VariableDeclarationExpression(new VariableDeclaration(name, type, mod, initialValue))
{}

bool VariableDeclarationExpression::definesSymbol() const
{
	return true;
}

const QString& VariableDeclarationExpression::symbolName() const
{
	static QString nullString;
	// TODO: Is there a better way of doing this
	auto decl = (const_cast<VariableDeclarationExpression*>(this))->decl();
	return decl ? decl->symbolName() : nullString;
}

VariableDeclarationExpression::SymbolTypes VariableDeclarationExpression::symbolType() const
{
	auto decl = (const_cast<VariableDeclarationExpression*>(this))->decl();
	return decl ? decl->symbolType() : UNSPECIFIED;
}

Type* VariableDeclarationExpression::type()
{
	auto t = decl()->typeExpression()->type();
	t->setValueType(true);
	return t;
}

}
