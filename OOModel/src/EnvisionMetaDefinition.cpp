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

#include "EnvisionMetaDefinition.h"

#include "allOOModelNodes.h"

namespace OOModel {

MetaDefinition* EnvisionMetaDefinition::attribute()
{
    auto md = new MetaDefinition("ATTRIBUTE");

    md->arguments()->append(new VariableDeclaration("type"));
    md->arguments()->append(new VariableDeclaration("name"));
    md->arguments()->append(new VariableDeclaration("setMethodName"));

    auto context = new Class("Context");
    {
        context->fields()->append(new Field("name##Index", new PointerTypeExpression(new ReferenceExpression("type")),
                                                        Modifier::Private | Modifier::Static));

        auto nameGetter = new Method("name", Modifier::Public);
        nameGetter->results()->append(new FormalResult(QString(),
                                                       new PointerTypeExpression(new ReferenceExpression("type"))));
        auto get = new MethodCallExpression("get");
        get->arguments()->append(new ReferenceExpression("name##Index"));
        auto cast = new OOModel::CastExpression(CastExpression::CastKind::StaticCast);
        cast->setExpr(get);
        cast->setType(new PointerTypeExpression(new ReferenceExpression("type")));
        auto returnStmt = new OOModel::ReturnStatement(cast);
        nameGetter->items()->append(returnStmt);
        context->methods()->append(nameGetter);

        auto setMethodName = new Method("setMethodName", Modifier::Public);
        setMethodName->arguments()->append(new FormalArgument("node",
                    new PointerTypeExpression(new ReferenceExpression("type"))));
        auto set = new MethodCallExpression("set");
        set->arguments()->append(new ReferenceExpression("name##Index"));
        set->arguments()->append(new ReferenceExpression("node"));
        setMethodName->items()->append(new ExpressionStatement(set));
        context->methods()->append(setMethodName);
    }

    md->setContext(context);
    return md;
}

MetaDefinition* EnvisionMetaDefinition::private_attribute()
{
    auto md = new MetaDefinition("PRIVATE_ATTRIBUTE");

    md->arguments()->append(new VariableDeclaration("type"));
    md->arguments()->append(new VariableDeclaration("name"));
    md->arguments()->append(new VariableDeclaration("setMethodName"));

    auto context = new Class("Context");
    {
        context->fields()->append(new Field("name##Index", new PointerTypeExpression(new ReferenceExpression("type")),
                                                        Modifier::Private | Modifier::Static));

        auto nameGetter = new Method("name", Modifier::Private);
        nameGetter->results()->append(new FormalResult(QString(),
                                                       new PointerTypeExpression(new ReferenceExpression("type"))));
        auto get = new MethodCallExpression("get");
        get->arguments()->append(new ReferenceExpression("name##Index"));
        auto cast = new OOModel::CastExpression(CastExpression::CastKind::StaticCast);
        cast->setExpr(get);
        cast->setType(new PointerTypeExpression(new ReferenceExpression("type")));
        auto returnStmt = new OOModel::ReturnStatement(cast);
        nameGetter->items()->append(returnStmt);
        context->methods()->append(nameGetter);

        auto setMethodName = new Method("setMethodName", Modifier::Private);
        setMethodName->arguments()->append(new FormalArgument("node",
                    new PointerTypeExpression(new ReferenceExpression("type"))));
        auto set = new MethodCallExpression("set");
        set->arguments()->append(new ReferenceExpression("name##Index"));
        set->arguments()->append(new ReferenceExpression("node"));
        setMethodName->items()->append(new ExpressionStatement(set));
        context->methods()->append(setMethodName);
    }

    md->setContext(context);
    return md;
}

MetaDefinition* EnvisionMetaDefinition::declare_type_id_common()
{
    auto md = new MetaDefinition("DECLARE_TYPE_ID_COMMON");

    md->arguments()->append(new VariableDeclaration("OVERRIDE"));

    auto context = new Class("Context");
    {
        Method* typeName = new Method("typeName", Modifier::Public | Modifier::Virtual | Modifier::Override);
        {
            auto resultType = new ReferenceTypeExpression(new ReferenceExpression("QString"));
            resultType->type()->setQualifiers(Type::Qualifier::CONST);
            FormalResult* result = new FormalResult(QString(), resultType);
            typeName->results()->append(result);
            context->methods()->append(typeName);
        }
    }

    md->setContext(context);
    return md;
}

}
