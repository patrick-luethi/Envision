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

	md->arguments()->append(new FormalMetaArgument("type"));
	md->arguments()->append(new FormalMetaArgument("name"));
	md->arguments()->append(new FormalMetaArgument("setMethodName"));

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

	md->arguments()->append(new FormalMetaArgument("type"));
	md->arguments()->append(new FormalMetaArgument("name"));
	md->arguments()->append(new FormalMetaArgument("setMethodName"));

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

	md->arguments()->append(new FormalMetaArgument("OVERRIDE"));

	auto context = new Class("Context");
	{
		Method* typeName = new Method("typeName", Modifier::Public | Modifier::Virtual);
		{
			auto resultType = new ReferenceTypeExpression(new ReferenceExpression("QString"));
			resultType->type()->setQualifiers(Type::Qualifier::CONST);
			FormalResult* result = new FormalResult(QString(), resultType);
			typeName->results()->append(result);

			auto overrideMetaCall = new MetaCallExpression("SET_OVERRIDE_FLAG");
			overrideMetaCall->arguments()->append(new ReferenceExpression("OVERRIDE"));
			typeName->metaCalls()->append(overrideMetaCall);

			context->methods()->append(typeName);
		}

		Method* typeId = new Method("typeId", Modifier::Public | Modifier::Virtual);
		{
			FormalResult* result = new FormalResult(QString(), new PrimitiveTypeExpression(PrimitiveType::INT));
			typeId->results()->append(result);

			auto overrideMetaCall = new MetaCallExpression("SET_OVERRIDE_FLAG");
			overrideMetaCall->arguments()->append(new ReferenceExpression("OVERRIDE"));
			typeId->metaCalls()->append(overrideMetaCall);

			context->methods()->append(typeId);
		}

		Method* hierarchyTypeIds = new Method("hierarchyTypeIds", Modifier::Public | Modifier::Virtual);
		{
			auto resultType = new ReferenceExpression("QString");
			resultType->typeArguments()->append(new PrimitiveTypeExpression(PrimitiveType::INT));
			FormalResult* result = new FormalResult(QString(), resultType);
			hierarchyTypeIds->results()->append(result);

			auto overrideMetaCall = new MetaCallExpression("SET_OVERRIDE_FLAG");
			overrideMetaCall->arguments()->append(new ReferenceExpression("OVERRIDE"));
			hierarchyTypeIds->metaCalls()->append(overrideMetaCall);

			context->methods()->append(hierarchyTypeIds);
		}

		Method* isSubtypeOf1 = new Method("isSubtypeOf", Modifier::Public | Modifier::Virtual);
		{
			FormalResult* result = new FormalResult(QString(), new PrimitiveTypeExpression(PrimitiveType::BOOLEAN));
			isSubtypeOf1->results()->append(result);

			isSubtypeOf1->arguments()->append(new FormalArgument("type",
																				  new PrimitiveTypeExpression(PrimitiveType::INT)));

			auto overrideMetaCall = new MetaCallExpression("SET_OVERRIDE_FLAG");
			overrideMetaCall->arguments()->append(new ReferenceExpression("OVERRIDE"));
			isSubtypeOf1->metaCalls()->append(overrideMetaCall);

			context->methods()->append(isSubtypeOf1);
		}

		Method* isSubtypeOf2 = new Method("isSubtypeOf", Modifier::Public | Modifier::Virtual);
		{
			FormalResult* result = new FormalResult(QString(), new PrimitiveTypeExpression(PrimitiveType::BOOLEAN));
			isSubtypeOf2->results()->append(result);

			auto argumentType = new ReferenceTypeExpression(new ReferenceExpression("QString"));
			argumentType->type()->setQualifiers(Type::Qualifier::CONST);
			isSubtypeOf2->arguments()->append(new FormalArgument("type", argumentType));

			auto overrideMetaCall = new MetaCallExpression("SET_OVERRIDE_FLAG");
			overrideMetaCall->arguments()->append(new ReferenceExpression("OVERRIDE"));
			isSubtypeOf2->metaCalls()->append(overrideMetaCall);

			context->methods()->append(isSubtypeOf2);
		}

		Method* typeNameStatic = new Method("typeNameStatic", Modifier::Public | Modifier::Static);
		{
			auto resultType = new ReferenceTypeExpression(new ReferenceExpression("QString"));
			resultType->type()->setQualifiers(Type::Qualifier::CONST);
			FormalResult* result = new FormalResult(QString(), resultType);
			typeNameStatic->results()->append(result);

			context->methods()->append(typeNameStatic);
		}

		Method* typeIdStatic = new Method("typeIdStatic", Modifier::Public | Modifier::Static);
		{
			FormalResult* result = new FormalResult(QString(), new PrimitiveTypeExpression(PrimitiveType::INT));
			typeIdStatic->results()->append(result);

			typeIdStatic->items()->append(new ReturnStatement(new ReferenceExpression("typeId_")));

			context->methods()->append(typeIdStatic);
		}

		Method* initType = new Method("initType", Modifier::Public | Modifier::Static);
		{
			context->methods()->append(initType);
		}

		Field* typeId_ = new Field("typeId_", new PrimitiveTypeExpression(PrimitiveType::INT),
											Modifier::Private | Modifier::Static);
		{
			context->fields()->append(typeId_);
		}
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::declare_type_id()
{
	auto md = new MetaDefinition("DECLARE_TYPE_ID");

	auto context = new Class("Context");
	{
		auto metaCall = new MetaCallExpression("DECLARE_TYPE_ID_COMMON");
		metaCall->arguments()->append(new BooleanLiteral(true));

		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::item_common_custom_stylename()
{
	auto md = new MetaDefinition("ITEM_COMMON_CUSTOM_STYLENAME");
	md->arguments()->append(new FormalMetaArgument("ItemClass"));
	md->arguments()->append(new FormalMetaArgument("StyleTypeName"));

	auto context = new Class("Context");
	{
		//		public:
		\
		//			typedef StyleTypeName StyleType;
		auto styleTypeAlias = new TypeAlias("StyleType", new ReferenceExpression("StyleTypeName"));
		context->subDeclarations()->append(styleTypeAlias);

		//			const StyleType* style() const { return static_cast<const StyleType*> (Item::style()); }
		auto style = new Method("style", Modifier::Public);
		{
			auto resultType = new OOModel::PointerTypeExpression(new ReferenceExpression("StyleType"));
			resultType->type()->setQualifiers(Type::Qualifier::CONST);
			style->results()->append(new FormalResult(QString(), resultType));

			auto cast = new OOModel::CastExpression(CastExpression::CastKind::StaticCast);
			auto castType = new OOModel::PointerTypeExpression(new ReferenceExpression("StyleType"));
			castType->type()->setQualifiers(Type::Qualifier::CONST);

			cast->setType(castType);
			cast->setExpr(new MethodCallExpression("style", new ReferenceExpression("Item")));

			style->items()->append(new OOModel::ReturnStatement(cast));

			context->methods()->append(style);
		}

		//			virtual void setStyle(const Visualization::ItemStyle* style) override;
		auto setStyle = new Method("setStyle", Modifier::Public | Modifier::Virtual | Modifier::Override);
		{
			auto argumentType = new OOModel::PointerTypeExpression(
						new ReferenceExpression("ItemStyle", new ReferenceExpression("Visualization")));
			argumentType->type()->setQualifiers(Type::Qualifier::CONST);
			setStyle->arguments()->append(new FormalArgument("style", argumentType));

			context->methods()->append(setStyle);
		}

		//			static Visualization::StyleSet<ItemClass>& itemStyles();
		auto itemStyles = new Method("itemStyles", Modifier::Public | Modifier::Static);
		{
			auto resultType = new ReferenceExpression("StyleSet", new ReferenceExpression("Visualization"));
			resultType->typeArguments()->append(new ReferenceExpression("ItemClass"));
			itemStyles->results()->append(new FormalResult(QString(), new ReferenceTypeExpression(resultType)));

			context->methods()->append(itemStyles);
		}

		//			virtual Visualization::InteractionHandler* handler() const override;
		auto handler = new Method("handler", Modifier::Public | Modifier::Virtual | Modifier::Override);
		{
			auto resultType = new ReferenceExpression("InteractionHandler", new ReferenceExpression("Visualization"));
			handler->results()->append(new FormalResult(QString(), new PointerTypeExpression(resultType)));

			context->methods()->append(handler);
		}

		//			static void setDefaultClassHandler(Visualization::InteractionHandler* handler){defaultClassHandler_ = handler;}
		auto setDefaultClassHandler = new Method("setDefaultClassHandler", Modifier::Public | Modifier::Static);
		{
			auto argumentType = new OOModel::PointerTypeExpression(
						new ReferenceExpression("InteractionHandler", new ReferenceExpression("Visualization")));
			setDefaultClassHandler->arguments()->append(new FormalArgument("handler", argumentType));

			auto assignment = new AssignmentExpression(AssignmentExpression::AssignmentTypes::ASSIGN,
																	 new ReferenceExpression("defaultClassHandler_"),
																	 new ReferenceExpression("handler"));

			setDefaultClassHandler->items()->append(new ExpressionStatement(assignment));

			context->methods()->append(setDefaultClassHandler);
		}

		//			static Visualization::InteractionHandler* defaultClassHandler() {return defaultClassHandler_;}
		auto defaultClassHandler = new Method("defaultClassHandler", Modifier::Public | Modifier::Static);
		{
			auto resultType = new ReferenceExpression("InteractionHandler", new ReferenceExpression("Visualization"));
			defaultClassHandler->results()->append(new FormalResult(QString(), new PointerTypeExpression(resultType)));

			defaultClassHandler->items()->append(new ReturnStatement(new ReferenceExpression("defaultClassHandler_")));

			context->methods()->append(defaultClassHandler);
		}

		//			virtual QList<Visualization::VisualizationAddOn*> addOns() override;
		auto addOns = new Method("addOns", Modifier::Public | Modifier::Virtual | Modifier::Override);
		{
			auto resultType = new ReferenceExpression("QList");
			resultType->typeArguments()->append(new PointerTypeExpression(
																new ReferenceExpression("VisualizationAddOn",
																								new ReferenceExpression("Visualization"))));
			addOns->results()->append(new FormalResult(QString(), resultType));

			context->methods()->append(addOns);
		}

		//			static void addAddOn(Visualization::VisualizationAddOn* addOn);
		auto addAddOn = new Method("addAddOn", Modifier::Public | Modifier::Static);
		{
			auto argumentType = new PointerTypeExpression(
						new ReferenceExpression("VisualizationAddOn", new ReferenceExpression("Visualization")));
			addAddOn->arguments()->append(new FormalArgument("addOn", argumentType));

			context->methods()->append(addAddOn);
		}

		//			static bool removeAddOn(Visualization::VisualizationAddOn* addOn);
		auto removeAddOn = new Method("removeAddOn", Modifier::Public | Modifier::Static);
		{
			removeAddOn->results()->append(new FormalResult(QString(),
																			new PrimitiveTypeExpression(PrimitiveType::BOOLEAN)));

			auto argumentType = new PointerTypeExpression(
						new ReferenceExpression("VisualizationAddOn", new ReferenceExpression("Visualization")));
			removeAddOn->arguments()->append(new FormalArgument("addOn", argumentType));

			context->methods()->append(removeAddOn);
		}

		//		private:

		//			static Visualization::InteractionHandler* defaultClassHandler_;
		auto defaultClassHandler_ = new Field("defaultClassHandler_",
														  new PointerTypeExpression(
															  new ReferenceExpression("InteractionHandler",
																							  new ReferenceExpression("Visualization"))),
														  Modifier::Private | Modifier::Static);
		context->fields()->append(defaultClassHandler_);

		//			static QList<Visualization::VisualizationAddOn*>& staticAddOns();
		auto staticAddOns = new Method("staticAddOns", Modifier::Private | Modifier::Static);
		{
			auto resultType = new ReferenceExpression("QList");
			resultType->typeArguments()->append(new PointerTypeExpression(
																new ReferenceExpression("VisualizationAddOn",
																								new ReferenceExpression("Visualization"))));
			staticAddOns->results()->append(new FormalResult(QString(), new ReferenceTypeExpression(resultType)));

			context->methods()->append(staticAddOns);
		}
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::begin_standard_expression_visualization_style_h()
{
	auto md = new MetaDefinition("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_STYLE_H");

	md->arguments()->append(new FormalMetaArgument("apiSpecification"));
	md->arguments()->append(new FormalMetaArgument("className"));
	md->arguments()->append(new FormalMetaArgument("nodeType"));
	md->arguments()->append(new FormalMetaArgument("styleTypeName"));

	auto context = new Module("Context");
	{
		auto c = new Class("className");
		{
			auto metaCall = new MetaCallExpression("ITEM_COMMON_CUSTOM_STYLENAME");
			metaCall->arguments()->append(new ReferenceExpression("className"));
			metaCall->arguments()->append(new ReferenceExpression("styleTypeName"));
			c->metaCalls()->append(metaCall);

			auto constructor = new Method("className", Modifier::Public, Method::MethodKind::Constructor);
			{
				auto argument1 = new FormalArgument("parent",
																new PointerTypeExpression(
																new ReferenceExpression("Item",
																	new ReferenceExpression("Visualization",
																		new GlobalScopeExpression()))));
				constructor->arguments()->append(argument1);

				auto argument2 = new FormalArgument("node",
																new PointerTypeExpression(
																	new ReferenceExpression("NodeType")));
				constructor->arguments()->append(argument2);

				auto argument3Type = new PointerTypeExpression(
							new ReferenceExpression("StyleType"));
				argument3Type->type()->setQualifiers(Type::Qualifier::CONST);
				auto argument3 = new FormalArgument("style", argument3Type);
				argument3->setInitialValue(
							new MethodCallExpression("get",
								new MethodCallExpression("itemStyles")));
				constructor->arguments()->append(argument3);
			}
			c->methods()->append(constructor);

			auto determineChildren = new Method("determineChildren",
															Modifier::Protected | Modifier::Virtual | Modifier::Override);
			c->methods()->append(determineChildren);
		}

		context->classes()->append(c);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::begin_standard_expression_visualization_h()
{
	auto md = new MetaDefinition("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_H");

	md->arguments()->append(new FormalMetaArgument("apiSpecification"));
	md->arguments()->append(new FormalMetaArgument("className"));
	md->arguments()->append(new FormalMetaArgument("nodeType"));

	auto context = new Module("Context");
	{
		auto metaCall = new MetaCallExpression("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_STYLE_H");
		metaCall->arguments()->append(new ReferenceExpression("apiSpecification"));
		metaCall->arguments()->append(new ReferenceExpression("className"));
		metaCall->arguments()->append(new ReferenceExpression("nodeType"));
		metaCall->arguments()->append(new ReferenceExpression("OperatorStyle",
													new ReferenceExpression("OOVisualization",
														new GlobalScopeExpression())));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::begin_standard_enumeration_expression_visualization_h()
{
	auto md = new MetaDefinition("BEGIN_STANDARD_ENUMERATION_EXPRESSION_VISUALIZATION_H");

	md->arguments()->append(new FormalMetaArgument("apiSpecification"));
	md->arguments()->append(new FormalMetaArgument("className"));
	md->arguments()->append(new FormalMetaArgument("nodeType"));
	md->arguments()->append(new FormalMetaArgument("enumeration"));

	auto context = new Module("Context");
	{
		auto metaCall = new MetaCallExpression("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_STYLE_H");
		metaCall->arguments()->append(new ReferenceExpression("apiSpecification"));
		metaCall->arguments()->append(new ReferenceExpression("className"));
		metaCall->arguments()->append(new ReferenceExpression("nodeType"));
		metaCall->arguments()->append(new ReferenceExpression("OperatorSequenceStyle",
													new ReferenceExpression("OOVisualization",
														new GlobalScopeExpression())));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::begin_standard_flag_expression_visualization_h()
{
	auto md = new MetaDefinition("BEGIN_STANDARD_FLAG_EXPRESSION_VISUALIZATION_H");

	md->arguments()->append(new FormalMetaArgument("apiSpecification"));
	md->arguments()->append(new FormalMetaArgument("className"));
	md->arguments()->append(new FormalMetaArgument("nodeType"));
	md->arguments()->append(new FormalMetaArgument("flag"));

	auto context = new Module("Context");
	{
		auto metaCall = new MetaCallExpression("BEGIN_STANDARD_ENUMERATION_EXPRESSION_VISUALIZATION_H");
		metaCall->arguments()->append(new ReferenceExpression("apiSpecification"));
		metaCall->arguments()->append(new ReferenceExpression("className"));
		metaCall->arguments()->append(new ReferenceExpression("nodeType"));
		metaCall->arguments()->append(new ReferenceExpression("flag"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::expression_part_h()
{
	auto md = new MetaDefinition("EXPRESSION_PART_H");

	md->arguments()->append(new FormalMetaArgument("type"));
	md->arguments()->append(new FormalMetaArgument("name"));

	auto context = new Class("Context");
	{
		auto name = new Method("name", Modifier::Public);
		{
			name->results()->append(new FormalResult(QString(),
												new PointerTypeExpression(
													new ReferenceExpression("type"))));

			name->items()->append(new ReturnStatement(
											 new ReferenceExpression("name##_")));

			context->methods()->append(name);
		}

		auto nameField = new Field("name##_",
											new PointerTypeExpression(
												new ReferenceExpression("type")),
											Modifier::Private,
											new NullLiteral());
		context->fields()->append(nameField);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::prefix_h()
{
	auto md = new MetaDefinition("PREFIX_H");

	md->arguments()->append(new FormalMetaArgument("condition"));

	auto context = new Class("Context");
	{
		auto metaCall = new MetaCallExpression("EXPRESSION_PART_H");
		metaCall->arguments()->append(new ReferenceExpression("Static",
													new ReferenceExpression("Visualization",
														new GlobalScopeExpression())));
		metaCall->arguments()->append(new ReferenceExpression("prefix"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::infix_h()
{
	auto md = new MetaDefinition("INFIX_H");

	md->arguments()->append(new FormalMetaArgument("condition"));

	auto context = new Class("Context");
	{
		auto metaCall = new MetaCallExpression("EXPRESSION_PART_H");
		metaCall->arguments()->append(new ReferenceExpression("Static",
													new ReferenceExpression("Visualization",
														new GlobalScopeExpression())));
		metaCall->arguments()->append(new ReferenceExpression("infix"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::infix2_h()
{
	auto md = new MetaDefinition("INFIX2_H");

	md->arguments()->append(new FormalMetaArgument("condition"));

	auto context = new Class("Context");
	{
		auto metaCall = new MetaCallExpression("EXPRESSION_PART_H");
		metaCall->arguments()->append(new ReferenceExpression("Static",
													new ReferenceExpression("Visualization",
														new GlobalScopeExpression())));
		metaCall->arguments()->append(new ReferenceExpression("infix2"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::postfix_h()
{
	auto md = new MetaDefinition("POSTFIX_H");

	md->arguments()->append(new FormalMetaArgument("condition"));

	auto context = new Class("Context");
	{
		auto metaCall = new MetaCallExpression("EXPRESSION_PART_H");
		metaCall->arguments()->append(new ReferenceExpression("Static",
													new ReferenceExpression("Visualization",
														new GlobalScopeExpression())));
		metaCall->arguments()->append(new ReferenceExpression("postfix"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::operand_h()
{
	auto md = new MetaDefinition("OPERAND_H");

	md->arguments()->append(new FormalMetaArgument("name"));

	auto context = new Class("Context");
	{
		auto metaCall = new MetaCallExpression("EXPRESSION_PART_H");
		metaCall->arguments()->append(new ReferenceExpression("Item",
													new ReferenceExpression("Visualization",
														new GlobalScopeExpression())));
		metaCall->arguments()->append(new ReferenceExpression("name"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::wrapped_operand_h()
{
	auto md = new MetaDefinition("WRAPPED_OPERAND_H");

	md->arguments()->append(new FormalMetaArgument("name"));
	md->arguments()->append(new FormalMetaArgument("wrapId"));

	auto context = new Class("Context");
	{
		auto metaCall = new MetaCallExpression("OPERAND_H");
		metaCall->arguments()->append(new ReferenceExpression("name"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::begin_standard_expression_visualization_all_cpp()
{
	auto md = new MetaDefinition("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_ALL_CPP");

	md->arguments()->append(new FormalMetaArgument("apiSpecification"));
	md->arguments()->append(new FormalMetaArgument("className"));
	md->arguments()->append(new FormalMetaArgument("nodeType"));

	auto context = new Module("Context");
	{
		auto c = new Class("className");
		{
			auto metaCall = new MetaCallExpression("ITEM_COMMON_DEFINITIONS");
			metaCall->arguments()->append(new ReferenceExpression("className"));
			metaCall->arguments()->append(new StringLiteral("item"));
			c->metaCalls()->append(metaCall);

			auto constructor = new Method("className", Modifier::Public, Method::MethodKind::Constructor);
			{
				/*constructor->memberInitializers()->append(
							new MemberInitializer(new ReferenceExpression("Super"),
														 new CommaExpression(new ReferenceExpression("parent"),
																					new CommaExpression(new ReferenceExpression("node"),
																											  new ReferenceExpression("style")))));
*/
				auto argument1 = new FormalArgument("parent",
																new PointerTypeExpression(
																	new ReferenceExpression("Item",
																									new ReferenceExpression("Visualization",
																																	new GlobalScopeExpression()))));
				constructor->arguments()->append(argument1);

				auto argument2 = new FormalArgument("node",
																new PointerTypeExpression(
																	new ReferenceExpression("NodeType")));
				constructor->arguments()->append(argument2);

				auto argument3Type = new PointerTypeExpression(
							new ReferenceExpression("StyleType"));
				argument3Type->type()->setQualifiers(Type::Qualifier::CONST);
				auto argument3 = new FormalArgument("style", argument3Type);
				constructor->arguments()->append(argument3);
			}
			c->methods()->append(constructor);

			auto determineChildren = new Method("determineChildren",
															Modifier::Protected | Modifier::Virtual | Modifier::Override);

			determineChildren->items()->append(new ExpressionStatement(
															  new VariableDeclarationExpression("index",
																											new PrimitiveTypeExpression(PrimitiveType::INT),
																											new IntegerLiteral(0))));

			c->methods()->append(determineChildren);
		}

		context->classes()->append(c);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::begin_standard_expression_visualization_cpp()
{
	auto md = new MetaDefinition("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_CPP");

	md->arguments()->append(new FormalMetaArgument("apiSpecification"));
	md->arguments()->append(new FormalMetaArgument("className"));
	md->arguments()->append(new FormalMetaArgument("nodeType"));

	auto context = new Method("Context");
	{
		auto metaCall = new MetaCallExpression("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_ALL_CPP");
		metaCall->arguments()->append(new ReferenceExpression("apiSpecification"));
		metaCall->arguments()->append(new ReferenceExpression("className"));
		metaCall->arguments()->append(new ReferenceExpression("nodeType"));
		context->metaCalls()->append(metaCall);

		auto opStyleType = new PointerTypeExpression(
					new ReferenceExpression("OperatorStyle",
													new ReferenceExpression("OOVisualization",
																					new GlobalScopeExpression())));
		opStyleType->type()->setQualifiers(Type::Qualifier::CONST);
		auto opStyle = new VariableDeclarationExpression("opStyle", opStyleType, new MethodCallExpression("style"));
		context->items()->append(new ExpressionStatement(opStyle));

		auto setLayoutStyle = new MethodCallExpression("setStyle", new MethodCallExpression("layout"));
		setLayoutStyle->arguments()->append(new MethodCallExpression("layout",
																						 new ReferenceTypeExpression(
																							 new ReferenceExpression("opStyle"))));
		context->items()->append(new ExpressionStatement(setLayoutStyle));
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::begin_standard_enumeration_expression_visualization_cpp()
{
	auto md = new MetaDefinition("BEGIN_STANDARD_ENUMERATION_EXPRESSION_VISUALIZATION_CPP");

	md->arguments()->append(new FormalMetaArgument("apiSpecification"));
	md->arguments()->append(new FormalMetaArgument("className"));
	md->arguments()->append(new FormalMetaArgument("nodeType"));
	md->arguments()->append(new FormalMetaArgument("enumeration"));

	auto context = new Method("Context");
	{
		auto metaCall = new MetaCallExpression("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_ALL_CPP");
		metaCall->arguments()->append(new ReferenceExpression("apiSpecification"));
		metaCall->arguments()->append(new ReferenceExpression("className"));
		metaCall->arguments()->append(new ReferenceExpression("nodeType"));
		context->metaCalls()->append(metaCall);

		auto opStyleType = new PointerTypeExpression(
					new ReferenceExpression("OperatorStyle",
													new ReferenceExpression("OOVisualization",
																					new GlobalScopeExpression())));
		opStyleType->type()->setQualifiers(Type::Qualifier::CONST);
		auto opStyleInit = new MethodCallExpression("op",
																  new ReferenceTypeExpression(
																	  new MethodCallExpression("style")));
		auto opStyleInitArgument = new CastExpression();
		opStyleInitArgument->setType(new PrimitiveTypeExpression(PrimitiveType::INT));
		opStyleInitArgument->setExpr(new MethodCallExpression("enumeration",
																				new MethodCallExpression("node")));
		opStyleInit->arguments()->append(opStyleInitArgument);
		auto opStyle = new VariableDeclarationExpression("opStyle", opStyleType, opStyleInit);
		context->items()->append(new ExpressionStatement(opStyle));

		auto setLayoutStyle = new MethodCallExpression("setStyle", new MethodCallExpression("layout"));
		setLayoutStyle->arguments()->append(new MethodCallExpression("layout",
																						 new ReferenceTypeExpression(
																							 new ReferenceExpression("opStyle"))));
		context->items()->append(new ExpressionStatement(setLayoutStyle));
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::begin_standard_flag_expression_visualization_cpp()
{
	auto md = new MetaDefinition("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_CPP");

	md->arguments()->append(new FormalMetaArgument("apiSpecification"));
	md->arguments()->append(new FormalMetaArgument("className"));
	md->arguments()->append(new FormalMetaArgument("nodeType"));
	md->arguments()->append(new FormalMetaArgument("flag"));

	auto context = new Method("Context");
	{
		auto metaCall = new MetaCallExpression("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_ALL_CPP");
		metaCall->arguments()->append(new ReferenceExpression("apiSpecification"));
		metaCall->arguments()->append(new ReferenceExpression("className"));
		metaCall->arguments()->append(new ReferenceExpression("nodeType"));
		context->metaCalls()->append(metaCall);

		auto f = new VariableDeclarationExpression("f", new PrimitiveTypeExpression(PrimitiveType::INT),
																 new MethodCallExpression("flag",
																								  new MethodCallExpression("node")));
		context->items()->append(new ExpressionStatement(f));

		auto qAssertF = new MethodCallExpression("Q_ASSERT");
		qAssertF->arguments()->append(new ReferenceExpression("f"));
		context->items()->append(new ExpressionStatement(qAssertF));

		auto i = new VariableDeclarationExpression("f", new PrimitiveTypeExpression(PrimitiveType::INT),
																 new IntegerLiteral(0));
		context->items()->append(new ExpressionStatement(i));

		//while ( !(f&1) ) {++i; f >>= 1;}
		auto loop = new LoopStatement(LoopStatement::LoopKind::PreCheck);
		loop->setCondition(new UnaryOperation(UnaryOperation::OperatorTypes::NOT,
														  new BinaryOperation(BinaryOperation::OperatorTypes::AND,
																					 new ReferenceExpression("f"), new IntegerLiteral(1))));
		loop->body()->append(new ExpressionStatement(
										new AssignmentExpression(
											AssignmentExpression::AssignmentTypes::ASSIGN,
											new ReferenceExpression("i"),
											new BinaryOperation(
												BinaryOperation::OperatorTypes::PLUS,
												new ReferenceExpression("i"), new IntegerLiteral(1))))); // TODO: ask increment
		loop->body()->append(new ExpressionStatement(
										new AssignmentExpression(
											AssignmentExpression::AssignmentTypes::RIGHT_SHIFT_SIGNED_ASSIGN,
											new ReferenceExpression("f"), new IntegerLiteral(1))));
		context->items()->append(loop);

		auto qAssertEq = new MethodCallExpression("Q_ASSERT");
		qAssertEq->arguments()->append(new BinaryOperation(BinaryOperation::OperatorTypes::EQUALS,
																			new ReferenceExpression("f"), new IntegerLiteral(1)));
		context->items()->append(new ExpressionStatement(qAssertEq));

		auto opStyleType = new PointerTypeExpression(
					new ReferenceExpression("OperatorStyle",
													new ReferenceExpression("OOVisualization",
																					new GlobalScopeExpression())));
		opStyleType->type()->setQualifiers(Type::Qualifier::CONST);
		auto opStyleInit = new MethodCallExpression("op",
																  new ReferenceTypeExpression(
																	  new MethodCallExpression("style")));
		opStyleInit->arguments()->append(new ReferenceExpression("i"));
		auto opStyle = new VariableDeclarationExpression("opStyle", opStyleType, opStyleInit);
		context->items()->append(new ExpressionStatement(opStyle));

		auto setLayoutStyle = new MethodCallExpression("setStyle", new MethodCallExpression("layout"));
		setLayoutStyle->arguments()->append(new MethodCallExpression("layout",
																						 new ReferenceTypeExpression(
																							 new ReferenceExpression("opStyle"))));
		context->items()->append(new ExpressionStatement(setLayoutStyle));
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::operand_cpp()
{
	auto md = new MetaDefinition("OPERAND_CPP");

	md->arguments()->append(new FormalMetaArgument("name"));

	auto context = new Method("Context");
	{
		auto synchronizeMid = new MethodCallExpression("synchronizeMid",
																new MethodCallExpression("layout"));
		synchronizeMid->arguments()->append(new ReferenceExpression("name##_"));
		synchronizeMid->arguments()->append(new MethodCallExpression("name",
															new MethodCallExpression("node")));
		synchronizeMid->arguments()->append(new ReferenceExpression("index"));
		context->items()->append(new ExpressionStatement(synchronizeMid));

		auto ifStmt = new IfStatement();
		ifStmt->setCondition(new ReferenceExpression("name##_"));
		ifStmt->thenBranch()->append(new ExpressionStatement(
										 new AssignmentExpression(
											 AssignmentExpression::AssignmentTypes::ASSIGN,
											 new ReferenceExpression("index"),
											 new BinaryOperation(
												 BinaryOperation::OperatorTypes::PLUS,
												 new ReferenceExpression("index"),
												 new IntegerLiteral(1)))));
		context->items()->append(ifStmt);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::wrapped_operand_cpp()
{
	auto md = new MetaDefinition("WRAPPED_OPERAND_CPP");

	md->arguments()->append(new FormalMetaArgument("name"));
	md->arguments()->append(new FormalMetaArgument("wrapId"));

	auto context = new Method("Context");
	{
		auto synchronizeMid = new MethodCallExpression("synchronizeMid",
																new MethodCallExpression("layout"));
		synchronizeMid->arguments()->append(new ReferenceExpression("name##_"));
		synchronizeMid->arguments()->append(new MethodCallExpression("name",
															new MethodCallExpression("node")));
		synchronizeMid->arguments()->append(new MethodCallExpression("operand##wrapId##Wrapper",
															new ReferenceTypeExpression(
																new ReferenceExpression("opStyle"))));
		synchronizeMid->arguments()->append(new ReferenceExpression("index"));
		// TODO: method type arguments?
		context->items()->append(new ExpressionStatement(synchronizeMid));

		auto ifStmt = new IfStatement();
		ifStmt->setCondition(new ReferenceExpression("name##_"));
		ifStmt->thenBranch()->append(new ExpressionStatement(
										 new AssignmentExpression(
											 AssignmentExpression::AssignmentTypes::ASSIGN,
											 new ReferenceExpression("index"),
											 new BinaryOperation(
												 BinaryOperation::OperatorTypes::PLUS,
												 new ReferenceExpression("index"),
												 new IntegerLiteral(1)))));
		auto setStyle = new MethodCallExpression("setStyle",
															  new ReferenceExpression("name##_"));
		setStyle->arguments()->append(new MethodCallExpression("operand##wrapId##Wrapper",
																				new ReferenceTypeExpression(
																					new ReferenceExpression("opStyle"))));
		ifStmt->thenBranch()->append(new ExpressionStatement(setStyle));

		context->items()->append(ifStmt);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::preinpostfix_cpp()
{
	auto md = new MetaDefinition("PREINPOSTFIX_CPP");

	md->arguments()->append(new FormalMetaArgument("name"));
	md->arguments()->append(new FormalMetaArgument("condition"));
	md->arguments()->append(new FormalMetaArgument("styleAttribute"));

	auto context = new Method("Context");
	{
		auto synchronizeMid = new MethodCallExpression("synchronizeMid",
																new MethodCallExpression("layout"));
		synchronizeMid->arguments()->append(new ReferenceExpression("name##_"));
		synchronizeMid->arguments()->append(new ReferenceExpression("condition"));
		synchronizeMid->arguments()->append(new MethodCallExpression("styleAttribute",
															new ReferenceTypeExpression(
																new ReferenceExpression("opStyle"))));
		synchronizeMid->arguments()->append(new ReferenceExpression("index"));
		context->items()->append(new ExpressionStatement(synchronizeMid));

		auto ifStmt = new IfStatement();
		ifStmt->setCondition(new ReferenceExpression("name##_"));
		ifStmt->thenBranch()->append(new ExpressionStatement(
										 new AssignmentExpression(
											 AssignmentExpression::AssignmentTypes::ASSIGN,
											 new ReferenceExpression("index"),
											 new BinaryOperation(
												 BinaryOperation::OperatorTypes::PLUS,
												 new ReferenceExpression("index"),
												 new IntegerLiteral(1)))));
		auto setStyle = new MethodCallExpression("setStyle",
															  new ReferenceExpression("name##_"));
		setStyle->arguments()->append(new MethodCallExpression("styleAttribute",
																				 new ReferenceTypeExpression(
																					 new ReferenceExpression("opStyle"))));
		ifStmt->thenBranch()->append(new ExpressionStatement(setStyle));

		context->items()->append(ifStmt);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::prefix_cpp()
{
	auto md = new MetaDefinition("PREFIX_CPP");

	md->arguments()->append(new FormalMetaArgument("condition"));

	auto context = new Method("Context");
	{
		auto metaCall = new MetaCallExpression("PREINPOSTFIX_CPP");
		metaCall->arguments()->append(new ReferenceExpression("prefix"));
		metaCall->arguments()->append(new ReferenceExpression("condition"));
		metaCall->arguments()->append(new ReferenceExpression("preSymbol"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::infix_cpp()
{
	auto md = new MetaDefinition("INFIX_CPP");

	md->arguments()->append(new FormalMetaArgument("condition"));

	auto context = new Class("Context");
	{
		auto metaCall = new MetaCallExpression("PREINPOSTFIX_CPP");
		metaCall->arguments()->append(new ReferenceExpression("infix"));
		metaCall->arguments()->append(new ReferenceExpression("condition"));
		metaCall->arguments()->append(new ReferenceExpression("inSymbol"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::infix2_cpp()
{
	auto md = new MetaDefinition("INFIX2_CPP");

	md->arguments()->append(new FormalMetaArgument("condition"));

	auto context = new Method("Context");
	{
		auto metaCall = new MetaCallExpression("PREINPOSTFIX_CPP");
		metaCall->arguments()->append(new ReferenceExpression("infix2"));
		metaCall->arguments()->append(new ReferenceExpression("condition"));
		metaCall->arguments()->append(new ReferenceExpression("in2Symbol"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::postfix_cpp()
{
	auto md = new MetaDefinition("POSTFIX_CPP");

	md->arguments()->append(new FormalMetaArgument("condition"));

	auto context = new Method("Context");
	{
		auto metaCall = new MetaCallExpression("PREINPOSTFIX_CPP");
		metaCall->arguments()->append(new ReferenceExpression("postfix"));
		metaCall->arguments()->append(new ReferenceExpression("condition"));
		metaCall->arguments()->append(new ReferenceExpression("postSymbol"));
		context->metaCalls()->append(metaCall);
	}

	md->setContext(context);
	return md;
}

MetaDefinition* EnvisionMetaDefinition::begin_standard_expression_visualization_base()
{
	auto md = new MetaDefinition("BEGIN_STANDARD_EXPRESSION_VISUALIZATION_BASE");

	md->arguments()->append(new FormalMetaArgument("apiSpecification"));
	md->arguments()->append(new FormalMetaArgument("className"));
	md->arguments()->append(new FormalMetaArgument("nodeType"));
	md->arguments()->append(new FormalMetaArgument("styleTypeName"));
	md->arguments()->append(new FormalMetaArgument("list"));

	auto mb1 = new MetaBinding("list1");
	{
		mb1->setInput(new ReferenceExpression("list"));

		auto mapping1 = new MetaCallMapping("PREFIX");
		mapping1->setValue(new ReferenceExpression("PREFIX_H"));
		mb1->mappings()->append(mapping1);

		auto mapping2 = new MetaCallMapping("OPERAND");
		mapping2->setValue(new ReferenceExpression("OPERAND_H"));
		mb1->mappings()->append(mapping2);
	}
	md->metaBindings()->append(mb1);

	auto mb2 = new MetaBinding("list2");
	{
		mb2->setInput(new ReferenceExpression("list"));

		auto mapping1 = new MetaCallMapping("PREFIX");
		mapping1->setValue(new ReferenceExpression("PREFIX_CPP"));
		mb2->mappings()->append(mapping1);

		auto mapping2 = new MetaCallMapping("OPERAND");
		mapping2->setValue(new ReferenceExpression("OPERAND_CPP"));
		mb2->mappings()->append(mapping2);
	}
	md->metaBindings()->append(mb2);

	auto context = new Module("Context");
	{
		auto c = new Class("className");
		{
			{ // h
				auto metaCall = new MetaCallExpression("ITEM_COMMON_CUSTOM_STYLENAME");
				metaCall->arguments()->append(new ReferenceExpression("className"));
				metaCall->arguments()->append(new ReferenceExpression("styleTypeName"));
				c->metaCalls()->append(metaCall);
			}
			{ // cpp
				auto metaCall = new MetaCallExpression("ITEM_COMMON_DEFINITIONS");
				metaCall->arguments()->append(new ReferenceExpression("className"));
				metaCall->arguments()->append(new StringLiteral("item"));
				c->metaCalls()->append(metaCall);
			}

			{ // list1
				c->metaCalls()->append(new ReferenceExpression("list1"));
			}

			auto constructor = new Method("className", Modifier::Public, Method::MethodKind::Constructor);
			{
				/*constructor->memberInitializers()->append(
							new MemberInitializer(new ReferenceExpression("Super"),
														 new CommaExpression(new ReferenceExpression("parent"),
																					new CommaExpression(new ReferenceExpression("node"),
																											  new ReferenceExpression("style")))));
*/
				auto argument1 = new FormalArgument("parent",
																new PointerTypeExpression(
																new ReferenceExpression("Item",
																	new ReferenceExpression("Visualization",
																		new GlobalScopeExpression()))));
				constructor->arguments()->append(argument1);

				auto argument2 = new FormalArgument("node",
																new PointerTypeExpression(
																	new ReferenceExpression("NodeType")));
				constructor->arguments()->append(argument2);

				auto argument3Type = new PointerTypeExpression(
							new ReferenceExpression("StyleType"));
				argument3Type->type()->setQualifiers(Type::Qualifier::CONST);
				auto argument3 = new FormalArgument("style", argument3Type);
				argument3->setInitialValue(
							new MethodCallExpression("get",
								new MethodCallExpression("itemStyles")));
				constructor->arguments()->append(argument3);
			}
			c->methods()->append(constructor);

			auto determineChildren = new Method("determineChildren",
															Modifier::Protected | Modifier::Virtual | Modifier::Override);

			determineChildren->items()->append(new ExpressionStatement(
															  new VariableDeclarationExpression("index",
																											new PrimitiveTypeExpression(PrimitiveType::INT),
																											new IntegerLiteral(0))));

			determineChildren->items()->append(new ExpressionStatement(
															  new ReferenceExpression("list2")));

			c->methods()->append(determineChildren);
		}

		context->classes()->append(c);
	}

	md->setContext(context);
	return md;
}

}
