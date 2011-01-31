/***********************************************************************************************************************
 * TypedListInstantiations.cpp
 *
 *  Created on: Jan 28, 2011
 *      Author: Dimitar Asenov
 **********************************************************************************************************************/

#include "ModelBase/headers/nodes/TypedList.h"
#include "ModelBase/headers/nodes/TypedListDefinition.h"

#include "Project.h"
#include "Module.h"
#include "Class.h"
#include "Method.h"
#include "MethodItem.h"
#include "FormalArgument.h"

#include "statements/Statement.h"
#include "statements/Block.h"
#include "statements/BreakStatement.h"
#include "statements/ContinueStatement.h"
#include "statements/ForEachStatement.h"
#include "statements/IfStatement.h"
#include "statements/LoopStatement.h"
#include "statements/ReturnStatement.h"
#include "statements/SwitchCase.h"
#include "statements/SwitchStatement.h"
#include "statements/VariableDeclaration.h"

#include "expressions/Expression.h"
#include "expressions/IntegerLiteral.h"
#include "expressions/FloatLiteral.h"
#include "expressions/StringLiteral.h"
#include "expressions/BooleanLiteral.h"
#include "expressions/CharacterLiteral.h"
#include "expressions/NullLiteral.h"
#include "expressions/ThisExpression.h"
#include "expressions/VariableAccess.h"
#include "expressions/NewExpression.h"
#include "expressions/MethodCall.h"
#include "expressions/UnaryOperation.h"
#include "expressions/BinaryOperation.h"
#include "expressions/CastExpression.h"

#include "types/Type.h"
#include "types/PrimitiveType.h"
#include "types/NamedType.h"

template class Model::TypedList<OOModel::Class>;
template class Model::TypedList<OOModel::Project>;
template class Model::TypedList<OOModel::Module>;
template class Model::TypedList<OOModel::Method>;
template class Model::TypedList<OOModel::MethodItem>;
template class Model::TypedList<OOModel::FormalArgument>;

template class Model::TypedList<OOModel::Statement>;
template class Model::TypedList<OOModel::Block>;
template class Model::TypedList<OOModel::BreakStatement>;
template class Model::TypedList<OOModel::ContinueStatement>;
template class Model::TypedList<OOModel::ForEachStatement>;
template class Model::TypedList<OOModel::IfStatement>;
template class Model::TypedList<OOModel::LoopStatement>;
template class Model::TypedList<OOModel::ReturnStatement>;
template class Model::TypedList<OOModel::SwitchCase>;
template class Model::TypedList<OOModel::SwitchStatement>;
template class Model::TypedList<OOModel::VariableDeclaration>;

template class Model::TypedList<OOModel::Expression>;
template class Model::TypedList<OOModel::IntegerLiteral>;
template class Model::TypedList<OOModel::FloatLiteral>;
template class Model::TypedList<OOModel::StringLiteral>;
template class Model::TypedList<OOModel::BooleanLiteral>;
template class Model::TypedList<OOModel::CharacterLiteral>;
template class Model::TypedList<OOModel::NullLiteral>;
template class Model::TypedList<OOModel::ThisExpression>;
template class Model::TypedList<OOModel::VariableAccess>;
template class Model::TypedList<OOModel::NewExpression>;
template class Model::TypedList<OOModel::MethodCall>;
template class Model::TypedList<OOModel::UnaryOperation>;
template class Model::TypedList<OOModel::BinaryOperation>;
template class Model::TypedList<OOModel::CastExpression>;

template class Model::TypedList<OOModel::Type>;
template class Model::TypedList<OOModel::PrimitiveType>;
template class Model::TypedList<OOModel::NamedType>;
