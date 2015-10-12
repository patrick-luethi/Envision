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

#include "AstMapping.h"

namespace CppImport {

Model::Node* AstMapping::closestParentWithAstMapping(Model::Node* node)
{
	if (!node) return nullptr;
	if (astMapping_.contains(node)) return node;
	if (node->parent()) return closestParentWithAstMapping(node->parent());

	return nullptr;
}

QHash<Model::Node*, QVector<clang::SourceRange>>::iterator AstMapping::begin() { return astMapping_.begin(); }

QHash<Model::Node*, QVector<clang::SourceRange>>::iterator AstMapping::end() { return astMapping_.end(); }

void AstMapping::mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode)
{
	if (auto bop = clang::dyn_cast<clang::BinaryOperator>(clangAstNode))
		astMapping_[envisionAstNode]
				.append(clang::SourceRange(bop->getOperatorLoc(), bop->getOperatorLoc()));
	else if (auto op = clang::dyn_cast<clang::CXXOperatorCallExpr>(clangAstNode))
		astMapping_[envisionAstNode]
				.append(clang::SourceRange(op->getOperatorLoc(), op->getOperatorLoc()));
	else
		astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
}

void AstMapping::mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode)
{
	if (!astMapping_[envisionAstNode].contains(clangAstNode->getSourceRange()))
		astMapping_[envisionAstNode].append(clangAstNode->getSourceRange());
}

QList<Model::Node*> AstMapping::nodes() { return astMapping_.keys(); }

QVector<clang::SourceRange> AstMapping::get(Model::Node* node)
{
	if (!astMapping_.contains(node)) return {};
	return astMapping_.value(node);
}

void AstMapping::clear() { astMapping_.clear(); }

bool AstMapping::contains(Model::Node* node) { return astMapping_.contains(node); }

}
