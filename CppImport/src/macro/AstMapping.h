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

#pragma once

#include "cppimport_api.h"

#include <ModelBase/src/nodes/Node.h>

namespace CppImport {

class CPPIMPORT_API AstMapping
{
	public:
		QHash<Model::Node*, QVector<clang::SourceRange>>::iterator begin();
		QHash<Model::Node*, QVector<clang::SourceRange>>::iterator end();

		void mapAst(clang::Stmt* clangAstNode, Model::Node* envisionAstNode);
		void mapAst(clang::Decl* clangAstNode, Model::Node* envisionAstNode);

		const QList<Model::Node*> nodes() const;
		QVector<clang::SourceRange> get(Model::Node* node);
		bool contains(Model::Node* node) const;
		void clear();

		Model::Node* closestParentWithAstMapping(Model::Node* node);

	private:
		QHash<Model::Node*, QVector<clang::SourceRange>> astMapping_;

};

inline QHash<Model::Node*, QVector<clang::SourceRange>>::iterator AstMapping::begin() { return astMapping_.begin(); }

inline QHash<Model::Node*, QVector<clang::SourceRange>>::iterator AstMapping::end() { return astMapping_.end(); }

inline const QList<Model::Node*> AstMapping::nodes() const { return astMapping_.keys(); }

inline void AstMapping::clear() { astMapping_.clear(); }

inline bool AstMapping::contains(Model::Node* node) const { return astMapping_.contains(node); }

}