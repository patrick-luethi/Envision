/***********************************************************************************************************************
 **
 ** Copyright (c) 2011, 2013 ETH Zurich
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

#include "TypeSystem.h"

namespace Model {
	class Node;
}

namespace OOModel {

class PrimitiveType;
class ArrayType;
class ClassType;
class SymbolProviderType;
class StringType;
class NullType;


class OOMODEL_API JavaTypeSystem : public TypeSystem {
	public:

		virtual TypeRelations relationFirstToSecond(const Type* first, const Type* other) override;

	private:
		bool isStandardJavaLangClass(const QString& className, Model::Node* target);

		TypeRelations relationPrimitiveToOther(const PrimitiveType* primitive, const Type* other);
		int primitiveTypeToSubtypingOrder(const PrimitiveType* primitive);

		TypeRelations relationArrayToOther(const ArrayType* array, const Type* other);
		TypeRelations relationClassToOther(const ClassType* classType, const Type* other);
		TypeRelations relationSymbolProviderToOther(const SymbolProviderType* symbolProvider, const Type* other);
		TypeRelations relationStringToOther(const StringType* stringType, const Type* other);
		TypeRelations relationNullToOther(const NullType* nullType, const Type* other);
};

} /* namespace OOModel */
