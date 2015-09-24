/***********************************************************************************************************************
**
** Copyright (c) 2011, 2015 ETH Zurich
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

#pragma once

#include "../informationscripting_api.h"

#include "ModelBase/src/util/SymbolMatcher.h"

#include "ScopedArgumentQuery.h"

namespace Model {
	class Text;
}

namespace InformationScripting {

class INFORMATIONSCRIPTING_API TagQuery : public ScopedArgumentQuery
{
	public:
		virtual QList<TupleSet> execute(QList<TupleSet> input) override;

		static void registerDefaultQueries();

	private:
		static const QStringList NAME_ARGUMENT_NAMES;
		static const QStringList ADD_ARGUMENT_NAMES;
		static const QStringList REMOVE_ARGUMENT_NAMES;

		ExecuteFunction<TagQuery> exec_{};

		TagQuery(ExecuteFunction<TagQuery> exec, Model::Node* target, QStringList args);
		QList<TupleSet> tags(QList<TupleSet> input);
		QList<TupleSet> queryTags(QList<TupleSet> input);
		QList<TupleSet> addTags(QList<TupleSet> input);
		QList<TupleSet> removeTags(QList<TupleSet> input);

		QList<Model::Text*> allTags(const Model::SymbolMatcher& matcher, Model::Node* target = nullptr);
};

} /* namespace InformationScripting */
