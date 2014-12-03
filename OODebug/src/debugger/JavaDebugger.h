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

#pragma once

#include "../oodebug_api.h"
#include "jdwp/DebugConnector.h"
#include "metadata/Breakpoint.h"

namespace Model {
	class Node;
	class TreeManager;
}

namespace OOModel {
	class Class;
}

namespace Visualization {
	class Item;
	class MessageOverlay;
}

namespace OODebug {

class Location;

class OODEBUG_API JavaDebugger
{
	public:
		static JavaDebugger& instance();
		void debugTree(Model::TreeManager* manager, const QString& pathToProjectContainerDirectory);
		bool addBreakpoint(Visualization::Item* target, QKeyEvent* event);
		bool resume(Visualization::Item* target, QKeyEvent* event);

	private:
		JavaDebugger();
		Visualization::MessageOverlay* addBreakpointOverlay(Visualization::Item* target);
		QString jvmSignatureFor(OOModel::Class* theClass);
		/**
		 * Returns a String with all containing module names split by \a delim in front of the \a clazz name.
		 */
		QString fullNameFor(OOModel::Class* theClass, QChar delimiter);

		Location nodeToLocation(Model::Node* node);

		void handleClassPrepare(Event e);

		DebugConnector debugConnector_;

		QHash<Visualization::Item*, Breakpoint> breakpoints_;
};

} /* namespace OODebug */
