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

#include "HQuery.h"

#include "../nodes/QueryNode.h"
#include "../nodes/QueryNodeContainer.h"
#include "../nodes/CommandNode.h"
#include "../nodes/CommandArgument.h"
#include "../nodes/CompositeQueryNode.h"
#include "../nodes/EmptyQueryNode.h"
#include "../nodes/OperatorQueryNode.h"

#include "../parsing/SimpleQueryParser.h"

#include "../visualization/VCommandNode.h"
#include "../visualization/VCommandArgument.h"
#include "../visualization/VEmptyQueryNode.h"
#include "../visualization/VCompositeQueryNode.h"

#include "OOInteraction/src/string_offset_providers/StringComponents.h"
#include "OOInteraction/src/string_offset_providers/StringOffsetProvider.h"

#include "Core/src/AdapterManager.h"

#include "OOInteraction/src/string_offset_providers/GridBasedOffsetProvider.h"
#include "OOInteraction/src/string_offset_providers/Cell.h"
#include "OOInteraction/src/string_offset_providers/ListCell.h"
#include "OOInteraction/src/handlers/SetExpressionCursorEvent.h"

namespace InformationScripting {

HQuery* HQuery::instance()
{
	static HQuery instance;
	return &instance;
}

void HQuery::initStringComponents()
{
	using namespace OOInteraction;

	StringComponents::add<CommandNode>([](CommandNode* command) {
		return StringComponents::c(command->name(),
			StringComponents::list(command->arguments(), " ", " ", "", true, true));
	});
	StringComponents::add<CommandArgument>([](CommandArgument* argument) {
		return StringComponents::c(argument->argument());
	});
	StringComponents::add<CompositeQueryNode>([](CompositeQueryNode* composite) {
		return StringComponents::c(StringComponents::list(composite->queries(),
																		  SimpleQueryParser::LIST_LEFT,
																		  SimpleQueryParser::LIST_DELIM,
																		  SimpleQueryParser::LIST_RIGHT, true, true));
	});

	StringComponents::add<EmptyQueryNode>([](EmptyQueryNode*) {
		return StringComponents::c("");
	});

	StringComponents::add<OperatorQueryNode>([](OperatorQueryNode* opNode) {
		return StringComponents::c(opNode->left(),
			StringComponents::choose(opNode->op(),
								OperatorQueryNode::OperatorTypes::Pipe, "|",
								OperatorQueryNode::OperatorTypes::Substract, "-",
								OperatorQueryNode::OperatorTypes::Union, "U"),
					opNode->right());
	});

	GridBasedOffsetProvider::addGridConstructor<VCommandNode>(
	[](GridBasedOffsetProvider* grid, VCommandNode* vis){
		grid->add(new Cell(0, vis->name(), 0));
		grid->add(new ListCell(1, vis->arguments(), 1, " ", " ", ""));
	});

	GridBasedOffsetProvider::addGridConstructor<VCompositeQueryNode>(
	[](GridBasedOffsetProvider* grid, VCompositeQueryNode* vis){
		grid->add(new ListCell(0, vis->queries(), 0,
									  SimpleQueryParser::LIST_LEFT,
									  SimpleQueryParser::LIST_DELIM,
									  SimpleQueryParser::LIST_RIGHT));
	});

	GridBasedOffsetProvider::addGridConstructor<VCommandArgument>(
	[](GridBasedOffsetProvider* grid, VCommandArgument* vis){
		grid->add(new Cell(0, vis->argument(), 0));
	});

	GridBasedOffsetProvider::addGridConstructor<VEmptyQueryNode>(
	[](GridBasedOffsetProvider* grid, VEmptyQueryNode* vis){
		grid->add(new Cell(0, vis->empty(), 0));
	});
}

void HQuery::keyPressEvent(Visualization::Item* target, QKeyEvent* event)
{
	qDebug() << "keyPressed" << event;
	target->setUpdateNeeded(Visualization::Item::StandardUpdate);

	auto key = event->key();
//	auto modifiers = event->modifiers();

	bool enterPressed = key == Qt::Key_Enter || key == Qt::Key_Return;
//	bool spacePressed = key == Qt::Key_Space;

	QString str;
	int index;
	auto topMostItem = stringInfo(target, (Qt::Key) key, str, index);
	qDebug() << str << index;
	QString newText = str;
	int newIndex = index;
	removeListsWithOneElement(newText, newIndex);

	switch (key)
	{
		// Below we let CompoundObjectDescriptor process Delete and Backspace since it might need to remove
		// extra characters if those keys are pressed just on the boundary of a compound object
		case Qt::Key_Delete:
		{
			if (index < newText.size() )
			{
				if (! processDeleteOrBackspace(Qt::Key_Delete, newText, newIndex))
					newText.remove(index, 1);
			}
		} break;
		case Qt::Key_Backspace:
		{
			if (index > 0 )
			{
				if (! processDeleteOrBackspace(Qt::Key_Backspace, newText, newIndex))
				{break;
					newText.remove(index-1, 1);
					--newIndex;
				}
			}
		} break;
		case Qt::Key_Enter: // Fallthrough
		case Qt::Key_Return:
		{
			if (event->modifiers() == Qt::ControlModifier)
				newIndex = processEnter(newText, index);
		} break;
		default:
		{
			if (!enterPressed && !event->text().isEmpty())
			{
				newText.insert(index, event->text());
				newIndex += event->text().size();
			}
		} break;
	}

	if (!enterPressed || (enterPressed && event->modifiers() == Qt::ControlModifier))
		setNewQuery(target, topMostItem, newText, newIndex);
}

QueryNodeContainer* HQuery::parentContainer(InformationScripting::QueryNode* e)
{
	// Is this expression part of an expression statement
	auto ep = e->parent();
	while (ep && !DCast<QueryNodeContainer>(ep)) ep = ep->parent();

	return DCast<QueryNodeContainer>(ep);
}

Visualization::Item* HQuery::stringInfo(Visualization::Item* target, Qt::Key key, QString& str, int& index)
{
	auto topMostItem = target;
	auto* topMostSP = Core::AdapterManager::adapt<OOInteraction::StringOffsetProvider>(topMostItem);

	auto p = topMostItem->parent();
	while (p)
	{
		auto* adapted = Core::AdapterManager::adapt<OOInteraction::StringOffsetProvider>(p);
		if (adapted)
		{
			SAFE_DELETE(topMostSP);
			topMostSP = adapted;
			topMostItem = p;
		}
		p = p->parent();
	}

	str = topMostSP->string();
	index = topMostSP->offset( key );
	SAFE_DELETE(topMostSP);

	return topMostItem;
}

void HQuery::setNewQuery(Visualization::Item* target, Visualization::Item* topMostItem, const QString& text, int index)
{
	QString newText = text;
	qDebug() << "NewText" << text;
	QueryNode* newQuery = SimpleQueryParser::parse(newText);

	Model::Node* containerNode = topMostItem->node()->parent();
	Q_ASSERT(containerNode);
	containerNode->replaceChild(topMostItem->node(), newQuery);

	// Compute the new offset. This can change in case the string of the new expression is different.
	QString expString = OOInteraction::StringComponents::stringForNode(newQuery);
	qDebug() << expString;
	index += expString.length() - newText.length();

	// Find an item that represents a node, as any intermediate items might disappear when during the update.
	// E.g. VLoopStatement keeps it's condition inside a wrapper for background color that also get deleted during the
	// update.
	auto parent = topMostItem->parent();
	while (!parent->node() && parent->parent()) parent=parent->parent();

	target->scene()->addPostEventAction(new OOInteraction::SetExpressionCursorEvent(parent, newQuery, index));
}

bool HQuery::processDeleteOrBackspace(Qt::Key, QString&, int&)
{
	return false;
}

int HQuery::processEnter(QString& exp, int index)
{
	// Insert a new list delimiter
	exp.insert(index, SimpleQueryParser::LIST_DELIM);
	int finalIndex = index+1;

	auto findBoundary = [&exp](int startIndex, bool forward, int& foundIndex) -> bool {
		// Check if it is needed to insert the list syntax
		auto needsDelimiter = false;
		foundIndex = startIndex;
		int subLists = 0;
		while ( foundIndex >= -1 && foundIndex <= exp.length()) // +-1 outside of range
		{
			Q_ASSERT( (foundIndex>=0 && foundIndex< exp.length()) || subLists == 0);

			if (subLists == 0)
			{
				if (foundIndex < 0 || foundIndex == exp.length() || exp[foundIndex] == SimpleQueryParser::OP_PIPE)
				{
					// We don't have a list, we must insert the list delimiters
					needsDelimiter = true;
					break;
				}
				else if (exp[foundIndex] == SimpleQueryParser::LIST_DELIM ||
							exp[foundIndex] == (forward?SimpleQueryParser::LIST_RIGHT:SimpleQueryParser::LIST_LEFT))
					break; // We already have a list, so nothing to do
				else if (exp[foundIndex] == (forward?SimpleQueryParser::LIST_LEFT:SimpleQueryParser::LIST_RIGHT) ) {
					++subLists;
				}
			}
			else if (exp[foundIndex] == (forward?SimpleQueryParser::LIST_RIGHT:SimpleQueryParser::LIST_LEFT)) --subLists;

			if (forward) ++foundIndex;
			else --foundIndex;
		}
		Q_ASSERT(subLists == 0);

		return needsDelimiter;
	};

	int frontIndex, backIndex;
	auto frontNeedsDelim = findBoundary(index-1, false, frontIndex);
	auto backNeedsDelim = findBoundary(index+1, true, backIndex);

	if (frontNeedsDelim || backNeedsDelim )
	{
		exp.insert(frontIndex+1, SimpleQueryParser::LIST_LEFT);
		++finalIndex;
		exp.insert(backIndex+1, SimpleQueryParser::LIST_RIGHT); // +1 because the previous insert shifted the text by 1
	}

	return finalIndex;
}

int HQuery::removeListsWithOneElement(QString& exp, int& index, int iteratorIndex)
{
	int listStart = iteratorIndex;
	++iteratorIndex;
	int numDelims = 0;

	while (iteratorIndex < exp.length())
	{
		if (exp[iteratorIndex] == SimpleQueryParser::LIST_DELIM)
		{
			++numDelims;
			++iteratorIndex;
		}
		else if (exp[iteratorIndex] == SimpleQueryParser::LIST_LEFT)
			iteratorIndex = removeListsWithOneElement(exp, index, iteratorIndex);
		else if (exp[iteratorIndex] == SimpleQueryParser::LIST_RIGHT)
		{
			if (numDelims == 0) {
				if (index > iteratorIndex) --index;
				exp.remove(iteratorIndex, 1);
				if (index > listStart) --index;
				exp.remove(listStart, 1);
				iteratorIndex -= 1;
			}
			else
				++iteratorIndex;

			break;
		}
		else ++iteratorIndex;
	}

	Q_ASSERT(listStart >= 0 || iteratorIndex >= exp.length());
	return iteratorIndex;
}

}
