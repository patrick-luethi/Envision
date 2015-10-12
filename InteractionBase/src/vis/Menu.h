/***********************************************************************************************************************
 **
 ** Copyright (c) 2015 ETH Zurich
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

#include "interactionbase_api.h"
#include "ModelBase/src/nodes/Node.h"
#include "VisualizationBase/src/declarative/DeclarativeItem.h"
#include "MenuStyle.h"

namespace Visualization {
	class Text;
}
namespace Interaction {

/**
 * This class provides a standard interface for creating a menu which is opened
 * at the last cursor position. Any kind of node can be used and actions may be
 * associated with the nodes. The interface can also optionally contain a text field
 * which is located above the normal items, where any text can be entered.
 */
class INTERACTIONBASE_API Menu : public Super<Visualization::DeclarativeItem<Menu>> {

	ITEM_COMMON(Menu)

	public:
		virtual ~Menu();

		/**
		 * Hides the active menu.
		 */
		static void hide();
		/**
		 * Returns whether a menu is currently visible on screen.
		 */
		static bool isVisible();

		/**
		 * The currently focused item. Either the text field,
		 * or a visualization of one of the nodes.
		 */
		Visualization::Item* focusedItem() const;
		/**
		 * Selects the given item as the focused item.
		 */
		void selectItem(Visualization::Item* item);
		QVector<QVector<Visualization::Item*>> currentItems() const;
		/**
		 * Executes the currently focused item's function.
		 * Returns whether it was successfully executed.
		 */
		bool executeFocused();

		static void initializeForms();

		virtual void updateGeometry(int availableWidth, int availableHeight) override;

	protected:
		static Menu* instance;
		static void hideNow();

		virtual bool sceneEventFilter(QGraphicsItem* watched, QEvent* event) override;
		/**
		 * The function to execute when selecting the given visualization.
		 */
		virtual bool executeEntry(Visualization::Item* item) = 0;
		/**
		 * Start focus mode with the given target to be focused.
		 */
		virtual void startFocusMode(Visualization::Item* target) = 0;
		/**
		 * End focus mode on the given target.
		 */
		virtual void endFocusMode(Visualization::Item* target) = 0;

		/**
		 * The index of the given item in the node grid. Returns (-1, -1) if not found.
		 */
		QPoint indexOf(Visualization::Item* item) const;

		Menu(QVector<QVector<Visualization::Item*>> items, Visualization::Item* selectedItem,
			 Visualization::Item* target, StyleType* style = itemStyles().get());

	private:

		/**
		 * Corrects the coordinates given by the point to ensure they are located within the grid.
		 * Implements a wraparound technique if the position selected is out of bounds.
		 */
		QPoint correctCoordinates(QPoint point) const;

		Visualization::Item* target_{};
		QPointF mousePosition_;
		QVector<QVector<Visualization::Item*>> currentItems_;

		bool inFocusMode_{};
		Visualization::Item* focusedItem_{};

};

inline QVector<QVector<Visualization::Item*>> Menu::currentItems() const { return currentItems_; }
inline Visualization::Item* Menu::focusedItem() const { return focusedItem_; }
inline bool Menu::isVisible() { return instance; }

}
