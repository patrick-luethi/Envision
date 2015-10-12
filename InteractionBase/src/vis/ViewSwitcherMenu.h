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
#include "Menu.h"
#include "MenuStyle.h"

namespace Interaction {

class INTERACTIONBASE_API ViewSwitcherMenu : public Super<Menu>
{

		ITEM_COMMON_CUSTOM_STYLENAME(ViewSwitcherMenu, MenuStyle)

	public:
		/**
		 * Shows a ViewSwitcherMenu, which is initialized on the given target.
		 */
		static void show(Visualization::Item* target);

	protected:
		virtual bool sceneEventFilter(QGraphicsItem* watched, QEvent* event) override;

		virtual bool executeEntry(Visualization::Item* item) override;
		virtual void startFocusMode(Visualization::Item* target) override;
		virtual void endFocusMode(Visualization::Item* target) override;

	private:
		static void showNow(Visualization::Item* target);

		bool inEditMode_{};
		QString nameBefore_;

		static QHash<int, QPoint> keyToIndexMap_;

		ViewSwitcherMenu(QVector<QVector<Visualization::Item*>> items, Visualization::Item* target,
							  StyleType* style = itemStyles().get());
};

}
