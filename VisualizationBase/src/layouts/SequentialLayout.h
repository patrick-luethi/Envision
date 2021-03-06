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

#include "../visualizationbase_api.h"

#include "Layout.h"
#include "SequentialLayoutStyle.h"

namespace Model { class Node; }

namespace Visualization {

class ModelRenderer;

class VISUALIZATIONBASE_API SequentialLayout: public Super<Layout>
{
	ITEM_COMMON(SequentialLayout)

	public:
		SequentialLayout(Item* parent, const StyleType* style = itemStyles().get());
		~SequentialLayout();

		virtual bool isEmpty() const;

		virtual void updateGeometry(int availableWidth, int availableHeight);
		virtual bool sizeDependsOnParent() const override;

		int focusedElementIndex() const;

		virtual QList<ItemRegion> regions();

		bool isHorizontal() const;
		bool isForward() const;

		int length() const;

		void append(Item* item);
		void prepend(Item* item);
		void insert(Item* item, int position);
		void swap(int i, int j);

		void remove(int index, bool deleteItem = true);
		void removeAll(Item* item, bool deleteItem = true);
		void clear(bool deleteItems = true);

		template <class T> T* at(int index);
		template <class T> T* at(int index) const;

		int spaceBetweenElements() const;
		void setSpaceBetweenElements(bool use, int space = 0);

		void synchronizeWithNodes(const QList<Model::Node*>& nodes, ModelRenderer* renderer);

		// Synchronize methods with a node
		void synchronizeFirst(Item*& item, Model::Node* node);
		void synchronizeLast(Item*& item, Model::Node* node);
		void synchronizeMid(Item*& item, Model::Node* node, int position);

		// Synchronize methods with a specific Visualization
		template <class FieldType, class VisualizationType = FieldType>
		void synchronizeFirst(FieldType*& item, bool present, const typename VisualizationType::StyleType* style);

		template <class FieldType, class VisualizationType = FieldType>
		void synchronizeLast(FieldType*& item, bool present, const typename VisualizationType::StyleType* style);

		template <class FieldType, class VisualizationType = FieldType>
		void synchronizeMid(FieldType*& item, bool present,
				const typename VisualizationType::StyleType* style, int position);

		// Synchronize methods with a specific Visualization for a specific Node
		template <class FieldType, class VisualizationType = FieldType>
		void synchronizeFirst(FieldType*& item, typename VisualizationType::NodeType* node,
				const typename VisualizationType::StyleType* style);

		template <class FieldType, class VisualizationType = FieldType>
		void synchronizeLast(FieldType*& item, typename VisualizationType::NodeType* node,
				const typename VisualizationType::StyleType* style);

		template <class FieldType, class VisualizationType = FieldType>
			void synchronizeMid(FieldType*& item, typename VisualizationType::NodeType* node,
				const typename VisualizationType::StyleType* style, int position);

	private:
		QVector<Item*> items;
		int spaceBetweenElements_;

		void adjustCursorRegionToAvoidZeroSize(QRect& region, bool horizontal, bool forward, bool first, bool last);
};

inline bool SequentialLayout::isHorizontal() const { return style()->isHorizontal(); }
inline bool SequentialLayout::isForward() const { return style()->isForward(); }

template <class T> inline T* SequentialLayout::at(int index) { return static_cast<T*> (items[index]); }
template <class T> inline T* SequentialLayout::at(int index) const { return static_cast<T*> (items[index]); }

template <class FieldType, class VisualizationType>
void SequentialLayout::synchronizeFirst(FieldType*& item, bool present,
		const typename VisualizationType::StyleType* style)
{
	synchronizeMid<FieldType, VisualizationType>(item, present, style, 0);
}

template <class FieldType, class VisualizationType>
void SequentialLayout::synchronizeLast(FieldType*& item, bool present,
		const typename VisualizationType::StyleType* style)
{
	synchronizeMid<FieldType, VisualizationType>(item, present, style, length());
}

template <class FieldType, class VisualizationType>
void SequentialLayout::synchronizeMid(FieldType*& item, bool present,
		const typename VisualizationType::StyleType* style, int position)
{
	if (item && !present)
	{
		removeAll(item);
		item = nullptr;
	}

	if (!item && present)
	{
		if (style) item = new VisualizationType(nullptr, style);
		else item = new VisualizationType(nullptr);

		insert(item, ((position > length()) ? length() : position) );
	}
}

template <class FieldType, class VisualizationType>
void SequentialLayout::synchronizeFirst(FieldType*& item, typename VisualizationType::NodeType* node,
		const typename VisualizationType::StyleType* style)
{
	synchronizeMid<FieldType, VisualizationType>(item, node, style, 0);
}

template <class FieldType, class VisualizationType>
void SequentialLayout::synchronizeLast(FieldType*& item, typename VisualizationType::NodeType* node,
		const typename VisualizationType::StyleType* style)
{
	synchronizeMid<FieldType, VisualizationType>(item, node, style, length());
}

template  <class FieldType, class VisualizationType>
void SequentialLayout::synchronizeMid(FieldType*& item, typename VisualizationType::NodeType* node,
		const typename VisualizationType::StyleType* style, int position)
{
	if (item && item->node() != node)
	{
		removeAll(item);
		item = nullptr;
	}

	if (!item && node)
	{
		if (style) item = new VisualizationType(nullptr, node, style);
		else item = new VisualizationType(nullptr, node);

		insert(item, ((position > length()) ? length() : position) );
	}
}

}
