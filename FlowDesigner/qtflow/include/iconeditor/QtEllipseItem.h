/***********************************************************************************
** Copyright (C) 2006-2008 Laborius (http://www.gel.usherbrooke.ca/laborius/). 
** All rights reserved. 
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License
** as published by the Free Software Foundation; either version 2
** of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
***********************************************************************************/
#ifndef QTELLIPSEITEM_
#define QTELLIPSEITEM_

#include "QtRectItem.h"
#include "QtVertex.h"

/**
 * An ellipse item.
 * @author Mathieu Labbe
 */
class QtEllipseItem : public QtRectItem
{
	Q_OBJECT;
	
public:
	/**
	 * The constructor.
	 * @param scene the graphics scene of this item
	 * @param pos the position where to insert the item
	 */
	QtEllipseItem(QGraphicsScene* scene, const QPointF &pos);
	
	/**
	 * The destructor.
	 */
	~QtEllipseItem();

	/**
	 * Overloaded method to draw an ellipse instead of a rectangle.
	 * @param painter the painter
	 * @param option style option
	 * @param widget
	 */
	virtual void paint(QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget = 0);
};

#endif /*QTELLIPSEITEM_*/
