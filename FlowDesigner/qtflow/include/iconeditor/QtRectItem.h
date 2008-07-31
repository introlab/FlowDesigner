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
#ifndef QTRECTITEM_H_
#define QTRECTITEM_H_

#include <QObject>
#include <QGraphicsRectItem>
#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include "QtVertex.h"

/**
 * A rectangle item.
 * @author Mathieu Labbe
 */
class QtRectItem : public QObject, public QGraphicsRectItem
{
	Q_OBJECT;

private:
	/**
	 * The vertexes of the item.
	 */
	QtVertex* m_vertexes[4];
	
	/**
	 * Used on resize when the mouse passed the left edge.
	 */
	bool m_reversedLeft;
	
	/**
	 * Used on resize when the mouse passed the top edge.
	 */
    bool m_reversedTop;

public:
	/**
	 * The constructor.
	 * @param scene the graphics scene of this item
	 * @param pos the position where to insert the item
	 */
	QtRectItem(QGraphicsScene* scene, const QPointF &pos);
	
	/**
	 * The destructor.
	 */
	~QtRectItem();
	
	/**
	 * Resize the item.
	 * @param mousePos the mouse position
	 */
	void resize(const QPointF &mousePos);

public slots:
	/**
	 * Called when a vertex is selected.
	 * @param selected true or false
	 * @param vertex the vertex selected or unselected
	 */
	void vertexSelected(bool selected, QtVertex* vertex);
	
	/**
	 * Resize the item with the new position of the vertex.
	 * @param vertex the vertex moved
	 * @param newPos the new position of the vertex
	 */
	void vertexMoved(QtVertex* vertex, const QPointF &newPos);

protected:
	/**
	 * Show vertexes on mouse hover enter.
	 * @param event the hover event
	 */
	virtual void hoverEnterEvent(QGraphicsSceneHoverEvent* event);
	
	/**
	 * Hide vertexes on mouse hover leave.
	 * @param event the hover event
	 */
	virtual void hoverLeaveEvent(QGraphicsSceneHoverEvent* event);
	
private:	
	/**
	 * Update vertexes's position.
	 */
	void updateVertexes();
	
	/**
	 * Show vertexes.
	 * @param show true or false
	 */
	void showVertexes(bool show);
};

#endif /*QTRECTITEM_H_*/
