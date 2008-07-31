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
#ifndef QTVERTEX_H_
#define QTVERTEX_H_

#include <QObject>
#include <QGraphicsRectItem>
#include <QGraphicsSceneHoverEvent>
#include <QPainter>

/**
 * A vertex object. 
 */
class QtVertex : public QObject, public QGraphicsRectItem
{
	Q_OBJECT;
	
public:
	/**
	 * The size of the rectangle.
	 */
	static const int SIZE = 10;
	
public:
	/**
	 * The constructor.
	 * @param parent the graphic item parent
	 * @param scene the scene of the graphic item
	 */
	QtVertex(QGraphicsItem * parent, QGraphicsScene * scene);
	
	/**
	 * The destructor.
	 */
	~QtVertex();
	
	/**
	 * Set the position of the vertex.
	 * @param pos the new position
	 */
	void setPosition(const QPointF &pos);
		
signals:
	/**
	 * Emitted when a vertex is selected.
	 * @param selected true or false
	 * @param vertex the vertex selected or unselected
	 */
	void vertexSelected(bool selected, QtVertex* vertexRect);
	
	/**
	 * Emitted when the vertex is moved.
	 * @param vertex the vertex moved
	 * @param newPos the new position
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
	
	/**
	 * Emit his new position.
	 * @param event the mosue event
	 */
	virtual void mouseMoveEvent(QGraphicsSceneMouseEvent* event);
	
	/**
	 * Modify the vertex properties depending of the change.
	 * @param change the change occured
	 * @param value the value of the change
	 * @return the same value
	 */
	virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);
};

#endif /*QTVERTEX_H_*/
