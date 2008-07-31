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
#ifndef QTPOLYGONITEM_H_
#define QTPOLYGONITEM_H_

#include <QObject>
#include <QGraphicsPolygonItem>
#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include "QtVertex.h"

/**
 * A polygon item.
 * @author Mathieu Labbe
 */
class QtPolygonItem : public QObject, public QGraphicsPolygonItem
{
	Q_OBJECT;

private:
	/**
	 * Vertexes of the item.
	 */
	QVector<QtVertex*> m_vertexes;
	
public:
	/**
	 * The constructor.
	 * @param scene the graphics scene of this item
	 * @param pos the position where to insert the item
	 */
	QtPolygonItem(QGraphicsScene* scene, const QPointF &pos);
	
	/**
	 * The destructor.
	 */
	~QtPolygonItem();
	
	/**
	 * Resize the polygon.
	 * @param vertexIndex the index of the vertex moved
	 * @param newPos the new position of the vertex
	 */
	void resize(int vertexIndex, const QPointF &newPos);
	
	/**
	 * Add a vertex to the polygon.
	 * @param point the position of the new vertex
	 */
	void addVertex(const QPointF &point);

public slots:
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

#endif /*QTPOLYGONITEM_H_*/
