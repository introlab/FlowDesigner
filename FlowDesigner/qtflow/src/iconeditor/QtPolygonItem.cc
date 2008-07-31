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
#include "iconeditor/QtPolygonItem.h"
#include <QPen>
#include <QBrush>

#include <iostream>

//===============================
// public
//===============================
QtPolygonItem::QtPolygonItem(QGraphicsScene* scene, const QPointF &pos) : QGraphicsPolygonItem(NULL, scene)
{
	setAcceptsHoverEvents(true);
	setFlag(ItemIsMovable);
    setFlag(ItemIsSelectable);

	this->addVertex(pos);
}

QtPolygonItem::~QtPolygonItem()
{
}

void QtPolygonItem::resize(int vertexIndex, const QPointF &newPos) 
{
	QPolygonF tmpPolygon = polygon();
	if(vertexIndex < tmpPolygon.size()) {
		tmpPolygon[vertexIndex] = newPos;
		setPolygon(tmpPolygon);
	}
	updateVertexes();
}

void QtPolygonItem::addVertex(const QPointF &point)
{
	QPolygonF tmpPolygon = polygon();
	tmpPolygon << point;
	setPolygon(tmpPolygon);
	
	QtVertex* vertex = new QtVertex(this, this->scene());
	// Connect signals vertexMoved
	connect(vertex, SIGNAL(vertexMoved(QtVertex*, const QPointF &)), this, SLOT(vertexMoved(QtVertex*, const QPointF &)));
	m_vertexes.append(vertex);
	
	updateVertexes();
}

//===============================
// public slots
//===============================
void QtPolygonItem::vertexMoved(QtVertex* vertex, const QPointF &newPos)
{
	for(int i=0; i<m_vertexes.size(); i++) {
		if(m_vertexes[i] == vertex) {
			resize(i, newPos);
		}
	}
}


//============================
// protected
//============================
void QtPolygonItem::hoverEnterEvent(QGraphicsSceneHoverEvent* event)
{
	showVertexes(true);
	QGraphicsPolygonItem::hoverEnterEvent(event);
}

void QtPolygonItem::hoverLeaveEvent(QGraphicsSceneHoverEvent* event)
{
	//if(!isSelected()) {
		showVertexes(false);
	//}
	QGraphicsPolygonItem::hoverLeaveEvent(event);
}

//==========================
// private
//==========================
void QtPolygonItem::updateVertexes()
{
	QPolygonF p = polygon();
	for(int i=0; i<p.size() && i<m_vertexes.size(); i++) {
		m_vertexes[i]->setPosition(p[i]);
	}
}

void QtPolygonItem::showVertexes(bool show)
{
	foreach(QtVertex* vertex, m_vertexes) {
		vertex->setVisible(show);
	}
	this->update();
}