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
#include "iconeditor/QtLineItem.h"
#include <QPen>

#include <iostream>

//===============================
// public
//===============================
QtLineItem::QtLineItem(QGraphicsScene* scene, const QPointF &pos) : QGraphicsLineItem(NULL, scene)
{
	setAcceptsHoverEvents(true);
	setFlag(ItemIsMovable);
    setFlag(ItemIsSelectable);

	setLine(QLineF(pos,pos));
	
	for(int i=0 ; i<2; i++) {
		m_vertexes[i] = new QtVertex(this, scene);
	}
	showVertexes(false);

	// Connect signals vertexMoved
	for(int i=0 ; i<2; i++) {
		connect(m_vertexes[i], SIGNAL(vertexMoved(QtVertex*, const QPointF &)), this, SLOT(vertexMoved(QtVertex*, const QPointF &)));
	}
}

QtLineItem::~QtLineItem()
{
}

void QtLineItem::resize(const QPointF &startPos, const QPointF &mousePos) 
{
	QLineF newLine(startPos, mousePos);
    setLine(newLine);
	updateVertexes();
}

//===============================
// public slots
//===============================
void QtLineItem::vertexMoved(QtVertex* vertex, const QPointF &newPos)
{
	if(vertex == m_vertexes[0]) {
		resize(newPos, line().p2());
	}
	else if(vertex == m_vertexes[1]) {
		resize(line().p1(), newPos);
	}
}


//============================
// protected
//============================
void QtLineItem::hoverEnterEvent(QGraphicsSceneHoverEvent* event)
{
	showVertexes(true);
	QGraphicsLineItem::hoverEnterEvent(event);
}

void QtLineItem::hoverLeaveEvent(QGraphicsSceneHoverEvent* event)
{
	//if(!isSelected()) {
		showVertexes(false);
	//}
	QGraphicsLineItem::hoverLeaveEvent(event);
}

//==========================
// private
//==========================
void QtLineItem::updateVertexes()
{
	QLineF l = line();
	m_vertexes[0]->setPosition(l.p1());
	m_vertexes[1]->setPosition(l.p2());
}

void QtLineItem::showVertexes(bool show)
{
	for(int i=0; i<2; i++) {
		m_vertexes[i]->setVisible(show);
	}
	this->update();
}