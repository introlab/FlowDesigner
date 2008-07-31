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
#include "iconeditor/QtVertex.h"
#include <QBrush>
#include <QPen>

#include <iostream>
//==========================
// public
//==========================
QtVertex::QtVertex(QGraphicsItem * parent, QGraphicsScene * scene) : QGraphicsRectItem(parent, scene)
{
	setAcceptsHoverEvents(true);
    setFlag(ItemIsSelectable);
    setFlag(ItemIsMovable);
    
	QBrush brush(Qt::transparent, Qt::SolidPattern);
	
	QPen pen(Qt::darkGray);
	pen.setWidth(1);
	
	setPen(pen);
	setBrush(brush);
}

QtVertex::~QtVertex() {}

void QtVertex::setPosition(const QPointF &pos)
{
	int halfSize = SIZE/2;
	setRect(pos.x()-halfSize, pos.y()-halfSize, SIZE, SIZE);
}

//==========================
// protected
//==========================
void QtVertex::hoverEnterEvent(QGraphicsSceneHoverEvent* event)
{
	if(!isSelected()) {
		QPen aPen = pen();
		aPen.setWidth(2);
		setPen(aPen);
	}
	QGraphicsRectItem::hoverEnterEvent(event);
}

void QtVertex::hoverLeaveEvent(QGraphicsSceneHoverEvent* event)
{
	if(!isSelected()) {
		QPen aPen = pen();
		aPen.setWidth(1);
		setPen(aPen);
	}
	QGraphicsRectItem::hoverLeaveEvent(event);
}

void QtVertex::mouseMoveEvent(QGraphicsSceneMouseEvent* event)
{
	emit vertexMoved(this, event->pos());
}

QVariant QtVertex::itemChange(GraphicsItemChange change, const QVariant &value)
{
    if(change == QGraphicsItem::ItemSelectedChange && scene())
    {   
        emit vertexSelected(value.toBool(), this);
        if(value.toBool())
        {
            QPen aPen = pen();
			aPen.setWidth(2);
			setPen(aPen);
        }               
        else
        {
            QPen aPen = pen();
			aPen.setWidth(1);
			setPen(aPen);
        }
    }
    return QGraphicsRectItem::itemChange(change, value);
}