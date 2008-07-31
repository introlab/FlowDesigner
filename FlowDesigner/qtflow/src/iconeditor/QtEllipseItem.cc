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
#include "iconeditor/QtEllipseItem.h"
#include <QBrush>
#include <QPen>

#include <iostream>

//===============================
// public
//===============================
QtEllipseItem::QtEllipseItem(QGraphicsScene* scene, const QPointF &pos) : QtRectItem(scene, pos)
{   
}

QtEllipseItem::~QtEllipseItem()
{
}

void QtEllipseItem::paint(QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget)
{
	if(isSelected()) {
		QPainterPath path = QtRectItem::shape();
		painter->setPen(QPen(Qt::DashLine));
		painter->drawPath(path);
	}

	painter->setPen(pen());
	painter->setBrush(brush());
	painter->drawEllipse(rect());
}
