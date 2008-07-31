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
#ifndef QTSVGITEM_H
#define QTSVGITEM_H

#include <QGraphicsPixmapItem>

QT_BEGIN_NAMESPACE
class QPixmap;
class QGraphicsSceneMouseEvent;
class QPainter;
class QStyleOptionGraphicsItem;
class QWidget;
class QPolygonF;
class QEllipseF;
QT_END_NAMESPACE

/**
 * An icon item. This class is just used 
 * to create an icon on the toolbox for each diagram 
 * type.
 * @author Mathieu Labbe
 */
class QtIconItem
{
public:
	enum { Type = QGraphicsItem::UserType + 15 };
    enum DiagramType { Rectangle, Line, Ellipse, Polygon };

    QtIconItem(DiagramType diagramType);

    DiagramType diagramType() const
        { return myDiagramType; }
    QPixmap image() const;

private:
    DiagramType myDiagramType;
    QPolygonF myPolygon;
    QRectF myEllipse;
};
    
#endif
