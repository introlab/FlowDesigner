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
#include <QtGui>

#include "iconeditor/QtIconItem.h"

QtIconItem::QtIconItem(DiagramType diagramType)
{
    myDiagramType = diagramType;
    QPainterPath path;
    switch (myDiagramType) {
    	case Rectangle:
            myPolygon << QPointF(-100, -100) << QPointF(100, -100)
                      << QPointF(100, 100) << QPointF(-100, 100)
                      << QPointF(-100, -100);
            break;
        case Line:
            myPolygon << QPointF(-100, -100) << QPointF(-95, -100)
                      << QPointF(105, 100) << QPointF(100, 100)
                      << QPointF(-100, -100);
            break;
        case Ellipse:
            myEllipse = QRectF(QPointF(-100, -80), QSizeF(200, 160));
            break;
        default:
            myPolygon << QPointF(-120, -80) << QPointF(-70, 80)
                      << QPointF(120, 80) << QPointF(70, -80)
                      << QPointF(-120, -80);
            break;
    }
}

QPixmap QtIconItem::image() const
{
    QPixmap pixmap(250, 250);
    pixmap.fill(Qt::transparent);
    QPainter painter(&pixmap);
    painter.setPen(QPen(Qt::black, 8));
    painter.translate(125, 125);
    
    if(myDiagramType == Ellipse) {
    	painter.drawEllipse(myEllipse);
    }
    else {
    	painter.drawPolyline(myPolygon);
    }
    

    return pixmap;
}