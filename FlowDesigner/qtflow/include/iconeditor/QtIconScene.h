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
#ifndef QTSVGSCENE_H
#define QTSVGSCENE_H

#include <QGraphicsScene>
#include "iconeditor/QtIconItem.h"

QT_BEGIN_NAMESPACE
class QGraphicsSceneMouseEvent;
class QMenu;
class QPointF;
class QGraphicsLineItem;
class QColor;
QT_END_NAMESPACE

class QtRectItem;
class QtLineItem;
class QtEllipseItem;
class QtPolygonItem;

/**
 * The graphics scene of the icon editor.
 * TODO: The Decorator pattern could 
 *       be used for vertexes in  the
 *       item classes. Some methods are 
 *       repetitives across the classes.
 * @author Mathieu Labbe
 */
class QtIconScene : public QGraphicsScene
{
    Q_OBJECT

public:
	/**
	 * An enum of item types.
	 */
    enum Mode { InsertRectItem, InsertLineItem, InsertEllipseItem, InsertPolygonItem, MoveItem };

	/**
	 * The constructor.
	 * @param parent the QObject parent
	 */
    QtIconScene(QObject *parent = 0);
    
    /**
     * @return the fill color used in the scene.
     */
    QColor itemColor() const
        { return myItemColor; }
    
    /**
     * @return the line color used in the scene.
     */
    QColor lineColor() const
        { return myLineColor; }
    
    /**
     * Set the line color. If an item is selected, his 
     * line color is changed.
     * @param color the new color
     * @return true if the line color of an item is changed, otherwise false
     */
    bool setLineColor(const QColor &color);
    
    /**
     * Set the fill color. If an item is selected, his 
     * fill color is changed.
     * @param color the new color
     * @return true if the fill color of an item is changed, otherwise false
     */
    bool setItemColor(const QColor &color);
    
    /**
     * Setter.
     * @param drawIt true or false
     */
    void setDrawBackground(bool drawIt);

public slots:
	/**
	 * Set the mode of the scene.
	 * @param mode the new mode
	 */
    void setMode(Mode mode);

signals:
	/**
	 * Emitted when an item is inserted.
	 */
    void itemInserted();
    
    /**
     * Emitted when an item is selected.
     * @param item the item selected
     */
    void itemSelected(QGraphicsItem* item);
    
    /**
     * Emitted when an item is inserted.
     * @param item the item inserted
     */
    void itemInserting(QGraphicsItem* item);

protected:
	/**
	 * If the scene is in inserting mode, a new item is 
	 * created but not yet added to the scene.
	 * @param mouseEvent the mouse event
	 */
    void mousePressEvent(QGraphicsSceneMouseEvent *mouseEvent);
    
    /**
     * Used to resize items.
     * @param mouseEvent the mouse event
     */
    void mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent);
    
    /**
     * If the scene was in inserting mode, the new item is added.
     * @param mouseEvent the mouse event
     */
    void mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent);
    
    /**
     * Draw the background of the scene.
     */
    virtual void drawBackground(QPainter *painter, const QRectF &rect);

private:
    Mode myMode;
    bool leftButtonDown;

    QColor myItemColor;
    QColor myLineColor;
    
    QtRectItem* m_tmpRectItem;
    QtLineItem* m_tmpLineItem;
    QtEllipseItem* m_tmpEllipseItem;
    QtPolygonItem* m_tmpPolygonItem;
    
    QVector<QtLineItem*> m_tmpPolygonTmpLines;
    bool m_insertingPolygon;
    
    bool m_drawBackground;
};

#endif
