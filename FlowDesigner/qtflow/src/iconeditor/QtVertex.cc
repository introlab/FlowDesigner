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

void QtVertex::setPosition(QPointF pos)
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