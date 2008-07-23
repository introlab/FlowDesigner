#include "iconeditor/QtEllipseItem.h"
#include <QBrush>
#include <QPen>

#include <iostream>

//===============================
// public
//===============================
QtEllipseItem::QtEllipseItem(QPointF pos, QGraphicsScene * scene) : QtRectItem(pos, scene)
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
