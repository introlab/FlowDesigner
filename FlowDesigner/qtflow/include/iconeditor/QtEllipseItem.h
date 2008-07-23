#ifndef QTELLIPSEITEM_
#define QTELLIPSEITEM_

#include "QtRectItem.h"
#include "QtVertex.h"

class QtEllipseItem : public QtRectItem
{
	Q_OBJECT;
	
public:
	QtEllipseItem(QPointF pos, QGraphicsScene * scene);
	~QtEllipseItem();

	virtual void paint(QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* widget = 0);
};

#endif /*QTELLIPSEITEM_*/
