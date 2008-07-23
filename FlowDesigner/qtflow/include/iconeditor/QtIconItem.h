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

class QtIconItem
{
public:
	enum { Type = QGraphicsItem::UserType + 15 };
    enum DiagramType { Rectangle, Line, Ellipse, Polygon, Step, Conditional, StartEnd, Io };

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
