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
        case StartEnd:
            path.moveTo(200, 50);
            path.arcTo(150, 0, 50, 50, 0, 90);
            path.arcTo(50, 0, 50, 50, 90, 90);
            path.arcTo(50, 50, 50, 50, 180, 90);
            path.arcTo(150, 50, 50, 50, 270, 90);
            path.lineTo(200, 25);
            myPolygon = path.toFillPolygon();
            break;
        case Conditional:
            myPolygon << QPointF(-100, 0) << QPointF(0, 100)
                      << QPointF(100, 0) << QPointF(0, -100)
                      << QPointF(-100, 0);
            break;
        case Step:
            myPolygon << QPointF(-100, -100) << QPointF(100, -100)
                      << QPointF(100, 100) << QPointF(-100, 100)
                      << QPointF(-100, -100);
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