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

class QtIconScene : public QGraphicsScene
{
    Q_OBJECT

public:
    enum Mode { InsertRectItem, InsertLineItem, InsertEllipseItem, InsertPolygonItem, MoveItem };

    QtIconScene(QObject *parent = 0);
    QColor itemColor() const
        { return myItemColor; }
    QColor lineColor() const
        { return myLineColor; }
    bool setLineColor(const QColor &color);
    bool setItemColor(const QColor &color);
    void setDrawBackground(bool drawIt);

public slots:
    void setMode(Mode mode);

signals:
    void itemInserted();
    void itemSelected(QGraphicsItem* item);
    void itemInserting(QGraphicsItem* item);

protected:
    void mousePressEvent(QGraphicsSceneMouseEvent *mouseEvent);
    void mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent);
    void mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent);
    
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
