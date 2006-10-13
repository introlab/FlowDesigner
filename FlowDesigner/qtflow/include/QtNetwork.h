#ifndef _QTNETWORK_H_
#define _QTNETWORK_H_


#include <QtGui/QGraphicsView>


class Node;
class Edge;
class QGraphicsSceneMouseEvent;
// class QGraphicsScene;

class QtNetwork : public QGraphicsView
{
    Q_OBJECT

    public:
        QtNetwork();

    protected:
        void keyPressEvent(QKeyEvent *event);
        void wheelEvent(QWheelEvent *event);
        void contextMenuEvent(QContextMenuEvent *event);
    // void mouseReleaseEvent(QMouseEvent *event);
    // void drawBackground(QPainter *painter, const QRectF &rect);

        void scaleView(qreal scaleFactor);
    
};

#endif
