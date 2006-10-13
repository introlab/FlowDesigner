//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#ifndef _QTNETWORK_H_
#define _QTNETWORK_H_


#include <QtGui/QGraphicsView>

namespace FD
{

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
}//namespace FD
#endif
