//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)

#ifndef _QTNETWORK_H_
#define _QTNETWORK_H_


#include <QtGui/QGraphicsView>
#include "UINetwork.h"
#include <map>

namespace FD
{
    //forward declaration
    class QtNode;
    class QtLink;      
    
    class QtNetwork : public QGraphicsView
    {
        Q_OBJECT

    public:
        QtNetwork(UINetwork *uiNetwork = NULL);
		
		void addQtLink(QtLink *link);
              

    protected:
		void mouseMoveEvent ( QMouseEvent * e );
        void keyPressEvent(QKeyEvent *event);
        void wheelEvent(QWheelEvent *event);
        void contextMenuEvent(QContextMenuEvent *event);
        // void mouseReleaseEvent(QMouseEvent *event);
        // void drawBackground(QPainter *painter, const QRectF &rect);

        void scaleView(qreal scaleFactor);

        UINetwork *m_uiNetwork;
                    
        //std::vector<QtNode*> m_nodes;
        //std::vector<QtLink*> m_links;
        std::map<UINode*,QtNode*> m_nodeMap;
        std::map<UILink*,QtLink*> m_linkMap;
                 
                     
        //Drag & Drop
        void dragEnterEvent(QDragEnterEvent *event);
        void dragMoveEvent(QDragMoveEvent *event);
        void dropEvent(QDropEvent *event);        
        
    };
}//namespace FD
#endif
