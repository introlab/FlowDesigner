//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)

#ifndef _QTNETWORK_H_
#define _QTNETWORK_H_


#include <QtGui/QGraphicsView>

#include <map>

#include <QMenu>

namespace FD
{
    //forward declaration
    class QtNode;
    class QtLink;
    class QtTerminal;
    class UINetworkController;
    class UINode;
    class UINodeController;
    class UILink;
    class UILinkController;
    
    class QtNetwork : public QGraphicsView
    {
        Q_OBJECT;
        
        public:
        
        QtNetwork(UINetworkController *uiNetwork);
        const std::string getName() const;

        //Node
        QtNode* addNode(UINodeController* node);
        void removeNode(QtNode* node);
        
        //Link
        QtLink* addLink(QtTerminal *source, QtTerminal *dest, UILinkController* link);
        void addQtLink(QtLink *link);
        void removeLink(QtLink* link);        
        
        protected: 
        
        void mouseMoveEvent ( QMouseEvent * e );
        void keyPressEvent(QKeyEvent *event);
        void wheelEvent(QWheelEvent *event);
        void contextMenuEvent(QContextMenuEvent *event);
        QContextMenuEvent *m_contextMenuEvent;
        QMenu* menu;

        // void mouseReleaseEvent(QMouseEvent *event);
        // void drawBackground(QPainter *painter, const QRectF &rect);
        
        void scaleView(qreal scaleFactor);
        
        UINetworkController *m_uiNetwork;
        
        //std::vector<QtNode*> m_nodes;
        //std::vector<QtLink*> m_links;
        //std::map<UINodeController*,QtNode*> m_nodeMap;
        std::map<QtNode*, UINodeController*> m_nodeMap;
        //std::map<UILink*,QtLink*> m_linkMap;
        std::map<QtLink*, UILinkController*> m_linkMap;
        
        bool isNodeExist(const QString &name);
        
        //Drag & Drop
        void dragEnterEvent(QDragEnterEvent *event);
        void dragMoveEvent(QDragMoveEvent *event);
        void dropEvent(QDropEvent *event);        
        
        protected slots:
        void menuTriggered(QAction* action);
        
    };
}//namespace FD
#endif
