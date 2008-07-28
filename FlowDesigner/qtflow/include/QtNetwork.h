//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)

#ifndef _QTNETWORK_H_
#define _QTNETWORK_H_


#include <QtGui/QGraphicsView>
#include "UINetwork.h"
#include <map>
#include <QMenu>
#include "QtNote.h"

namespace FD
{
    //forward declaration
    class QtNode;
    class QtLink;
    class QtTerminal;
    class QtDocument;
    class UINetwork;
    class UINode;
    class UILink;

    
    class QtNetwork : public QGraphicsView, public UINetwork::UINetworkObserverIF
    {
        Q_OBJECT;
        
        public:
        
        QtNetwork(QtDocument *parent, UINetwork *uiNetwork);
        
        ~QtNetwork();
        
        const std::string getName() const;
        
        //Network
        UINetwork* getUINetWork() {return m_uiNetwork;}

        //Node
        QtNode* addNode(UINode* node);
        void removeNode(QtNode* node);
        
        //Link
        QtLink* addLink(QtTerminal *source, QtTerminal *dest, UILink* link);
        QtLink* addLink(UILink* uiLink);
        void addQtLink(QtLink *link);
        void removeLink(QtLink* link);  
        
        //Notes
        QtNote* addNote(UINote* uinote);
        void removeNote(QtNote *note);
        
        //Events
		
		//Node removed
		virtual void notifyNodeRemoved(const UINetwork *net, const UINode* node);
		
		//Node added
		virtual void notifyNodeAdded(const UINetwork *net, const UINode* node);
		
		//Link removed
		virtual void notifyLinkRemoved(const UINetwork *net, const UILink* link);
		
		//Link added
		virtual void notifyLinkAdded(const UINetwork *net, const UILink* link);

		//Note removed
		virtual void notifyNoteRemoved(const UINetwork *net, const UINote* note);
		
		//Note added
		virtual void notifyNoteAdded(const UINetwork *net, const UINote* note);
		
		//NetTerminal removed
		virtual void notifyNetTerminalRemoved(const UINetwork *net, const UINetTerminal* terminal);
		
		//NetTerminal added
		virtual void notifyNetTerminalAdded(const UINetwork *net, const UINetTerminal* terminal) ;
		
		//Name changed
		virtual void notifyNameChanged(const UINetwork *net, const std::string &name);
		
		//Description changed
		virtual void notifyDescriptionChanged(const UINetwork *net, const std::string &description);
		
		//Destroyed
		virtual void notifyDestroyed(const UINetwork *net);
        
		void resizeSceneView();
		
		void scaleView(qreal scaleFactor);
		
        protected: 
        
        void resizeView();
        void drawBackground(QPainter *painter, const QRectF &rect);
        virtual void mouseMoveEvent ( QMouseEvent * e );
        virtual void mousePressEvent (QMouseEvent * e);
        virtual void mouseReleaseEvent (QMouseEvent * e);
        void keyPressEvent(QKeyEvent *event);
        void wheelEvent(QWheelEvent *event);
        virtual void mouseDoubleClickEvent ( QMouseEvent * e );
        QContextMenuEvent *m_contextMenuEvent;
        QMenu* menu;

        // void mouseReleaseEvent(QMouseEvent *event);
        // void drawBackground(QPainter *painter, const QRectF &rect);
    
        QtDocument *m_doc;
        
        UINetwork *m_uiNetwork;
        
        std::map<UINode*, QtNode*> m_nodeMap;
        
        std::map<UILink*, QtLink*> m_linkMap;
        
        std::map<UINote*, QtNote*> m_noteMap;
        
        bool isNodeExist(const QString &name);
        
        //Drag & Drop
        void dragEnterEvent(QDragEnterEvent *event);
        void dragMoveEvent(QDragMoveEvent *event);
        void dropEvent(QDropEvent *event);        
        
        signals:
        void signalLinkProbed(int, const QString &);
        
        public slots:
        void linkProbed(int linkId, const QString &probeType) {emit signalLinkProbed(linkId, probeType);}
        void menuTriggered(QAction* action, const QPointF &pos);
        
        
		
		
        
    };
}//namespace FD
#endif
