//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#ifndef _QTTERMINAL_H_
#define _QTTERMINAL_H_

#include <QGraphicsSceneMouseEvent>
#include <QGraphicsEllipseItem>
#include <QGraphicsTextItem>
#include <QGraphicsRectItem>
#include <string>
#include "UITerminal.h"


namespace FD
{
    
    class QtNode;
    class QtLink;
    class QtNetTerminal;
    
    
    class QtTerminal : public QObject, public QGraphicsRectItem
    {
        
        Q_OBJECT;
        
        public:
        
        
        enum {INPUT,OUTPUT,VIRTUAL};
        
	    QtTerminal(QtNode *node, std::string name="", int type = INPUT ,float x = 0, float y = 0);
	    
	    QtTerminal(QtNode *node, UITerminal *uiTerminal);
	    
	    virtual ~QtTerminal();
        
	    int getType(){return m_type;}
	    
	    void showTerminalInfo(bool visible);
	    
	    void setType(int type) {m_type = type;}
	    
	    QtNode* getQtNode(){return m_node;}
        
	    std::string getName();
        
	    UITerminal * getUITerminal() {return m_uiTerminal;}
        
        QtNetTerminal* addNetTerminal(UINetTerminal *netTerminal);
        
        //signals:
        
        //void positionChanged(float x, float y);
        //void newLinkCreated(UITerminal* from, UITerminal* to);
        
        protected:
        
		//QT events
        virtual void mousePressEvent(QGraphicsSceneMouseEvent *event);
        virtual void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
        virtual void mouseMoveEvent(QGraphicsSceneMouseEvent * event);
        virtual void hoverEnterEvent ( QGraphicsSceneHoverEvent * event );
        virtual void focusInEvent ( QFocusEvent * event );
    
        void createIONetTerminal();
        void createCondNetTerminal();
        void removeNetTerminal();
        
        //QGraphicsTextItem *m_label;
        QtNode *m_node;            
        int m_type;
        QtTerminal* m_virtualQtTerminal;
        QtLink* m_virtualQtLink;
        bool m_linking;
		
        UITerminal *m_uiTerminal;
        
        QtNetTerminal* m_qtNetTerminal;
        QGraphicsTextItem *m_infoItem;

    };   
    
    
}
#endif

