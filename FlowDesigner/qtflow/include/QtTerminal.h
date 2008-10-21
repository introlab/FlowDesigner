/***********************************************************************************
** Copyright (C) 2006-2008 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca). 
** All rights reserved. 
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License
** as published by the Free Software Foundation; either version 2
** of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
***********************************************************************************/
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
        //void newLinkCreated(UITerminal* fro
		
		void removeNetTerminal();
		
        protected:
        
		//QT events
        virtual void mousePressEvent(QGraphicsSceneMouseEvent *event);
        virtual void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
        virtual void mouseMoveEvent(QGraphicsSceneMouseEvent * event);
        virtual void hoverEnterEvent ( QGraphicsSceneHoverEvent * event );
        virtual void focusInEvent ( QFocusEvent * event );
    
        void createIONetTerminal();
        void createCondNetTerminal();
        
        
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

