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
#ifndef _QTNODE_H
#define _QTNODE_H

#include <QGraphicsItem>
#include <QList>
#include <QGraphicsTextItem>
#include <QGraphicsSceneMouseEvent>
#include <QGraphicsItemGroup>
#include <QGraphicsRectItem>
#include <string>
#include <vector>
#include <map>

#include "UINode.h"

namespace FD
{
    
    class QtLink;
    class QtNetwork;
    class QtTerminal;
    class UITerminal;
    class UINode;
    
    class QtNode : public QObject, public QGraphicsRectItem, public UINode::UINodeObserverIF
    {
    	Q_OBJECT;
        
        public:
        
        //QtNode(QtNetwork *graphWidget, std::string name = "");
        
        QtNode(QtNetwork *graphWidget, UINode *uiNode);
        
        ~QtNode();
        		
		//Terminal removed
		virtual void notifyTerminalRemoved(const UINode *node, const UITerminal* terminal);
		
		//Terminal Added
		virtual void notifyTerminalAdded(const UINode *node, const UITerminal* terminal);
					
		//Parameters changed
		virtual void notifyParametersChanged(const UINode *node, const UINodeParameters *params);
		
		//Destroyed
		virtual void notifyDestroyed(const UINode *node);
		
		//Position changed
		virtual void notifyPositionChanged(const UINode* node, double x, double y);
        
        
        void addQtLink(QtLink *edge);
        void removeQtLink(QtLink *edge);
        QList<QtLink *> edges() const;
        
        enum { Type = UserType + 1 };
        int type() const { return Type; }
        
        
        QtTerminal* addTerminal(UITerminal *terminal);
        void removeTerminal(UITerminal* terminal);
        
        QtNetwork* getQtNetwork() { return graph;}   
        
        QtTerminal* getQtTerminal(UITerminal *term);
        
        UINode* getUINode(){return m_uiNode;}
        
        void positionChanged(float x, float y);
        
        void redrawNode();
        
        public slots:
        /**
         * Called when an icon is saved by the icon 
         * editor.
         * @author Mathieu Labbe
         * @param path the full path of the icon
         */
        void iconSaved(QString path);

        protected:
        
        
        QVariant itemChange(GraphicsItemChange change, const QVariant &value);
        void mousePressEvent(QGraphicsSceneMouseEvent *event);
        void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
        void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
        void mouseDoubleClickEvent (QGraphicsSceneMouseEvent * event);
        
        /**
         * Open a popup menu with some options like 
         * editing parameters or editing the icon of 
         * the node.
         * @author Mathieu Labbe
         * @param event the menu event
         */
        void contextMenuEvent(QGraphicsSceneContextMenuEvent *event);
        
        UINode *m_uiNode;
        QGraphicsTextItem *m_nameItem;
        QList<QtLink *> edgeList;
        QPointF newPos;
        QtNetwork *graph;
        QtNode* m_virtualQtNode;
        QtLink* m_virtualQtLink;
        QGraphicsItem *m_internalItem;
        bool m_linking;
        std::map<UITerminal*,QtTerminal*> m_inputTerminalsMap;
        std::map<UITerminal*,QtTerminal*> m_outputTerminalsMap;
        
        
    };
    
}//namespace FD
#endif
