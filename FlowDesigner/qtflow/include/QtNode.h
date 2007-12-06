//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#ifndef _QTNODE_H
#define _QTNODE_H

#include <QGraphicsItem>
#include <QList>
#include <QGraphicsTextItem>
#include <QGraphicsSceneMouseEvent>
#include <QGraphicsSvgItem>
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
    
    class QtNode : public QGraphicsSvgItem, public UINode::UINodeObserverIF
    {
        
        public:
        
        //QtNode(QtNetwork *graphWidget, std::string name = "");
        
        QtNode(QtNetwork *graphWidget, UINode *uiNode);
        		
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
        
 
        
        void positionChanged(float x, float y);

        protected:
        
        QVariant itemChange(GraphicsItemChange change, const QVariant &value);
        void mousePressEvent(QGraphicsSceneMouseEvent *event);
        void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
        void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
        void mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event);
        
        UINode *m_uiNode;
        QGraphicsTextItem *nameItem;
        QList<QtLink *> edgeList;
        QPointF newPos;
        QtNetwork *graph;
        QtNode* m_virtualQtNode;
        QtLink* m_virtualQtLink;
        bool m_linking;
        //std::vector<QtTerminal*> m_inputQtTerminals;
        //std::vector<QtTerminal*> m_outputQtTerminals;
        std::map<UITerminal*,QtTerminal*> m_inputTerminalsMap;
        std::map<UITerminal*,QtTerminal*> m_outputTerminalsMap;
        
        
    };
    
}//namespace FD
#endif
