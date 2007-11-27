//Copyright (C) 2006-2007 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)
#include "QtLink.h"
#include "QtNode.h"
#include "QtNetwork.h"
#include "QtTerminal.h"
#include "QtNodeParameters.h"

#include <QRectF>
#include <QGraphicsScene>
#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include <QStyleOption>
#include <QtGui/QGraphicsView>

#include <iostream>
#include <algorithm>


#include "UINodeController.h"
#include "UITerminalController.h"

namespace FD
{
    using namespace std;
 /*   
    QtNode::QtNode(QtNetwork *graphWidget, std::string name)
    : QGraphicsRectItem(0,0,50,25),graph(graphWidget),
    m_linking(false)
    {
        
        nameItem = new QGraphicsTextItem(name.c_str(),this);
        setFlag(ItemIsMovable);
        setFlag(ItemIsSelectable);
        setBrush(QBrush(QColor(0,128,0,128)));
        setZValue(1);
    }
   */
    
    QtNode::QtNode(QtNetwork *graphWidget, UINode *uiNode)
    : QGraphicsRectItem(0,0,50,25), graph(graphWidget), m_uiNode(uiNode), m_linking(false)
    {
        if (m_uiNode)
        {
        	//register to events
        	m_uiNode->registerEvents(this);
        	
            double posx, posy;
            m_uiNode->getPos(posx,posy);
            setPos(posx,posy);
            
            //cerr<<"inserting node "<<m_uiNode->getName()<<" at position " << posx<<","<<posy<<endl;         
            
            nameItem = new QGraphicsTextItem(m_uiNode->getType().c_str(),this);
            
            QRectF boundaries = nameItem->boundingRect();
            
            qreal x1,y1,x2,y2;
            boundaries.getCoords(&x1,&y1,&x2,&y2);
            //boundaries.setCoords(x1-20,y1-20,x2+20,y2+20);         
            
            setRect(boundaries);
            
            
            
            setFlag(ItemIsMovable);
            setFlag(ItemIsSelectable);
            setBrush(QBrush(QColor(0,128,0,128)));
            setZValue(1);                                   
            
            //cerr<<"QtNode::QtNode(QtNetwork *graphWidget, UINode *uiNode)"<<endl;
            /*         
            
            std::vector<UITerminal *> inputs = m_uiNode->getInputs();
            //cerr<<"inputs size : "<<inputs.size()<<endl;
            
            for (unsigned int i = 0; i < inputs.size(); i++)
            {            
                //QtTerminal *term = new QtTerminal(this,inputs[i]);                             
                //m_inputQtTerminals.push_back(term);
                addQtTerminal(inputs[i]);
            }                  
            
            std::vector <UITerminal *> outputs = m_uiNode->getOutputs();
            //cerr<<"outputs size : "<<outputs.size()<<endl;
            for (unsigned int i =0; i< outputs.size(); i++)
            {
                //QtTerminal *term = new QtTerminal(this,outputs[i]);
                //m_outputQtTerminals.push_back(term);
                addQtTerminal(outputs[i]);
            }   
            */      
        } //if m_uiNode        
        
    }
    
    void QtNode::addQtLink(QtLink *edge)
    {
        edgeList << edge;
        edge->adjust();
    }
    
    void QtNode::removeQtLink(QtLink *edge)
    {
        cerr<<"removeQtLink(QtLink *edge)"<<endl;
        edgeList.removeAll(edge);
    }
    
    QList<QtLink *> QtNode::edges() const
    {
        return edgeList;
    }
    
    
    
    QVariant QtNode::itemChange(GraphicsItemChange change, const QVariant &value)
    {
        
        if (change == ItemPositionChange && scene()) 
        {
       		//value is the new position.
         	QPointF newPos = value.toPointF();
            
            //emit position changed signal
            cerr<<"(EMIT) positionChanged(newPos.x(),newPos.y())"<<endl;
            emit positionChanged(newPos.x(),newPos.y());
            
     	}
        
        if (change == ItemSelectedChange && scene()) 
        {
       		if(value.toBool())
                setBrush(QBrush(QColor(0,90,0,128))); 
            else
                setBrush(QBrush(QColor(0,128,0,128)));             
     	}
        
        return QGraphicsItem::itemChange(change, value);
    }
    
    void QtNode::mousePressEvent(QGraphicsSceneMouseEvent *event)
    {
        QGraphicsItem::mousePressEvent(event);
    }
    
	void QtNode::mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event)
	{
		update();
		if (event->button() == Qt::LeftButton)
		{
			if (m_uiNode)
			{
				QtNodeParameters params(m_uiNode);
                
				params.exec();
                
				event->accept();
			}
		}
        
	}
	
    void QtNode::mouseMoveEvent(QGraphicsSceneMouseEvent * event)
    {
        QGraphicsItem::mouseMoveEvent(event);
    }
    
    void QtNode::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
    {
        QGraphicsItem::mouseReleaseEvent(event);
    }
    
    QtTerminal* QtNode::addTerminal(UITerminal *uiTerminal)
    {
        QtTerminal *terminal = NULL;
        
        if (uiTerminal)
        {
            //get boundaries
            QRectF boundaries = nameItem->boundingRect();
            
            qreal x1,y1,x2,y2;
            boundaries.getCoords(&x1,&y1,&x2,&y2);
            
            qreal xx1,yy1,xx2,yy2;
            
            if (uiTerminal->isInputTerminal())
            {             
                terminal = new QtTerminal(this,uiTerminal);
                QRectF terminalBoundaries = terminal->childrenBoundingRect().unite(terminal->boundingRect());
                terminalBoundaries.getCoords(&xx1,&yy1, &xx2, &yy2);            
                scene()->addItem(terminal);                
                terminal->setPos(x1 - 2.5 - (xx2 - xx1),y1 + 10 * (qreal) m_inputTerminalsMap.size());
                m_inputTerminalsMap.insert(make_pair(uiTerminal,terminal));         
            }
            else
            {
                terminal = new QtTerminal(this,uiTerminal);
                scene()->addItem(terminal);
                QRectF terminalBoundaries = terminal->childrenBoundingRect().unite(terminal->boundingRect());
                terminalBoundaries.getCoords(&xx1,&yy1, &xx2, &yy2);                
                terminal->setPos(x2 + 2.5 + (xx2 - xx1),y1 + 10 * (qreal) m_outputTerminalsMap.size());
                m_outputTerminalsMap.insert(make_pair(uiTerminal,terminal));
            }
            
            //This will resize the node bounding rect
            boundaries = childrenBoundingRect().unite(boundingRect());
            boundaries.getCoords(&x1,&y1,&x2,&y2);
            setRect(boundaries);
            
            //Align inputs
            for (map<UITerminal*,QtTerminal*>::iterator iter = m_inputTerminalsMap.begin(); iter != m_inputTerminalsMap.end(); iter++)
            {
            	QPointF pos = (*iter).second->pos();
                pos.setX(x1);
                (*iter).second->setPos(pos);
            }
            
            //Align outputs
            for (map<UITerminal*,QtTerminal*>::iterator iter = m_outputTerminalsMap.begin(); iter != m_outputTerminalsMap.end(); iter++)
            {
            	QPointF pos = (*iter).second->pos();
            	QRectF terminalBoundaries = (*iter).second->childrenBoundingRect().unite((*iter).second->boundingRect());
            	terminalBoundaries.getCoords(&xx1,&yy1, &xx2, &yy2);
            	
            	//coordinate change (to Parent node)
            	QPointF lr = (*iter).second->mapToParent(xx2,yy2);
            	
                //Move by the offset 
                (*iter).second->moveBy(x2 - lr.x(),0);
            }
            
            
            
        }
        return terminal;
    }
    
    void QtNode::removeTerminal(QtTerminal* terminal)
    {
        scene()->removeItem(terminal);    
    }
    
	//Terminal removed
	void QtNode::notifyTerminalRemoved(const UINode *node, const UITerminal* terminal)
	{
		cerr<<"QtNode::notifyTerminalRemoved(const UINode *node, const UITerminal* terminal)"<<endl;
	}
	
	//Terminal Added
	void QtNode::notifyTerminalAdded(const UINode *node, const UITerminal* terminal)
	{
		cerr<<"QtNode::notifyTerminalAdded(const UINode *node, const UITerminal* terminal)"<<endl;
	}
				
	//Parameters changed
	void QtNode::notifyParametersChanged(const UINode *node, const UINodeParameters *params)
	{
		cerr<<"QtNode::notifyParametersChanged(const UINode *node, const UINodeParameters *params)"<<endl;
	}
	
	//Destroyed
	void QtNode::notifyDestroyed(const UINode *node)
	{
		cerr<<"QtNode::notifyDestroyed(const UINode *node)"<<endl;
	}
	
	//Position changed
	void QtNode::notifyPositionChanged(const UINode* node, double x, double y)
	{
		cerr<<"QtNode::notifyPositionChanged(const UINode* node, double x, double y)"<<endl;
		setPos(x,y);
	}
    

}//namespace FD
