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
#include "UINode.h"
#include "UITerminal.h"
#include <QGraphicsSvgItem>
#include <QGraphicsRectItem>

namespace FD
{
    using namespace std;

    
   QtNode::~QtNode()
   {
	   if (m_uiNode)
	   {
		   m_uiNode->unregisterEvents(this);
	   }
   }
    
    QtNode::QtNode(QtNetwork *graphWidget, UINode *uiNode)
    :  graph(graphWidget), m_uiNode(uiNode), m_linking(false)
    {
        if (m_uiNode)
        {
        	setHandlesChildEvents(false);
        	
        	//register to events
        	m_uiNode->registerEvents(this);
        	
            double posx, posy;
            m_uiNode->getPos(posx,posy);
            setPos(posx,posy);
            
                       
            //Svg icon loading
            NodeInfo *nInfo = UINodeRepository::Find(m_uiNode->getType());
            
            if (nInfo)
            {
            	cerr<<"Found icon : "<<nInfo->icon<<endl;
            	
            	if (nInfo->icon != "")
            	{
					QGraphicsSvgItem *svg = new QGraphicsSvgItem(QString(FD_ICONS_PATH) + QString("/") + QString(nInfo->icon.c_str()), this);
					
					
            	}
            	else
            	{
            		//Simple rectangle...
            		QGraphicsRectItem *rect = new QGraphicsRectItem(0,0,30,30,this);
            		
            		//TODO Change color and stuff...
            		rect->setBrush(QBrush(Qt::green));            		
            	                        			
            	}
            }
            
            //cerr<<"inserting node "<<m_uiNode->getName()<<" at position " << posx<<","<<posy<<endl;         
            
            nameItem = new QGraphicsTextItem(m_uiNode->getType().c_str(),this);
            
            QRectF boundaries = childrenBoundingRect();
            
            qreal x1,y1,x2,y2;
            boundaries.getCoords(&x1,&y1,&x2,&y2);
            boundaries.setCoords(x1-20,y1-20,x2+20,y2+20);         
            
			nameItem->setPos(x1,y2);
			
            setRect(boundaries);

            setFlag(ItemIsMovable);
            setFlag(ItemIsSelectable);
            //setBrush(QBrush(QColor(0,128,0,128)));
            setZValue(1);                                   
            
            //Add input terminal
            std::vector<UITerminal *> inputs = m_uiNode->getInputs();            
            for (unsigned int i = 0; i < inputs.size(); i++)
            {            
                addTerminal(inputs[i]);
            }                  
            
            //Add output terminal
            std::vector <UITerminal *> outputs = m_uiNode->getOutputs();
            for (unsigned int i =0; i< outputs.size(); i++)
            {
                addTerminal(outputs[i]);
            }
            
			
			
            
        } //if m_uiNode            
    }
    
    void QtNode::addQtLink(QtLink *edge)
    {
        edgeList << edge;
        edge->adjust();   
        //connect(this,SIGNAL(positionChanged(float,float)),edge,SLOT(nodePositionChanged(float,float)));
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
         	if (m_uiNode)
         	{	
         		m_uiNode->setPos(newPos.x(),newPos.y());
         		//emit positionChanged(newPos.x(),newPos.y());
         	}
     	}
        
        if (change == ItemSelectedChange && scene()) 
        {
		/*
       		if(value.toBool())
                setBrush(QBrush(QColor(0,90,0,128))); 
            else
                setBrush(QBrush(QColor(0,128,0,128)));             
				*/
     	}
        
        return QGraphicsItem::itemChange(change, value);
    }
    
    void QtNode::mousePressEvent(QGraphicsSceneMouseEvent *event)
    {
    	cerr<<"QtNode::mousePressEvent(QGraphicsSceneMouseEvent *event)"<<endl;
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
				
				//TODO Can we do that better?
				//get network for possible interface change
				UINetwork* net = m_uiNode->getNetwork();
				if (net)
					net->interfaceChangeNotify();
				
			}
		}
        
	}
	
    void QtNode::mouseMoveEvent(QGraphicsSceneMouseEvent * event)
    {
        QGraphicsItem::mouseMoveEvent(event);
		
		//graph->resizeSceneView();
		
    }
    
    void QtNode::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
    {
        QGraphicsItem::mouseReleaseEvent(event);
		
		graph->resizeSceneView();
		
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
                terminal->setPos(x1 - 2.5 - (xx2 - xx1),y1 + 10 * (qreal) m_inputTerminalsMap.size());
                m_inputTerminalsMap.insert(make_pair(uiTerminal,terminal));     
                
                if(scene())
                {
                	scene()->addItem(terminal);
                }
            }
            else
            {
                terminal = new QtTerminal(this,uiTerminal);
                QRectF terminalBoundaries = terminal->childrenBoundingRect().unite(terminal->boundingRect());
                terminalBoundaries.getCoords(&xx1,&yy1, &xx2, &yy2);                
                terminal->setPos(x2 + 2.5 + (xx2 - xx1),y1 + 10 * (qreal) m_outputTerminalsMap.size());
                m_outputTerminalsMap.insert(make_pair(uiTerminal,terminal));

                if(scene())
                {
                	scene()->addItem(terminal);
                }
            }
            
            //This will resize the node bounding rect
            boundaries = childrenBoundingRect().unite(boundingRect());
            boundaries.getCoords(&x1,&y1,&x2,&y2);
            //setRect(boundaries);
            
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
            
            boundaries = childrenBoundingRect().unite(boundingRect());
            boundaries.getCoords(&x1,&y1,&x2,&y2);
            setRect(boundaries);
            
        }
        return terminal;
    }
    
    /*
    void QtNode::removeTerminal(QtTerminal* terminal)
    {
        scene()->removeItem(terminal);    
    }
    */
    
    void QtNode::removeTerminal(UITerminal* terminal)
    {
    	
    	std::map<UITerminal*, QtTerminal*>::iterator iter;
    	
    	if ((iter = m_inputTerminalsMap.find(terminal)) != m_inputTerminalsMap.end())
    	{
    		delete iter->second;
    		m_inputTerminalsMap.erase(iter);
    		
    	}
    	else if ((iter = m_outputTerminalsMap.find(terminal)) != m_outputTerminalsMap.end())
    	{
    		delete iter->second;
    		m_outputTerminalsMap.erase(iter);
    	}
    	
    }
    
    
	//Terminal removed
	void QtNode::notifyTerminalRemoved(const UINode *node, const UITerminal* terminal)
	{
		cerr<<"QtNode::notifyTerminalRemoved(const UINode *node, const UITerminal* terminal)"<<endl;
		removeTerminal(const_cast<UITerminal*>(terminal));
		
		
	}
	
	//Terminal Added
	void QtNode::notifyTerminalAdded(const UINode *node, const UITerminal* terminal)
	{
		cerr<<"QtNode::notifyTerminalAdded(const UINode *node, const UITerminal* terminal)"<<endl;
		addTerminal(const_cast<UITerminal*>(terminal));
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
		m_uiNode = NULL;
	}
	
	//Position changed
	void QtNode::notifyPositionChanged(const UINode* node, double x, double y)
	{
		cerr<<"QtNode::notifyPositionChanged(const UINode* node, double x, double y)"<<endl;
		
		//Update Links
		for (QList<QtLink*>::iterator iter = edgeList.begin(); iter != edgeList.end(); iter++)
		{			
			(*iter)->adjust();
		}		
	}
	
    QtTerminal* QtNode::getQtTerminal(UITerminal *term)
    {
    	if(term)
    	{
    		if (term->isInputTerminal() )
    		{
		    	if (m_inputTerminalsMap.find(term) != m_inputTerminalsMap.end())
		    	{
		    		return m_inputTerminalsMap[term];
		    	}
    		}
    		else
    		{
    	    	if (m_outputTerminalsMap.find(term) != m_outputTerminalsMap.end())
		    	{
		    		return m_outputTerminalsMap[term];
		    	}
    		}
    	}
    	return NULL;
    }
    

    

}//namespace FD
