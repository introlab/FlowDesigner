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
#include "QtLink.h"
#include "QtNode.h"
#include "QtNetwork.h"
#include "QtTerminal.h"
#include "QtNodeParameters.h"
#include "iconeditor/QtIconEditor.h"
#include <QRectF>
#include <QGraphicsScene>
#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include <QStyleOption>
#include <QtGui/QGraphicsView>
#include <QDir>
#include <QFile>
#include <iostream>
#include <algorithm>
#include "UINode.h"
#include "UITerminal.h"
#include "UIDocument.h"
#include <QGraphicsSvgItem>
#include <QGraphicsRectItem>

namespace FD
{
    using namespace std;

    
   QtNode::~QtNode()
   {
	   //Remove edges
	   
	   
	   
	   if (m_uiNode)
	   {
		   m_uiNode->unregisterEvents(this);
	   }
   }
    
    QtNode::QtNode(QtNetwork *graphWidget, UINode *uiNode)
    :  m_uiNode(uiNode), graph(graphWidget), m_internalItem(NULL), m_linking(false)
    {
        if (m_uiNode)
        {
        	setHandlesChildEvents(false);
        	

        	
            double posx, posy;
            m_uiNode->getPos(posx,posy);
            setPos(posx,posy);
            
                       
            //Svg icon loading
            NodeInfo *nInfo = UINodeRepository::Find(m_uiNode->getType());
            // TODO set it in user preference
            QString userIcon = QDir::homePath() + QDir::separator() + QString(".flowdesigner") + QDir::separator() + "icons" + QDir::separator() + m_uiNode->getType().c_str() + ".svg";
            if (nInfo && nInfo->icon != "")
            {
            	//SVG ICON
        		//cerr<<"Found icon : "<<nInfo->icon<<endl;
        		m_internalItem = new QGraphicsSvgItem(QString(FD_ICONS_PATH) + QString("/") + QString(nInfo->icon.c_str()), this);
            }
            else if(QFile::exists(userIcon))
            {
            	m_internalItem = new QGraphicsSvgItem(userIcon, this);
            }
            else
            {
            	//Simple rectangle...
        		m_internalItem = new QGraphicsRectItem(0,0,30,30,this);
        		
        		//TODO Change color and stuff...
        		dynamic_cast<QGraphicsRectItem*>(m_internalItem)->setBrush(QBrush(Qt::green));
            }
            
            //cerr<<"inserting node "<<m_uiNode->getName()<<" at position " << posx<<","<<posy<<endl;         
            
            m_nameItem = new QGraphicsTextItem(
            		QString(m_uiNode->getName().c_str()) + QString("<br>[<b>") + QString(m_uiNode->getType().c_str()) + QString("</b>]"),this);
            

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
            
            //invisible pen
			QPen myPen = pen();
			myPen.setStyle(Qt::NoPen);
			setPen(myPen);
			
			//redraw node
            redrawNode();
            
        	//register to events
        	m_uiNode->registerEvents(this);
            
        } //if m_uiNode            
    }
    
    void QtNode::redrawNode()
    {
    	if (m_uiNode)
    	{	
	    	 
	    	 //update text
	    	 m_nameItem->setHtml(
	    	           		QString(m_uiNode->getName().c_str()) 
	    	           		+ QString("<br>[<b>") + QString(m_uiNode->getType().c_str()) + QString("</b>]"));
	    	            
	    	
	    	
	    	qreal name_width = m_nameItem->boundingRect().width();
	    	qreal internal_item_width = m_internalItem->boundingRect().width();
	    	
	    	qreal input_max_width = 0;
	    	//Getting max length for inputs
	    	for (map<UITerminal*,QtTerminal*>::iterator iter = m_inputTerminalsMap.begin(); iter != m_inputTerminalsMap.end(); iter++)
	    	{
	    		input_max_width = max(input_max_width,(iter->second->boundingRect().width()));
	    	}
	    	
	    	qreal output_max_width = 0;
	     	//Getting max length for outputs
	    	for (map<UITerminal*,QtTerminal*>::iterator iter = m_outputTerminalsMap.begin(); iter != m_outputTerminalsMap.end(); iter++)
	    	{
	    		output_max_width = max(output_max_width,(iter->second->boundingRect().width()));
	    	}
	    	
	      	//Redraw inputs
	    	//left aligned inputs
	    	qreal input_offset_x = 0;
	    	qreal input_offset_y = 0;	
	        for (map<UITerminal*,QtTerminal*>::iterator iter = m_inputTerminalsMap.begin(); iter != m_inputTerminalsMap.end(); iter++)
	        {
	        		qreal current_width = iter->second->boundingRect().width();
	        		input_offset_x = current_width - input_max_width;
	        		iter->second->setPos(input_offset_x,input_offset_y - (15.0 * m_inputTerminalsMap.size() - m_nameItem->boundingRect().height()) / 2.0);
	        		input_offset_y += 15.0;
	        }
	        
	        //Redraw outputs
	        //right aligned output
	        qreal output_offset_x = 0;
	        qreal output_offset_y = 0;
	    	for (map<UITerminal*,QtTerminal*>::iterator iter = m_outputTerminalsMap.begin(); iter != m_outputTerminalsMap.end(); iter++)
	    	{
	    		qreal current_width = iter->second->boundingRect().width();
	    		output_offset_x = internal_item_width + input_max_width + output_max_width - current_width;
	    		iter->second->setPos(output_offset_x,output_offset_y - (15.0 * m_outputTerminalsMap.size() - m_nameItem->boundingRect().height()) / 2.0);
	    		output_offset_y += 15.0;
	    	}
	    	
	    	//draw internal item
	    	m_internalItem->setPos(input_max_width,0);
	    	
	    	//draw text item
	    	m_nameItem->setPos((internal_item_width + input_max_width + output_max_width - name_width) / 2.0 ,max(m_internalItem->boundingRect().height(),max(input_offset_y ,output_offset_y)));
	        
	    	//update bounding box
	        QRectF boundaries = childrenBoundingRect();
	  
	        qreal x1,y1,x2,y2;
	        boundaries.getCoords(&x1,&y1,&x2,&y2);
	        boundaries.setCoords(x1-5,y1-5,x2+5,y2+5);
	        setRect(boundaries);
	    	
	    	//Adjust links
	        for (QList<QtLink*>::iterator iter = edgeList.begin(); iter != edgeList.end(); iter++)
	        {
	        	(*iter)->adjust();
	        }
    	}
    }
    
    void QtNode::addQtLink(QtLink *edge)
    {
        edgeList << edge;
        edge->adjust();   
        //connect(this,SIGNAL(positionChanged(float,float)),edge,SLOT(nodePositionChanged(float,float)));
    }
    
    void QtNode::removeQtLink(QtLink *edge)
    {
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
         	
         	graph->resizeSceneView();
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
            //QRectF boundaries = nameItem->boundingRect();
            
            //qreal x1,y1,x2,y2;
            //boundaries.getCoords(&x1,&y1,&x2,&y2);
            
            //qreal xx1,yy1,xx2,yy2;
            
            if (uiTerminal->isInputTerminal())
            {             
                terminal = new QtTerminal(this,uiTerminal);
                //QRectF terminalBoundaries = terminal->childrenBoundingRect().unite(terminal->boundingRect());
                //terminalBoundaries.getCoords(&xx1,&yy1, &xx2, &yy2);                            
                //terminal->setPos(x1 - 2.5 - (xx2 - xx1),y1 + 15 * (qreal) m_inputTerminalsMap.size());
                m_inputTerminalsMap.insert(make_pair(uiTerminal,terminal));     
                
                if(scene())
                {
                	scene()->addItem(terminal);
                }
            }
            else
            {
                terminal = new QtTerminal(this,uiTerminal);
                //QRectF terminalBoundaries = terminal->childrenBoundingRect().unite(terminal->boundingRect());
                //terminalBoundaries.getCoords(&xx1,&yy1, &xx2, &yy2);                
                //terminal->setPos(x2 + 2.5 + (xx2 - xx1),y1 + 15 * (qreal) m_outputTerminalsMap.size());
                m_outputTerminalsMap.insert(make_pair(uiTerminal,terminal));

                if(scene())
                {
                	scene()->addItem(terminal);
                }
            }
            
            //This will resize the node bounding rect
            //boundaries = childrenBoundingRect().unite(boundingRect());
            //boundaries.getCoords(&x1,&y1,&x2,&y2);
            //setRect(boundaries);
            
            //Align inputs
            /*
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
            */
        }
        redrawNode();
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
			m_inputTerminalsMap.erase(iter);
    		delete iter->second;
    	}
    	else if ((iter = m_outputTerminalsMap.find(terminal)) != m_outputTerminalsMap.end())
    	{
			m_outputTerminalsMap.erase(iter);
    		delete iter->second;
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
    
    void QtNode::contextMenuEvent( QGraphicsSceneContextMenuEvent *event )
    {
    	if(m_uiNode && m_uiNode->getNetwork()->getDocument()->isEditable()) {
	    	QMenu popupMenu(tr("Node edit"));
	    	QAction* editProperties = popupMenu.addAction(QString(tr("Edit properties")));
	    	QAction* editIcon = popupMenu.addAction(QString(tr("Edit icon")));
	        
	        QAction* action = popupMenu.exec(QCursor::pos());
	        if(action) {
        		if(action == editProperties) {
        			if (m_uiNode) {
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
        		else if(action == editIcon) {
        			QString iconName(m_uiNode->getType().c_str());
        			//Create svgeditor
        			QtIconEditor* iconEditor = new QtIconEditor(graph, iconName);
        			connect(iconEditor, SIGNAL(iconSaved(QString)), this, SLOT(iconSaved(QString)));
        			iconEditor->show();
        		}
        	}
	        
	        event->accept();
    	}
    }
    
    void QtNode::iconSaved(QString path)
    {
    	if(m_internalItem) {
    		delete m_internalItem;
    	}
    	if(path.isEmpty()) {
    		m_internalItem = new QGraphicsRectItem(0,0,30,30,this);
        	dynamic_cast<QGraphicsRectItem*>(m_internalItem)->setBrush(QBrush(Qt::green));
    	}
    	else {
    		m_internalItem = new QGraphicsSvgItem(path, this);
    	}
    	m_internalItem->update();
    	this->redrawNode();
    }
}//namespace FD
