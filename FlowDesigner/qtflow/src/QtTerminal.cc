//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)

#include "QtNetwork.h"
#include "QtTerminal.h"
#include "QtNode.h"
#include "QtLink.h"
#include "QtNetTerminal.h"
#include <QGraphicsScene>
#include <QBrush>
#include <QInputDialog>
#include <iostream>
#include <string>

#include "UINetTerminal.h"
#include "UINetwork.h"
#include "UIProbeLink.h"

using namespace std;


namespace FD
{
    

	QtTerminal::QtTerminal ( QtNode *node, std::string name, int type, float x, float y )
    : QGraphicsRectItem ( QRectF ( 0,0,10.0,10.0 ),node ),m_node ( node ), m_type ( type ),
    m_virtualQtTerminal ( NULL ), m_virtualQtLink ( NULL ), m_linking ( false )
	{
		setPos ( x,y );
		
		/*
		m_label = new QGraphicsTextItem ( name.c_str(),this );
		QRectF rect = m_label->boundingRect();
        
		float offset_x = 0;
		float offset_y = -3;
        
		switch ( m_type )
		{
			case INPUT:
            offset_x = -1 * rect.width();
            break;
            
			case OUTPUT:
            offset_x = 10.0;
            break;
		}
        
		//m_label->setPos ( offset_x, offset_y );
		*/
		setBrush ( QBrush ( QColor ( 255,0,0,128 ) ) );
		
		m_infoItem = new QGraphicsTextItem("VIRTUAL", NULL);
		m_infoItem->moveBy(15,0);
		m_infoItem->hide();
	}
	
    
    
	QtTerminal::QtTerminal ( QtNode *node, UITerminal *uiTerminal )
    : QGraphicsRectItem ( QRectF ( 0,0,10.0,10.0 ),node ), m_node ( node ),
    m_virtualQtTerminal ( NULL ), m_virtualQtLink ( NULL ), m_linking ( false ), m_uiTerminal ( uiTerminal ), m_netTerminal ( NULL )
	{
		if ( m_uiTerminal )
		{
			double posx, posy;
			m_uiTerminal->getPos(posx,posy);
			cerr<<"Terminal name "<<m_uiTerminal->getName()<<" pos "<<posx<<","<<posy<<" ptr:"<<this<<endl;
			//setPos(posx,posy);
            /*
			m_label = new QGraphicsTextItem ( m_uiTerminal->getName().c_str(),this );
			
			
			QRectF rect = m_label->boundingRect();
            
			float offset_x = 0;
			float offset_y = -3;
            
			if ( m_uiTerminal->isInputTerminal() )
			{
				offset_x = 10;
                m_type = INPUT;
			}
			else
			{
				offset_x = -1 * rect.width();
                m_type = OUTPUT;
			}
			m_label->setPos ( offset_x, offset_y );
			*/
			
			setBrush ( QBrush ( QColor ( 255,0,0,128 ) ) );
            
			if (m_uiTerminal->getNetTerminal())
			{
				addNetTerminal(m_uiTerminal->getNetTerminal());
			}
			
			setToolTip( m_uiTerminal->getName().c_str());
			
			
			m_infoItem = new QGraphicsTextItem(QString(m_uiTerminal->getName().c_str()) + QString("\ntype=") + QString(m_uiTerminal->getType().c_str()) , NULL);
			m_infoItem->hide();
			
			
            
		}
	}
	
	QtTerminal::~QtTerminal()
	{
	
		
	}
    
	void QtTerminal::mousePressEvent ( QGraphicsSceneMouseEvent *event )
	{
		QGraphicsItem::mousePressEvent ( event );
		update();
		if ( event->button() == Qt::LeftButton )
		{
            
			if ( event->modifiers() == Qt::NoModifier )
			{
				cerr<<"mousePressEvent on terminal "<<getName() <<" posx "<<pos().x() <<" posy "<<pos().y() <<endl;
				//TODO CREATE LINK
				m_virtualQtTerminal = new QtTerminal ( NULL,"VIRTUAL",QtTerminal::VIRTUAL );
				m_virtualQtTerminal->hide();
				m_virtualQtTerminal->setPos ( event->scenePos() );
				m_virtualQtLink = new QtLink ( this,m_virtualQtTerminal,NULL );
				scene()->addItem ( m_virtualQtLink );
				scene()->addItem ( m_virtualQtTerminal );
				m_virtualQtLink->adjust();
				m_linking = true;
				event->accept();
			}
			else if ( event->modifiers() == Qt::ShiftModifier )
			{
				if ( m_uiTerminal && m_uiTerminal->getNetTerminal() == NULL )
				{
                    
					//CREATING A NET TERMINAL
					bool ok;
					QString name = QInputDialog::getText ( NULL,QString ( "Network Terminal Name" ),
                    QString ( "Terminal Name : " ),QLineEdit::Normal,
                    QString ( m_uiTerminal->getName().c_str() ),&ok );
                    
					//TODO :  LOOK FOR DUPLICATED NAMES
					if ( ok && !name.isEmpty() )
					{  
						if ( m_uiTerminal->isInputTerminal() )
						{
							UINetTerminal *term = new UINetTerminal( m_uiTerminal,UINetTerminal::INPUT,name.toStdString() );
							addNetTerminal(term);
						}
						else
						{
							UINetTerminal *term = new UINetTerminal( m_uiTerminal,UINetTerminal::OUTPUT,name.toStdString() );
							addNetTerminal(term);
						}									                       
					}
                    
					m_linking = false;
					event->accept();
				}
			}
            else if ( event->modifiers() == Qt::ControlModifier )
			{
                if ( m_uiTerminal && m_uiTerminal->getNetTerminal() == NULL && !m_uiTerminal->isInputTerminal())
				{              
                	UINetTerminal *term = new UINetTerminal( m_uiTerminal,UINetTerminal::CONDITION,"CONDITION");  
                	addNetTerminal(term);
                }
 
                m_linking = false;
                event->accept();
            }
        }
    }
    
    void QtTerminal::mouseReleaseEvent ( QGraphicsSceneMouseEvent *event )
    {
	
		QGraphicsItem::mouseReleaseEvent ( event );
		
        update();
        if ( event->button() == Qt::LeftButton && m_linking)
        {
            
            
            cerr<<"mouseReleaseEvent on terminal (linking)"<<getName() <<endl;
            QtTerminal* destinationQtTerminal = dynamic_cast<QtTerminal*> ( scene()->itemAt ( event->scenePos() ) );
            
            //Go back to old brush for all terminals
           	QList<QGraphicsItem *> allItems = scene()->items();
                	
        	for(QList<QGraphicsItem*>::iterator iter = allItems.begin(); iter != allItems.end(); iter++)
        	{
        		QtTerminal *myTerminal = dynamic_cast<QtTerminal*>(*iter);
        		if (myTerminal && myTerminal != this)
        		{
        			myTerminal->setBrush(this->brush());
        			myTerminal->showTerminalInfo(false);
        		}	
        	}
            
            //TERMINAL NEAR BY?
            if ( destinationQtTerminal && destinationQtTerminal != this)
            {                            	
            	if (m_uiTerminal->getNode() != destinationQtTerminal->getUITerminal()->getNode())
            	{
            		//Look for already existing links
            		//TODO : This should be done in UINetwork
            		UINetwork *myNetwork = m_uiTerminal->getNode()->getNetwork();
            		std::vector<UILink *> allLinks = myNetwork->getLinks(); 
            		
            		bool found = false;
            		
            		for (unsigned int i = 0; i<allLinks.size(); i++ )
            		{
            			if(allLinks[i]->getFromTerminal() == m_uiTerminal)
            			{
            				if (allLinks[i]->getToTerminal() == destinationQtTerminal->getUITerminal())
            					found = true;
            			}
            			else if (allLinks[i]->getToTerminal() == m_uiTerminal)
            			{
            				if (allLinks[i]->getFromTerminal() == destinationQtTerminal->getUITerminal())
            					found = true;
            			}		
            		}
            		
            		if (!found)
            		{
		                if (m_uiTerminal->isInputTerminal() && !destinationQtTerminal->getUITerminal()->isInputTerminal())
		                {
		                	UILink *link = m_uiTerminal->getNode()->getNetwork()->newLink(destinationQtTerminal->getUITerminal(),m_uiTerminal,NULL);
		                }
		                else if (!m_uiTerminal->isInputTerminal() && destinationQtTerminal->getUITerminal()->isInputTerminal())
		                {   
		                	UILink *link = m_uiTerminal->getNode()->getNetwork()->newLink(m_uiTerminal,destinationQtTerminal->getUITerminal(),NULL);
		                }
            		}
            	}    

            }
            
            m_linking = false;
            delete m_virtualQtLink;
            m_virtualQtLink = NULL;
            delete m_virtualQtTerminal;
            m_virtualQtTerminal = NULL;
        }
		else
		{
			cerr<<"mouseReleaseEvent on terminal (NOT linking)"<<getName() <<endl;
			m_linking = false;
		}
    }
    
    void QtTerminal::mouseMoveEvent ( QGraphicsSceneMouseEvent * event )
    {
        update();
        cerr<<"mouseMoveEvent on terminal "<<getName() <<endl;
        if ( m_linking )
        {
            m_virtualQtTerminal->setPos ( event->scenePos() + QPointF(-5.0,-5.0));
            m_virtualQtLink->adjust();
            
            QtTerminal* nearTerminal = dynamic_cast<QtTerminal*> ( scene()->itemAt ( event->scenePos() ) );
            
            if (nearTerminal && nearTerminal != this)
            {
            	QBrush brush = nearTerminal->brush();
       
            	brush.setColor(Qt::green);
            	nearTerminal->setBrush(brush);
            	
            	nearTerminal->showTerminalInfo(true);
            	
            }
            
          	//Get all other items
            //Put them back to normal color
        	QList<QGraphicsItem *> allItems = scene()->items();
        	
        	for(QList<QGraphicsItem*>::iterator iter = allItems.begin(); iter != allItems.end(); iter++)
        	{
        		
        		QtTerminal *myTerminal = dynamic_cast<QtTerminal*>(*iter);
        		if (myTerminal && myTerminal != nearTerminal)
        		{
        			myTerminal->setBrush(this->brush());
        			myTerminal->showTerminalInfo(false);
        		}
        		
        		
        		
        	}
            
            
            event->accept();
        }
        else
        {
            m_node->getQtNetwork()->ensureVisible ( this );
            QGraphicsItem::mouseMoveEvent ( event );
        }
    }
    
    void QtTerminal::showTerminalInfo(bool visible)
    {
    	if (!m_infoItem->scene())
    	{
    		if (scene())
    			scene()->addItem(m_infoItem);
    	}
    		
    	if (visible)  	
    	{
    		
    		m_infoItem->show();	
    		m_infoItem->setPos(scenePos());
			if (m_uiTerminal->isInputTerminal())
			{
				m_infoItem->moveBy(-15 -m_infoItem->boundingRect().width(),5);
			}
			else
			{
				m_infoItem->moveBy(15,5);
			}
    	}
    	else
    	{
    		m_infoItem->hide();
    	}
    }
    
    std::string QtTerminal::getName()
    {
        return m_uiTerminal->getName();
    }
    
    QtNetTerminal* QtTerminal::addNetTerminal ( UINetTerminal *netTerminal )
    {
        QtNetTerminal* myNetTerminal = new QtNetTerminal ( this,netTerminal );
    
        
        return myNetTerminal;
    }
    
    void QtTerminal::hoverEnterEvent ( QGraphicsSceneHoverEvent * event )
    {
    	cerr<<" void QtTerminal::hoverEnterEvent ( QGraphicsSceneHoverEvent * event )"<<endl;
    	QGraphicsItem::hoverEnterEvent(event);
    }
    
    void QtTerminal::focusInEvent ( QFocusEvent * event )
    {
    	cerr<<"void QtTerminal::focusInEvent ( QFocusEvent * event )"<<endl;
    }
    
  /*  
    QVariant QtTerminal::itemChange ( GraphicsItemChange change, const QVariant & value )
    {
        cerr<<"QtTerminal::itemChange ( GraphicsItemChange change, const QVariant & value ) ptr:"<<this<<endl;
        if (m_uiTerminal)
        	cerr<<"Terminal Name : "<<m_uiTerminal->getName()<<endl;
        
        if ( change == ItemPositionChange && scene() )
        {
            // value is the new position.
            QPointF newPos = value.toPointF();
            
            emit positionChanged ( newPos.x(), newPos.y() );
            
            //QRectF rect = scene()->sceneRect();
            //if (!rect.contains(newPos)) {
                // Keep the item inside the scene rect.
                //    newPos.setX(qMin(rect.right(), qMax(newPos.x(), rect.left())));
                //    newPos.setY(qMin(rect.bottom(), qMax(newPos.y(), rect.top())));
                //    return newPos;
            //}
        }
        
        return QGraphicsItem::itemChange ( change, value );
    }
    */
    
} //namespace FD

