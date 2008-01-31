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
		setBrush ( QBrush ( QColor ( 255,0,0,128 ) ) );
	}
	
    
    
	QtTerminal::QtTerminal ( QtNode *node, UITerminal *uiTerminal )
    : QGraphicsRectItem ( QRectF ( 0,0,10.0,10.0 ),node ), m_node ( node ),
    m_virtualQtTerminal ( NULL ), m_virtualQtLink ( NULL ), m_linking ( false ), m_uiTerminal ( uiTerminal ), m_netTerminal ( NULL )
	{
		if ( m_uiTerminal )
		{
			//double posx, posy;
			//m_uiTerminal->getPos(posx,posy);
			//cerr<<"Terminal name "<<m_uiTerminal->getName()<<" pos "<<posx<<","<<posy<<endl;
			//setPos(posx,posy);
            
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
			setBrush ( QBrush ( QColor ( 255,0,0,128 ) ) );
            
			if (m_uiTerminal->getNetTerminal())
			{
				addNetTerminal(m_uiTerminal->getNetTerminal());
			}
			
			setToolTip( m_uiTerminal->getName().c_str());
            
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
            
            //TERMINAL NEAR BY?
            if ( destinationQtTerminal )
            {                
                QtTerminal *sourceQtTerminal = m_virtualQtLink->sourceQtTerminal();
                
				//MAKE SURE WI LINK AN OUTPOUT WITH AN INPUT
                if (sourceQtTerminal->getType() !=
                    destinationQtTerminal->getType())
                {
                    if (sourceQtTerminal->getType() == INPUT)
                    {
                    	
                    	UILink *link = m_uiTerminal->getNode()->getNetwork()->newLink(destinationQtTerminal->getUITerminal(),sourceQtTerminal->getUITerminal(),NULL);
                    	//m_uiTerminal->getNode()->getNetwork()->addLink(link);
                        //emit newLinkCreated(sourceQtTerminal->getUITerminal(),destinationQtTerminal->getUITerminal());
                        //m_uiTerminal->getNode()->getNetwork()->newLink(destinationQtTerminal->getUITerminal(),sourceQtTerminal->getUITerminal(),NULL);
                    }
                    else
                    {   
                    	UILink *link = m_uiTerminal->getNode()->getNetwork()->newLink(sourceQtTerminal->getUITerminal(),destinationQtTerminal->getUITerminal(),NULL);
                    	//m_uiTerminal->getNode()->getNetwork()->addLink(link);
                        //emit newLinkCreated(destinationQtTerminal->getUITerminal(),sourceQtTerminal->getUITerminal());
                        //m_uiTerminal->getNode()->getNetwork()->newLink(sourceQtTerminal->getUITerminal(),destinationQtTerminal->getUITerminal(),NULL);
                        
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
            m_virtualQtTerminal->setPos ( event->scenePos() );
            m_virtualQtLink->adjust();
        }
        else
        {
            m_node->getQtNetwork()->ensureVisible ( this );
            QGraphicsItem::mouseMoveEvent ( event );
        }
    }
    
    std::string QtTerminal::getName()
    {
        return m_uiTerminal->getName();
    }
    
    QtNetTerminal* QtTerminal::addNetTerminal ( UINetTerminal *netTerminal )
    {
        QtNetTerminal* myNetTerminal = new QtNetTerminal ( this,netTerminal );
        if (scene())
        {
        	scene()->addItem ( myNetTerminal );
        }
        
        return myNetTerminal;
    }
    
    
    QVariant QtTerminal::itemChange ( GraphicsItemChange change, const QVariant & value )
    {
        cerr<<"QtTerminal::itemChange ( GraphicsItemChange change, const QVariant & value )"<<endl;
        
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
    
} //namespace FD

