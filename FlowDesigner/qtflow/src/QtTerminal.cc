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
#include "UIProbeLink.h"

using namespace std;


namespace FD
{

QtTerminal::QtTerminal(QtNode *node, std::string name, int type, float x, float y)
    : QGraphicsRectItem(QRectF(0,0,5.0,5.0),node),m_node(node), m_type(type),
    m_virtualQtTerminal(NULL), m_virtualQtLink(NULL), m_linking(false)
{
    setPos(x,y);   
    m_label = new QGraphicsTextItem(name.c_str(),this);    
    QRectF rect = m_label->boundingRect();

    float offset_x = 0;
    float offset_y = -1 * rect.height() / 2.0 + 5.0 / 2.0;
    
    switch(m_type)
    {
        case INPUT:
            offset_x = -1 * rect.width();                              
            break;
                     
        case OUTPUT:
            offset_x = 5.0;                            
            break;                              
    }      
    
    m_label->setPos(offset_x, offset_y);
    setBrush(QBrush(QColor(255,0,0,128)));
}


QtTerminal::QtTerminal(QtNode *node, UITerminal *uiTerminal)
    : QGraphicsRectItem(QRectF(0,0,5.0,5.0),node), m_node(node),
      m_virtualQtTerminal(NULL), m_virtualQtLink(NULL), m_linking(false), m_uiTerminal(uiTerminal), m_netTerminal(NULL)
{
    if (m_uiTerminal)
    {
        //double posx, posy;
        //m_uiTerminal->getPos(posx,posy);              
        //cerr<<"Terminal name "<<m_uiTerminal->getName()<<" pos "<<posx<<","<<posy<<endl;              
        //setPos(posx,posy);                                            
        
        m_label = new QGraphicsTextItem(m_uiTerminal->getName().c_str(),this);
        QRectF rect = m_label->boundingRect();
        
        float offset_x = 0;
        float offset_y = 0;//-1 * rect.height() / 2.0 + 5.0 / 2.0;
        
        if (m_uiTerminal->isInputTerminal())
        {
            offset_x = 5;//-1 * rect.width();
            cerr<<"temrminal is input"<<endl;         
        }
        else
        {
            offset_x = -1 * rect.width();//5.0;
            cerr<<"terminal is output"<<endl;                                 
        }         
        m_label->setPos(offset_x, offset_y);
        setBrush(QBrush(QColor(255,0,0,128)));

        //HANDLE NET TERMINALS      
        if (m_uiTerminal->getNetTerminal())
        {
            m_netTerminal = new QtNetTerminal(this,m_uiTerminal->getNetTerminal());         
        }         
        
        
    }      
}   






void QtTerminal::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    update();
    if (event->button() == Qt::LeftButton)
    {
	
		if (event->modifiers() == Qt::NoModifier)
		{
		
	        QGraphicsItem::mousePressEvent(event);
	        
	        cerr<<"mousePressEvent on terminal "<<getName()<<" posx "<<pos().x()<<" posy "<<pos().y()<<endl;
	        //TODO CREATE LINK
	        m_virtualQtTerminal = new QtTerminal(NULL,"VIRTUAL",QtTerminal::VIRTUAL);
	        //m_virtualQtTerminal->hide();
	        m_virtualQtTerminal->setPos(event->scenePos());
	        m_virtualQtLink = new QtLink(this,m_virtualQtTerminal,NULL);
	        m_node->getQtNetwork()->scene()->addItem(m_virtualQtLink);
	        m_node->getQtNetwork()->scene()->addItem(m_virtualQtTerminal);
	        m_virtualQtLink->adjust();
	        m_linking = true;
	        event->accept();      
		}
		else if (event->modifiers() == Qt::ShiftModifier)
		{
			if (m_uiTerminal && m_uiTerminal->getNetTerminal() == NULL)
			{
		
				//CREATING A NET TERMINAL
				bool ok;
				QString name = QInputDialog::getText(NULL,QString("Network Terminal Name"),
										QString("Terminal Name : "),QLineEdit::Normal,
										QString(m_uiTerminal->getName().c_str()),&ok);

				//TODO :  LOOK FOR DUPLICATED NAMES							
				if (ok && !name.isEmpty())
				{
				
					//CREATE NET TERMINAL
					UINetTerminal *netTerminal = NULL;


					if (m_uiTerminal->isInputTerminal())
					{
						netTerminal = new UINetTerminal(m_uiTerminal,UINetTerminal::INPUT,name.toStdString());
					}
					else
					{
						netTerminal = new UINetTerminal(m_uiTerminal,UINetTerminal::OUTPUT,name.toStdString());
					}
					
					//CONNECT NET TERMINAL
					m_uiTerminal->connectNetTerminal(netTerminal);
				
					//CREATE GUI PART
					cerr << "QtTerminal Scene pos x :"<<scenePos ().x() << " y:"<< scenePos().y() <<endl;
					
					m_netTerminal = new QtNetTerminal(this,m_uiTerminal->getNetTerminal());
					scene()->addItem(m_netTerminal);
				}
										
				m_linking = false;
				event->accept();
			}
		}
    }

}

void QtTerminal::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
    update();
    if (event->button() == Qt::LeftButton)
    {
        QGraphicsItem::mousePressEvent(event);
        
        cerr<<"mouseReleaseEvent on terminal "<<getName()<<endl;
        QtTerminal* destinationQtTerminal = dynamic_cast<QtTerminal*>(m_node->getQtNetwork()->scene()->itemAt(event->scenePos()));
        if (destinationQtTerminal)
        {
            cerr<<"found terminal "<<destinationQtTerminal->getName()<<endl;
            //QtNode* node= destinationQtTerminal->getQtNode();
			
			//Creating probe link (from , to , points)
			UIProbeLink *pLink = new UIProbeLink(m_uiTerminal,destinationQtTerminal->getUITerminal(),NULL);
			
			
            QtLink* link = new QtLink(this,destinationQtTerminal,pLink);
            
           
			
			m_node->getQtNetwork()->addQtLink(link);
			
            
			//USEFUL WHEN UPDATING POSITION OF NODES
			m_node->addQtLink(link);
            destinationQtTerminal->getQtNode()->addQtLink(link);                                   
        }
                
        m_linking = false;
        delete m_virtualQtTerminal;
		m_virtualQtTerminal = NULL;
        delete m_virtualQtLink;
		m_virtualQtLink = NULL;
    }            
}

void QtTerminal::mouseMoveEvent(QGraphicsSceneMouseEvent * event)
{
    update();
    cerr<<"mouseMoveEvent on terminal "<<getName()<<endl;
    if (m_linking)
    {
        m_virtualQtTerminal->setPos(event->scenePos());
        m_virtualQtLink->adjust();
    }
    else
    {
        m_node->getQtNetwork()->ensureVisible(this);
        QGraphicsItem::mouseMoveEvent(event);
    }
}

std::string QtTerminal::getName()
{
    return m_label->toPlainText().toStdString();
}   

} //namespace FD

