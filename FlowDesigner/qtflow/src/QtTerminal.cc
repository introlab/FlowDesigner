#include "QtNetwork.h"
#include "QtTerminal.h"
#include "QtNode.h"
#include "QtLink.h"
#include <QGraphicsScene>
#include <QBrush>
#include <iostream>
#include <string>

using namespace std;

QtTerminal::QtTerminal(QtNode *node, std::string name, int type, float x, float y)
    : QGraphicsRectItem(QRectF(x,y,5.0,5.0),node),m_node(node), m_type(type),
    m_virtualQtTerminal(NULL), m_virtualQtLink(NULL), m_linking(false)
{
       
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
    
    m_label->setPos(x + offset_x, y+ offset_y);
    setBrush(QBrush(QColor(255,0,0,128)));
}


void QtTerminal::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    update();
    if (event->button() == Qt::LeftButton)
    {
        QGraphicsItem::mousePressEvent(event);
        
        cerr<<"mousePressEvent on terminal "<<getName()<<" posx "<<pos().x()<<" posy "<<pos().y()<<endl;
        //TODO CREATE LINK
        m_virtualQtTerminal = new QtTerminal(NULL,"VIRTUAL",QtTerminal::VIRTUAL);
        //m_virtualQtTerminal->hide();
        m_virtualQtTerminal->setPos(event->scenePos());
        m_virtualQtLink = new QtLink(this,m_virtualQtTerminal);
        m_node->getQtNetwork()->scene()->addItem(m_virtualQtLink);
        m_node->getQtNetwork()->scene()->addItem(m_virtualQtTerminal);
        m_virtualQtLink->adjust();
        m_linking = true;
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
            QtLink* link = new QtLink(this,destinationQtTerminal);
            link->adjust();
            m_node->getQtNetwork()->scene()->addItem(link);
            m_node->addQtLink(link);
            destinationQtTerminal->getQtNode()->addQtLink(link);                                   
        }
                
        m_linking = false;
        delete m_virtualQtTerminal;
        delete m_virtualQtLink;
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

