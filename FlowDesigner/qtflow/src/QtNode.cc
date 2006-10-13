/****************************************************************************
**
** Copyright (C) 2006-2006 Trolltech ASA. All rights reserved.
**
** This file is part of the example classes of the Qt Toolkit.
**
** Licensees holding valid Qt Preview licenses may use this file in
** accordance with the Qt Preview License Agreement provided with the
** Software.
**
** See http://www.trolltech.com/pricing.html or email sales@trolltech.com for
** information about Qt Commercial License Agreements.
**
** Contact info@trolltech.com if any conditions of this licensing are
** not clear to you.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

#include <QGraphicsScene>
#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include <QStyleOption>

#include "QtLink.h"
#include "QtNode.h"
#include "QtNetwork.h"
#include "QtTerminal.h"
#include <QRectF>
#include <iostream>
#include <algorithm>

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

void QtNode::addQtLink(QtLink *edge)
{
    edgeList << edge;
    edge->adjust();
}

void QtNode::removeQtLink(QtLink *edge)
{
   edgeList.removeAll(edge);
}

QList<QtLink *> QtNode::edges() const
{
    return edgeList;
}
/*
QRectF QtNode::boundingRect() const
{
    qreal adjust = 2;
    return QRectF(-10 - adjust, -10 - adjust,
                  43 + adjust, 23 + adjust);
}

QPainterPath QtNode::shape() const
{
    QPainterPath path;
    // path.addEllipse(-10, -10, 20, 20);
    path.addRect(-10, -10, 40, 20);
    return path;
}

void QtNode::paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *)
{
    // painter->setPen(Qt::NoPen);
    if (isSelected())
    {
       painter->setPen(QPen(Qt::blue));
    }
    else
    {
       painter->setPen(QPen(Qt::black));
    }
    painter->setBrush(Qt::darkGray);
    // painter->drawEllipse(-7, -7, 20, 20);
    painter->drawRect(-10,-10,40,20);

    // QRadialGradient gradient(-3, -3, 10);
    // if (option->state & QStyle::State_Sunken) {
        // gradient.setCenter(3, 3);
        // gradient.setFocalPoint(3, 3);
        // gradient.setColorAt(1, QColor(Qt::yellow).light(120));
        // gradient.setColorAt(0, QColor(Qt::darkYellow).light(120));
    // } else {
        // gradient.setColorAt(0, Qt::yellow);
        // gradient.setColorAt(1, Qt::darkYellow);
    // }
    // painter->setBrush(gradient);
    // painter->setPen(QPen(Qt::black, 0));
    // painter->drawEllipse(-10, -10, 20, 20);
}
*/
QVariant QtNode::itemChange(GraphicsItemChange change, const QVariant &value)
{
    switch (change) {
    case ItemPositionChange:
        foreach (QtLink *edge, edgeList)
            edge->adjust();
        break;
    default:
        break;
    };

    return QGraphicsItem::itemChange(change, value);
}

void QtNode::mousePressEvent(QGraphicsSceneMouseEvent *event)
{
    update();
    if (event->button() == Qt::LeftButton)
    {
       QGraphicsItem::mousePressEvent(event);
    }
    else if (event->button() == Qt::MidButton)
    {
       m_virtualQtNode = new QtNode(graph,"NEW");
       m_virtualQtNode->hide();
       graph->scene()->addItem(m_virtualQtNode);
       m_virtualQtNode->setPos(event->scenePos());
       //m_virtualQtLink = new QtLink(this, m_virtualQtNode);
       //TODO CREATE LINK
       graph->scene()->addItem(m_virtualQtLink);
       m_virtualQtLink->adjust();
       m_linking = true;
       event->accept();
    }
}

void QtNode::mouseMoveEvent(QGraphicsSceneMouseEvent * event)
{
   update();
   // if (m_virtualQtNode != NULL && m_virtualQtLink != NULL)
   if (m_linking)
   {
      m_virtualQtNode->setPos(event->scenePos());
      m_virtualQtLink->adjust();
   }
   else
   {

       graph->ensureVisible (this);
       
      QGraphicsItem::mouseMoveEvent(event);
   }
}

void QtNode::mouseReleaseEvent(QGraphicsSceneMouseEvent *event)
{
    update();
    if (event->button() == Qt::LeftButton)
    {
       QGraphicsItem::mouseReleaseEvent(event);
    }
    else if (event->button() == Qt::MidButton)
    {
       // if (this == graph->scene()->mouseGrabberItem())
       // {
          // graph->scene()->itemAt(event->buttonDownScenePos(event->button()))->mouseReleaseEvent(event);
       // }
       // else
          // std::cerr << "mid button released"  << std::endl;
          // QtNode* newQtNode = new QtNode(graph);
          // graph->scene()->addItem(newQtNode);
          // newQtNode->setPos(event->scenePos());
          // QtNode* destinationQtNode = dynamic_cast<QtNode*>(graph->scene()->itemAt(event->buttonDownScenePos(event->button())));
          QtNode* destinationQtNode = dynamic_cast<QtNode*>(graph->scene()->itemAt(event->scenePos()));
          if (destinationQtNode == m_virtualQtNode)
          {
             std::cerr << "aieouille!!" << std::endl;
          }
          // QtNode* previousQtNode = dynamic_cast<QtNode*>(graph->scene()->mouseGrabberItem());
          if (destinationQtNode != NULL && destinationQtNode != this)
          {
              //TODO CREATE LINK         
             //QtLink* createdQtLink = new QtLink(this, destinationQtNode);
             //graph->scene()->addItem(createdQtLink);
             //createdQtLink->adjust();
             // graph->update();
          }
          graph->scene()->removeItem(m_virtualQtLink);
          graph->scene()->removeItem(m_virtualQtNode);
          delete(m_virtualQtLink);
          m_virtualQtLink = NULL;
          delete(m_virtualQtNode);
          m_virtualQtNode = NULL;
          m_linking = false;
    }
}

QtTerminal* QtNode::addQtTerminal(std::string name, int type)
{
    //get boundaries
    QRectF boundaries = boundingRect();

    qreal x1,y1,x2,y2;
    boundaries.getCoords(&x1,&y1,&x2,&y2);
    QtTerminal *terminal = NULL;   
    switch (type)
    {
        case QtTerminal::INPUT:                     
            terminal = new QtTerminal(this,name,type, x1 - 2.5,y1 + 10 * (qreal) m_inputQtTerminals.size());
            m_inputQtTerminals.push_back(terminal);                 
            break;
        case QtTerminal::OUTPUT:
            terminal = new QtTerminal(this,name,type, x2 - 2.5,y1 + 10 * (qreal) m_outputQtTerminals.size());
            m_outputQtTerminals.push_back(terminal);                 
            break;               
    }   
    
    
    //boundaries = childrenBoundingRect().unite(boundingRect());

    setRect(QRectF(x1,y1,x2,y1 + 10 * (qreal) std::max( m_inputQtTerminals.size(), m_outputQtTerminals.size())));
        
    
    return terminal;
}   

