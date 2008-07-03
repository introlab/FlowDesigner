//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#include <QPainter>
#include "QtLink.h"
#include "QtNode.h"
#include "QtTerminal.h"
#include "UIProbeLink.h"
#include "UILink.h"
#include <cmath>
#include <iostream>

using namespace std;

namespace FD
{
    
    static const double Pi = 3.14159265358979323846264338327950288419717;
    static double TwoPi = 2.0 * Pi;
    
    QtLink::QtLink(QtTerminal * source, QtTerminal * dest, UILink* uiLink)
    : m_source(source), m_dest(dest), arrowSize(10), m_uiLink(uiLink), m_penWidth(1)
    {
        //setAcceptedMouseButtons(0);
        setFlag(ItemIsSelectable);
        adjust();
    }
    
    QtLink::~QtLink()
    {
        cerr << "QtLink::~QtLink()" << endl;
    	if (m_source && m_source->getQtNode())
    		m_source->getQtNode()->removeQtLink(this);    
    	
    	if (m_dest && m_dest->getQtNode())
    		m_dest->getQtNode()->removeQtLink(this);
    }
    
    void QtLink::adjust()
    {
        if (!m_source || !m_dest)
            return;
        
        QPointF testPoint = m_source->mapFromParent (0,0);
        
        //cerr<<"Map from parent x "<<testPoint.x()<<" y "<<testPoint.y()<<endl;
        
        //QLineF line(mapFromItem(m_source, 0, 0), mapFromItem(m_dest, 0, 0));
        
        QPointF sourcePoint = m_source->scenePos();
        sourcePoint += QPointF(5.0,5.0);
        QPointF destPoint = m_dest->scenePos();
        destPoint += QPointF(5.0,5.0);
        QLineF line(sourcePoint, destPoint);
        
        //QLineF line(m_source->mapToScene(0,0), m_dest->mapToScene(0,0));
        removeFromIndex();
        m_sourcePoint = line.p1();
        m_destPoint = line.p2();
        addToIndex();
    }
    
    QRectF QtLink::boundingRect() const
    {
        if (!m_source || !m_dest)
            return QRectF();
        
        qreal penWidth = 1;
        qreal extra = (penWidth + arrowSize)/2;
        
        return QRectF(m_sourcePoint, QSizeF(m_destPoint.x() - m_sourcePoint.x(),
        m_destPoint.y() - m_sourcePoint.y()))
        .normalized()
        .adjusted(-extra, -extra, extra, extra);
    }
    
    void QtLink::paint(QPainter *painter, const QStyleOptionGraphicsItem *, QWidget *)
    {
        if (!m_source || !m_dest)
            return;
        QPen pen = QPen(Qt::black, m_penWidth, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin);

        // Draw the line itself
        QLineF line(m_sourcePoint, m_destPoint);
        painter->setPen(pen);
        painter->drawLine(line);
        
        // Draw the arrows if there's enough room
        double angle = ::acos(line.dx() / line.length());
        if (line.dy() >= 0)
            angle = TwoPi - angle;
        
        QPointF sourceArrowP1 = m_sourcePoint + QPointF(sin(angle + Pi / 3) * arrowSize,
        cos(angle + Pi / 3) * arrowSize);
        QPointF sourceArrowP2 = m_sourcePoint + QPointF(sin(angle + Pi - Pi / 3) * arrowSize,
        cos(angle + Pi - Pi / 3) * arrowSize);
        QPointF destArrowP1 = m_destPoint + QPointF(sin(angle - Pi / 3) * arrowSize,
        cos(angle - Pi / 3) * arrowSize);
        QPointF destArrowP2 = m_destPoint + QPointF(sin(angle - Pi + Pi / 3) * arrowSize,
        cos(angle - Pi + Pi / 3) * arrowSize);
        
        painter->setBrush(pen.color());
        painter->drawPolygon(QPolygonF() << line.p1() << sourceArrowP1 << sourceArrowP2);
        painter->drawPolygon(QPolygonF() << line.p2() << destArrowP1 << destArrowP2);
    }
    
    QVariant QtLink::itemChange(GraphicsItemChange change, const QVariant &value)
    {
        cerr<<"QVariant QtLink::itemChange(GraphicsItemChange change, const QVariant &value)"<<endl;
        if(change == ItemSelectedChange && scene())
        {   
            if(value.toBool())
            {
                m_penWidth = 3;
            }               
            else
            {
                m_penWidth = 1;
            }
        }
        return QGraphicsItem::itemChange(change, value);
    }
    
    void QtLink::nodePositionChanged(float x, float y)
    {
    	adjust();	
    }
    
    
}//namespace FD
