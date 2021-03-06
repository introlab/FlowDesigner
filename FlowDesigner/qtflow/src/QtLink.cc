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
#include <QPainter>
#include <QMenu>
#include "QtLink.h"
#include "QtNode.h"
#include "QtTerminal.h"
#include "QtProbeRegistry.h"
#include "UIProbeLink.h"
#include "UILink.h"
#include "UINetwork.h"
#include "UIDocument.h"
#include <cmath>
#include <iostream>


using namespace std;

namespace FD
{
    
    static const double Pi = 3.14159265358979323846264338327950288419717;
    static double TwoPi = 2.0 * Pi;
    
    QtLink::QtLink(QtTerminal * source, QtTerminal * dest, UILink* uiLink)
    : m_source(source), m_dest(dest), m_uiLink(uiLink), arrowSize(10)
    {
        //setAcceptedMouseButtons(0);
        setFlag(ItemIsSelectable);
        adjust();
        setAcceptsHoverEvents(true);
        setPen(QPen(Qt::black, 1, Qt::SolidLine, Qt::RoundCap, Qt::RoundJoin));
    }
    
    QtLink::~QtLink()
    {
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
        
        qreal extra = (pen().width() + arrowSize)/2;
        
        return QRectF(m_sourcePoint, QSizeF(m_destPoint.x() - m_sourcePoint.x(),
        m_destPoint.y() - m_sourcePoint.y()))
        .normalized()
        .adjusted(-extra, -extra, extra, extra);
    }
    
    QPainterPath QtLink::shape() const
    {
    	QPainterPath path = QGraphicsLineItem::shape();
     	path.addPolygon(m_arrowHead);
     	path.addEllipse(m_sourcePoint.x(), m_sourcePoint.y(), pen().width()*2, pen().width()*2);
     	return path;
    }
    
    void QtLink::paint(QPainter *painter, const QStyleOptionGraphicsItem *, QWidget *)
    {
        if (!m_source || !m_dest)
            return;

		//Set Line
		QLineF lineBuf(m_sourcePoint, m_destPoint);
		setLine(lineBuf);
		
		//Set painter pen and brush color
        painter->setPen(pen());
        painter->setBrush(pen().color());
        
        // Make an arrow head
        double angle = ::acos(line().dx() / line().length());
        if (line().dy() >= 0)
            angle = TwoPi - angle;
        
        QPointF destArrowP1 = line().p2() + QPointF(sin(angle - Pi / 3) * arrowSize,
        cos(angle - Pi / 3) * arrowSize);
        QPointF destArrowP2 = line().p2() + QPointF(sin(angle - Pi + Pi / 3) * arrowSize,
        cos(angle - Pi + Pi / 3) * arrowSize);
        
        m_arrowHead.clear();
        m_arrowHead << line().p2() << destArrowP1 << destArrowP2;

		// Draw the arrow
        painter->drawLine(line()); 
        painter->drawEllipse(line().p1(), pen().width()*2, pen().width()*2);
        painter->drawPolygon(m_arrowHead);
    }
    
    QVariant QtLink::itemChange(GraphicsItemChange change, const QVariant &value)
    {
        if(change == ItemSelectedChange && scene())
        {   
            if(value.toBool())
            {
                QPen aPen = pen();
    			aPen.setWidth(3);
	    		setPen(aPen);
            }               
            else
            {
                QPen aPen = pen();
    			aPen.setWidth(1);
	    		setPen(aPen);
            }
        }
        return QGraphicsItem::itemChange(change, value);
    }
    
    void QtLink::nodePositionChanged(float x, float y)
    {
    	adjust();	
    }
    
    void QtLink::hoverEnterEvent ( QGraphicsSceneHoverEvent * event ) 
    {
    	if(!isSelected())
    	{
	    	QPen aPen = pen();
    		aPen.setWidth(2);
	    	setPen(aPen);
	    	this->update();
    	}
    }
    
    void QtLink::hoverLeaveEvent ( QGraphicsSceneHoverEvent * event ) 
    {
    	if(!isSelected()) {
    		QPen aPen = pen();
    		aPen.setWidth(1);
	    	setPen(aPen);
	    	this->update();
    	}
    }
    
    void QtLink::contextMenuEvent( QGraphicsSceneContextMenuEvent *event )
    {
    	// QtFlow is running
    	if(m_uiLink && m_uiLink->getNetwork() && !m_uiLink->getNetwork()->getDocument()->isEditable()) {
	    	QMenu popupMenu(tr("Probing"));
	        QMenu* probeMenu = popupMenu.addMenu(tr("Probe it!"));
	        
	        QVector<QString> probeTypes = QtProbeRegistry::getAllowedProbe(m_uiLink->getFromTerminal()->getType().c_str());
	        
	        QVector<QAction*> actions(probeTypes.size());
	        for(int i=0; i<probeTypes.size(); i++) {
	        	actions[i] = probeMenu->addAction(probeTypes[i]);
	        }
	        
	        QAction* action = popupMenu.exec(QCursor::pos());
	        if(action) {
	        	for(int i=0; i<actions.size(); i++) {
	        		if(action == actions[i]) {
	        			// Signal that link is probed
	        			emit signalLinkProbed(m_uiLink->getId(), probeTypes[i]);
	        		}
		        }
        	}
	        
	        event->accept();
    	}
    }
    
    
}//namespace FD
