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
#ifndef _QTLINK_H
#define _QTLINK_H

#include <QPen>
#include <QGraphicsItem>
#include <QGraphicsPathItem>
#include <string>
#include <QContextMenuEvent>


namespace FD 
{
    
    class QtNode;
    class QtTerminal;
    class UILink;
    
    class QtLink : public QObject, public QGraphicsLineItem
    {
        
    	Q_OBJECT;
    	
        public:
	        QtLink(QtTerminal *source, QtTerminal *dest, UILink* uiLink);
	        ~QtLink();
	        
	        QtTerminal *sourceQtTerminal() const {return m_source;}
	        void setSourceQtTerminal(QtTerminal *source) {m_source = source; adjust();}
	        
	        QtTerminal *destQtTerminal() const {return m_dest;}
	        void setDestQtTerminal(QtTerminal* dest) {m_dest = dest; adjust();}
	        
	        void adjust();
	        
	        UILink* getUILink() {return m_uiLink;}
	        
	        enum { Type = UserType + 2 };
	        int type() const { return Type; }
        
        signals:
        
        	void signalLinkProbed(int, const QString &);
        
        public slots:
        
	        void nodePositionChanged(float x, float y);
	        
	    protected:
	        virtual QRectF boundingRect() const;
	        virtual QPainterPath shape() const;
	        void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
	        QVariant itemChange(GraphicsItemChange change, const QVariant &value);
	        
	        virtual void hoverEnterEvent ( QGraphicsSceneHoverEvent * event );
	        virtual void hoverLeaveEvent ( QGraphicsSceneHoverEvent * event );
	        virtual void contextMenuEvent( QGraphicsSceneContextMenuEvent *event );
        
        private:
        
	        QPointF m_sourcePoint;
	        QPointF m_destPoint;      
	        QtTerminal *m_source;
	        QtTerminal *m_dest;
	        UILink *m_uiLink;
	        qreal arrowSize;
	        QPolygonF m_arrowHead;
    };
    
}//namespace FD

#endif
