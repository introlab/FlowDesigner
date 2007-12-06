//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#ifndef _QTLINK_H
#define _QTLINK_H

#include <QPen>
#include <QGraphicsItem>
#include <QGraphicsPathItem>
#include <string>


namespace FD 
{
    
    class QtNode;
    class QtTerminal;
    class UILink;
    
    class QtLink : public QObject, public QGraphicsItem
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
        
        
        public slots:
        
        void nodePositionChanged(float x, float y);
        
        protected:
        QRectF boundingRect() const;
        void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
        QVariant itemChange(GraphicsItemChange change, const QVariant &value);
        
        private:
        
        QPointF m_sourcePoint;
        QPointF m_destPoint;      
        QtTerminal *m_source;
        QtTerminal *m_dest;
        UILink *m_uiLink;
        qreal arrowSize;
        
        qreal m_penWidth;
    };
    
}//namespace FD

#endif
