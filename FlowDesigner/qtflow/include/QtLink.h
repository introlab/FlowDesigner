//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#ifndef _QTLINK_H
#define _QTLINK_H

#include <QGraphicsItem>
#include <string>


namespace FD 
{

class QtNode;
class QtTerminal;
class UIProbeLink;

class QtLink : public QGraphicsItem
{
    public:
        QtLink(QtTerminal *source, QtTerminal *dest);
        ~QtLink();

        QtTerminal *sourceQtTerminal() const {return m_source;}
        void setSourceQtTerminal(QtTerminal *source) {m_source = source; adjust();}

        QtTerminal *destQtTerminal() const {return m_dest;}
        void setDestQtTerminal(QtTerminal* dest) {m_dest = dest; adjust();}

        void adjust();

    
    protected:
        QRectF boundingRect() const;
        void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
    
    private:
	
		UIProbeLink *m_probeLink;
        QPointF m_sourcePoint;
        QPointF m_destPoint;      
        QtTerminal *m_source;
        QtTerminal *m_dest;
        qreal arrowSize;
};

}//namespace FD

#endif
