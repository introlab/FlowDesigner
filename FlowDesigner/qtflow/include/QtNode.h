//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#ifndef _QTNODE_H
#define _QTNODE_H

#include <QGraphicsItem>
#include <QList>
#include <QGraphicsTextItem>
#include <QGraphicsSceneMouseEvent>
#include <string>
#include <vector>

namespace FD
{

class QtLink;
class QtNetwork;
class QtTerminal;

class QtNode : public QGraphicsRectItem
{
public:
    QtNode(QtNetwork *graphWidget, std::string name = "");
    
    void addQtLink(QtLink *edge);
    void removeQtLink(QtLink *edge);
    QList<QtLink *> edges() const;
    
    enum { Type = UserType + 1 };
    int type() const { return Type; }
    
    //QRectF boundingRect() const;
    //QPainterPath shape() const;
    //void paint(QPainter *painter, const QStyleOptionGraphicsItem *option, QWidget *widget);
    QtTerminal* addQtTerminal(std::string name, int type);
    
    QtNetwork* getQtNetwork() { return graph;}   
    
protected:
    QVariant itemChange(GraphicsItemChange change, const QVariant &value);
    
    void mousePressEvent(QGraphicsSceneMouseEvent *event);
    void mouseMoveEvent(QGraphicsSceneMouseEvent *event);
    void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
    
private:
    QGraphicsTextItem *nameItem;
    QList<QtLink *> edgeList;
    QPointF newPos;
    QtNetwork *graph;
   QtNode* m_virtualQtNode;
   QtLink* m_virtualQtLink;
   bool m_linking;
   std::vector<QtTerminal*> m_inputQtTerminals;
   std::vector<QtTerminal*> m_outputQtTerminals;

};

}//namespace FD
#endif
