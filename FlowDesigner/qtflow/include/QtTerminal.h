#ifndef _QTTERMINAL_H_
#define _QTTERMINAL_H_

#include <QGraphicsSceneMouseEvent>
#include <QGraphicsEllipseItem>
#include <QGraphicsTextItem>
#include <QGraphicsRectItem>
#include <string>

class QtNode;
class QtLink;

class QtTerminal : public QGraphicsRectItem
{
    public:
           
        enum {INPUT,OUTPUT,VIRTUAL};
              
    QtTerminal(QtNode *node, std::string name="", int type = INPUT ,float x = 0, float y = 0);

    int getType(){return m_type;}
    
    void setType(int type) {m_type = type;}
    
    QtNode* getQtNode(){return m_node;}
      
    std::string getName();

    
    protected:
        virtual void mousePressEvent(QGraphicsSceneMouseEvent *event);
        virtual void mouseReleaseEvent(QGraphicsSceneMouseEvent *event);
        virtual void mouseMoveEvent(QGraphicsSceneMouseEvent * event);
        
        QGraphicsTextItem *m_label;
        QtNode *m_node;            
        int m_type;
        QtTerminal* m_virtualQtTerminal;
        QtLink* m_virtualQtLink;
        bool m_linking;

};   



#endif

