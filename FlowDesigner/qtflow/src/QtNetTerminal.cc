//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)
#include "QtNetTerminal.h"
#include "QtTerminal.h"
#include <QPainter>
#include <QStyleOption>
#include <QGraphicsScene>
#include <iostream>

namespace FD
{
	using namespace std;

    QtNetTerminal::QtNetTerminal(QtTerminal* parent, UINetTerminal *uiNetTerminal)
    : QGraphicsTextItem(parent), m_uiNetTerminal(uiNetTerminal)
    {

        if (m_uiNetTerminal)
        {		
            setPlainText(m_uiNetTerminal->getName().c_str());
            //QFont myfont(font());
                        
            QRectF boundaries = boundingRect();
            
            float width = boundaries.width();
			
			//cerr <<"width :  "<<boundaries.width()<<" height : "<<boundaries.height()<<endl;
            
			
			//QGraphicsRectItem *item = new QGraphicsRectItem(QRectF(0,0,500.0,500.0),parent);
			//item->setBrush(QBrush(QColor(255,0,0,128)));
			
			//cerr << "QtNetTerminal Scene pos x :"<<scenePos ().x() << " y:"<< scenePos().y() <<endl;
			
			
            switch(m_uiNetTerminal->getType())
            {
                case UINetTerminal::INPUT :
                    setDefaultTextColor( QColor(255,0,0,255));
                    setPos(-width -15,-5);
                    break;
                                                           
                case UINetTerminal::OUTPUT :
                    setDefaultTextColor( QColor(255,0,0,255));
                    setPos(15,-5);
                    break;
                                                     
                case UINetTerminal::CONDITION :
                    setDefaultTextColor( QColor(0,0,0));
                    setPos(15,-5);               
                    break;                              
            }
			
            //setFont(myfont);     
             
        }         

    }
    
    QtNetTerminal::~QtNetTerminal()
    {
        cerr << "QtNetTerminal::~QtNetTerminal()" << endl;
    	/*if (m_uiNetTerminal)
    	{
    		delete m_uiNetTerminal;
    	}*///Already deleted, see destructor of UITerminal
    }
    
	void QtNetTerminal::mousePressEvent ( QGraphicsSceneMouseEvent *event )
	{
		if ( event->button() == Qt::LeftButton)
		{
			if ( event->modifiers() == Qt::ShiftModifier )
			{
				event->accept();
				delete this;	
			}
		}
	}

}//namespace FD
