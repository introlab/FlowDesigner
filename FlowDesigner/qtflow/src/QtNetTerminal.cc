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
                    setPos(-width -10,0);
                    break;
                                                           
                case UINetTerminal::OUTPUT :
					cerr<<"Creating TEXT FOR NET OUTPUT : "<<m_uiNetTerminal->getName().c_str()<<endl;
                    setDefaultTextColor( QColor(255,0,0,255));
                    setPos(10,0);
                    break;
                                                     
                case UINetTerminal::CONDITION :
                    setDefaultTextColor( QColor(0,0,0));
                    setPos(10,0);               
                    break;                              
            }
			
            //setFont(myfont);     
             
        }         

    }
    
    QtNetTerminal::~QtNetTerminal()
    {
    	if (m_uiNetTerminal)
    	{
    		delete m_uiNetTerminal;
    	}
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
