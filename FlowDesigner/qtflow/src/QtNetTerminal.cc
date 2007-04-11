//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)
#include "QtNetTerminal.h"
#include "QtTerminal.h"
#include <QPainter>
#include <QStyleOption>
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
            
            switch(m_uiNetTerminal->getType())
            {
                case UINetTerminal::INPUT :
                    setDefaultTextColor( QColor(0,0,0));
                    setPos(-width -10,0);
                    break;
                                                           
                case UINetTerminal::OUTPUT :
					cerr<<"Creating TEXT FOR NET OUTPUT : "<<m_uiNetTerminal->getName().c_str()<<endl;
                    setDefaultTextColor( QColor(0,0,0));
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

}//namespace FD
