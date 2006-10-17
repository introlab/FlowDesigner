//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)
#include "QtNetTerminal.h"
#include "QtTerminal.h"
#include <QPainter>
#include <QStyleOption>


namespace FD
{

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
                    setDefaultTextColor( QColor(128,0,0,128));
                    setPos(-width -10,0);
                    break;
                                                           
                case UINetTerminal::OUTPUT :
                    setDefaultTextColor( QColor(0,128,0,128));
                    setPos(10,0);
                    break;
                                                     
                case UINetTerminal::CONDITION :
                    setDefaultTextColor( QColor(0,0,128,128));
                    setPos(10,0);               
                    break;                              
            }
                        
            //setFont(myfont);         
        }         

    }

}//namespace FD
