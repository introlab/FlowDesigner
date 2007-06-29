//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)
#ifndef _QTNETTERMINAL_H_
#define _QTNETTERMINAL_H_

#include <QGraphicsTextItem>
#include "UINetTerminal.h"

namespace FD
{
    //forward declaration
    class QtTerminal;   
    
    class QtNetTerminal : public QGraphicsTextItem
    {
              
        public:
            
            QtNetTerminal(QtTerminal* parent, UINetTerminal *uiNetTerminal);
                                                                      
        protected:
        
        
            UINetTerminal *m_uiNetTerminal;
    };    



}//namespace FD
#endif
