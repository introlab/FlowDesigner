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
            virtual ~QtNetTerminal();
      
                                                                      
        protected:
        
        	virtual void mousePressEvent ( QGraphicsSceneMouseEvent *event );
            UINetTerminal *m_uiNetTerminal;
    };    



}//namespace FD
#endif
