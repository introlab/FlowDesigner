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
