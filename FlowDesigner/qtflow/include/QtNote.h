/***********************************************************************************
** Copyright (C) 2006-2008 Laborius (http://www.gel.usherbrooke.ca/laborius/). 
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
#ifndef _QTNOTE_H_
#define _QTNOTE_H_

#include <QGraphicsRectItem>
#include <QGraphicsTextItem>
#include <QGraphicsSceneMouseEvent>
#include <string>
#include "UINote.h"

namespace FD
{

	class QtNetwork;

	class QtNote : public QGraphicsRectItem
	{
	protected:
		UINote *m_uiNote;
		QGraphicsTextItem *m_textItem;
		QtNetwork *m_qtNetwork;
		void mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event);
		QVariant itemChange(GraphicsItemChange change, const QVariant &value);
		void update();
		
	public:
		QtNote(QtNetwork *parent, UINote *note);
		
		UINote* getUINote(){return m_uiNote;}
	};



}//namespace FD

#endif
