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
#ifndef QTNETWORKSCENE_H_
#define QTNETWORKSCENE_H_

#include "QtNetwork.h"
#include <QGraphicsScene>
namespace FD
{
/**
 * The scene of a QtNetwork.
 * TODO: This class was firstly created to handle contextMenuEvent 
 *       with QGraphicsSceneContextMenuEvent. It is mush easier to 
 *       pass contextMenuEvent to QGraphicsItem before to do a default 
 *       behavior.
 *       To do a clean job, most of all features contained
 *       in QtNetwork must be transfered here. Refer to the 
 *       Qt documentation and the examples about QGraphicsView 
 *       and QGraphicsScene. 
 * @author Mathieu Labbe
 * @See QtNetwork
 */
class QtNetworkScene : public QGraphicsScene
{
	Q_OBJECT;
	
	public:
		/**
		 * The constructor.
		 * @param parent the QtNetwork view
		 */
		QtNetworkScene(QtNetwork *parent) : QGraphicsScene(parent), m_networkView(parent) {}
		
	protected:
		/**
		 * It is the default behavior called when a right click occurs 
		 * over the scene.
		 */
		virtual void contextMenuEvent(QGraphicsSceneContextMenuEvent *contextMenuEvent);
		
	private:
		QtNetwork *m_networkView;
	
};

} //end namespace FD

#endif /*QTNETWORKSCENE_H_*/
