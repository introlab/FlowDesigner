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
#include "QtNetworkScene.h"
#include "UIDocument.h"
#include <QtGui/QMenu>

namespace FD
{
	
void QtNetworkScene::contextMenuEvent(QGraphicsSceneContextMenuEvent *event)
{
	//Make sure the event is not accepted
	event->setAccepted(false);
	
	//Passs event to all items in the scene
	QGraphicsScene::contextMenuEvent(event);
	
	if(event->isAccepted()) {
		return;
	}
	
    if(!m_networkView->getUINetWork()->getDocument()->isEditable()) {
    	event->ignore();
    }
    else {
        std::vector<UINetwork *> network = m_networkView->getUINetWork()->getDocument()->get_networks();
        QMenu* menu = new QMenu(m_networkView);
        menu->setTitle(tr("SubNetwork")); 
        for( unsigned int i=0; i < network.size(); i++ )            
        {
        	if (network[i]->getName() != m_networkView->getUINetWork()->getName())
        	{
        		menu->addAction( new QAction(network[i]->getName().c_str(), this) );
        	}
        }
        
        QAction *action = menu->exec(event->screenPos());
        if(action) {
        	m_networkView->menuTriggered(action, event->scenePos());
        }
        
        //QtNode* newQtNode = new QtNode(this);
        //scene()->addItem(newQtNode);
        //newQtNode->setPos(mapToScene(event->pos()));
    } 
    event->accept();
}

}//end namespace FD