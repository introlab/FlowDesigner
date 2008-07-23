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