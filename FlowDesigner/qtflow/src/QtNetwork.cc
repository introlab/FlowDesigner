//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)

#include "QtNetwork.h"
#include "QtLink.h"
#include "QtNode.h"
#include "UILink.h"

#include <QDebug>
#include <QGraphicsScene>
#include <QWheelEvent>
#include <QGraphicsRectItem>
#include <QGraphicsSceneMouseEvent>
#include <QTreeWidgetItem>
#include "QtNodeTreeView.h"
#include "QtDocument.h"

#include <math.h>
#include "QtTerminal.h"
#include "UINetwork.h"
#include "UINode.h"
#include "UILink.h"
#include "UINodeRepository.h"
#include "UIDocument.h"
#include <iostream>
#include <sstream>

namespace FD
{
    
    using namespace std;
    
    QtNetwork::QtNetwork(QtDocument *doc, UINetwork *uiNetwork)
    : m_doc(doc), m_uiNetwork(uiNetwork)
    {
        //Creating graphics scene
        QGraphicsScene* scene = new QGraphicsScene(this);
        scene->setItemIndexMethod(QGraphicsScene::NoIndex);
        //scene->setSceneRect(-400, -400, 400, 400);
        setScene(scene);
        setCacheMode(CacheBackground);
        setRenderHint(QPainter::Antialiasing);
        setDragMode( QGraphicsView::RubberBandDrag );
		
        if (m_uiNetwork)
        {
            
            //ADD NODES
            std::vector<UINode *> nodes = m_uiNetwork->getNodes();
            for (unsigned int i = 0; i < nodes.size(); i++)
            {
            	addNode(nodes[i]);
            }

            //PROCESS LINKS
            std::vector<UILink *> links = m_uiNetwork->getLinks();
            for (unsigned int i = 0; i < links.size(); i++)
            {

            	addLink(links[i]);
            	/*
                //CAN WE DO BETTER?
                UINode *fromNode = links[i]->getFromTerminal()->getNode();
                UINode *destNode = links[i]->getToTerminal()->getNode();
                QtNode *source = m_nodeMap[fromNode];
                QtNode *dest = m_nodeMap[destNode];
                QtTerminal *sourceTerminal = source->getQtTerminal(links[i]->getFromTerminal());
                QtTerminal *destTerminal = dest->getQtTerminal(links[i]->getToTerminal());
                QtLink *link = new QtLink(sourceTerminal,destTerminal,links[i]);
                
                link->adjust();
                source->addQtLink(link);
                dest->addQtLink(link);
                scene->addItem(link);
                m_linkMap.insert(make_pair(links[i],link));
				*/
            }
            
            
            //TODO PROCCESS NETWORK PARAMETERS
            
           
            
            
            //register events
            m_uiNetwork->registerEvents(this);

        }
		else {
			cerr<<"No UINetworkController defined"<<endl;
		}
        
        //setDragEnabled(true);
        //setAcceptDrops(true);
        //setDropIndicatorShown(true)
        
        
        
        
        scale(1.0, 1.0);
        setMinimumSize(400, 400);
        
    }
    
    QtNetwork::~QtNetwork()
    {
    	if (m_uiNetwork)
    	{
    		m_uiNetwork->unregisterEvents(this);
    	}
    }
    
    
    const std::string QtNetwork::getName() const
    {
        return m_uiNetwork->getName();
    }
    
    void QtNetwork::keyPressEvent(QKeyEvent *event)
    {
        switch (event->key())
        {
            case Qt::Key_Up:
            // centerQtNode->moveBy(0, -20);
            break;
            case Qt::Key_Down:
            // centerQtNode->moveBy(0, 20);
            break;
            case Qt::Key_Left:
            // centerQtNode->moveBy(-20, 0);
            break;
            case Qt::Key_Right:
            // centerQtNode->moveBy(20, 0);
            break;
            case Qt::Key_Plus:
            scaleView(1.2);
            break;
            case Qt::Key_Minus:
            scaleView(1 / 1.2);
            break;
            case Qt::Key_Space:
            case Qt::Key_Enter:
            foreach (QGraphicsItem *item, scene()->items())
            {
                if (qgraphicsitem_cast<QtNode *>(item))
                    item->setPos(-150 + rand() % 300, -150 + rand() % 300);
            }
            break;
            case Qt::Key_Delete:
            {
                int nbSelectedItems = scene()->selectedItems().size();                
                while(scene()->selectedItems().size()!=0)
                {
                	
                    QtNode * selectNode = qgraphicsitem_cast<QtNode *>(scene()->selectedItems()[0]);  
                    if(selectNode)
                    {	
                        delete selectNode->getUINode();
                    }
                    else 
                    {
                    
	                    
	                    QtLink *selectLink = qgraphicsitem_cast<QtLink *>(scene()->selectedItems()[0]); 
	                    if (selectLink)
	                    {
	                    	delete selectLink->getUILink();
	                    }
	                    
                    }	
                    
                }
                break;
            }
            default:
            QGraphicsView::keyPressEvent(event);
        }
    }
    
    void QtNetwork::contextMenuEvent(QContextMenuEvent *event)
    {
        cerr<<"QtNetwork::contextMenuEvent(QContextMenuEvent *event)"<<endl;        
        m_contextMenuEvent = event;
        std::vector<UINetwork *> network = m_uiNetwork->getDocument()->get_networks();
        menu = new QMenu(this);
        menu->setTitle(tr("SubNetwork")); 
        for( unsigned int i=0; i < network.size(); i++ )            
        {
        	if (network[i]->getName() != m_uiNetwork->getName())
        	{
        		menu->addAction( new QAction(network[i]->getName().c_str(), this) );
        	}
        }
        
        connect(menu, SIGNAL(triggered(QAction*)) ,this, SLOT( menuTriggered(QAction*) ) ); 
        
        menu->exec(event->globalPos());
        
        //QtNode* newQtNode = new QtNode(this);
        //scene()->addItem(newQtNode);
        //newQtNode->setPos(mapToScene(event->pos()));
    }
    
    void QtNetwork::menuTriggered(QAction* action)
    {
     
        std::vector<UINetwork *> network = m_uiNetwork->getDocument()->get_networks();
        QString name;
        static int number = 0;
        stringstream networkname;
        
        networkname << action->text().toStdString() << "_" << number++;
        
        
        QPointF pos = mapToScene(m_contextMenuEvent->pos());
        UINode* node = m_uiNetwork->newNode(m_uiNetwork
        , networkname.str()
        , action->text().toStdString()
        , pos.x()
        , pos.y()
        , true);
        m_uiNetwork->addNode(node);
        
    }
    
    bool QtNetwork::isNodeExist(const QString &name)
    {
        for(unsigned int i=0; i<m_uiNetwork->getNodes().size(); i++)
            if(m_uiNetwork->getNodes()[i]->getName() == name.toStdString())
                return true;
        return false;
    }
    
    void QtNetwork::wheelEvent(QWheelEvent *event)
    {                
        scaleView(pow((double)2, -event->delta() / 240.0)); 
    }
    
    // void QtNetwork::drawBackground(QPainter *painter, const QRectF &rect)
    // {
        // Q_UNUSED(rect);
        
        // Shadow
        // QRectF sceneRect = this->sceneRect();
        // QRectF rightShadow(sceneRect.right(), sceneRect.top() + 5, 5, sceneRect.height());
        // QRectF bottomShadow(sceneRect.left() + 5, sceneRect.bottom(), sceneRect.width(), 5);
        // if (rightShadow.intersects(rect) || rightShadow.contains(rect))
        // painter->fillRect(rightShadow, Qt::darkGray);
        // if (bottomShadow.intersects(rect) || bottomShadow.contains(rect))
        // painter->fillRect(bottomShadow, Qt::darkGray);
        
        // Fill
        // QLinearGradient gradient(sceneRect.topLeft(), sceneRect.bottomRight());
        // gradient.setColorAt(0, Qt::white);
        // gradient.setColorAt(1, Qt::lightGray);
        // painter->fillRect(rect.intersect(sceneRect), gradient);
        // painter->setBrush(Qt::NoBrush);
        // painter->setBrush(Qt::lightGray);
        // painter->drawRect(sceneRect);
        
        // Text
        // QRectF textRect(sceneRect.left() + 4, sceneRect.top() + 4,
        // sceneRect.width() - 4, sceneRect.height() - 4);
        // QString message(tr("Click and drag the nodes around, and zoom with the mouse "
        // "wheel or the '+' and '-' keys"));
        
        // QFont font = painter->font();
        // font.setBold(true);
        // font.setPointSize(14);
        // painter->setFont(font);
        // painter->setPen(Qt::lightGray);
        // painter->drawText(textRect.translated(2, 2), message);
        // painter->setPen(Qt::black);
        // painter->drawText(textRect, message);
    // }
    
    void QtNetwork::scaleView(qreal scaleFactor)
    {
        qreal factor = matrix().scale(scaleFactor, scaleFactor).mapRect(QRectF(0, 0, 1, 1)).width();
        if (factor < 0.07 || factor > 100)
            return;
        
        scale(scaleFactor, scaleFactor);
    }
    
    //Drag & Drop
    void QtNetwork::dragEnterEvent(QDragEnterEvent *event)
    {
        //cerr<<"QtNetwork::dragEnterEvent(QDragEnterEvent *event)"<<endl;      
        event->accept();         
    }
    
    void QtNetwork::dragMoveEvent(QDragMoveEvent *event)
    {
        //cerr<<"QtNetwork::dragMoveEvent(QDragMoveEvent *event)";
		//cerr<<" x: "<<event->pos().x()<<" y:"<<event->pos().y()<<endl;
        event->accept();         
    }
    
	void QtNetwork::resizeSceneView()
	{
	
		QRectF sceneRect = scene()->itemsBoundingRect();
		ensureVisible(sceneRect,300,300);
	}
	
    void QtNetwork::dropEvent(QDropEvent *event)
    {
    	
    	
        //cerr<<"QtNetwork::dropEvent(QDropEvent *event)"<<endl;	
		//cerr<<"Source is "<<event->source()<<endl;					
		//cerr<<"Mime data "<<event->mimeData()->text().toStdString()<<endl;
		
		if (event->mimeData()->hasText())
		{
			//We received a node type name
			event->accept();         		
			
			//create this node
			if (m_uiNetwork)
			{		
				static int number = m_uiNetwork->getNodes().size();
                
				QPointF pos = mapToScene(event->pos());
               
				stringstream nodename;
				
				//TODO : Find a better way for node names?
				nodename << event->mimeData()->text().toStdString() << "_" << number++;
                
				UINode* node = m_uiNetwork->newNode(m_uiNetwork,nodename.str(),event->mimeData()->text().toStdString(),pos.x(),pos.y(),true);
				
				//This will generate an event, we will then display the node properly
				m_uiNetwork->addNode(node);
			}		
		}
		else
		{
			event->ignore();
		}
    }      
    
	void QtNetwork::mouseMoveEvent ( QMouseEvent * e )
	{
		QGraphicsView::mouseMoveEvent(e);
	}
	
	void QtNetwork::addQtLink(QtLink *link)
	{
		if (link)
		{
			//m_linkMap.insert(make_pair(links[i],link));
            
			if (link->getUILink() && m_uiNetwork)
			{
                //m_uiNetwork->addLink(link->getUILink());
				scene()->addItem(link);
				resizeSceneView();			
                m_linkMap.insert(make_pair(link->getUILink(),link));
			}		
		}
	}
	
    QtNode* QtNetwork::addNode(UINode* node)
	{
		//cerr<<"QtNode* QtNetwork::addNode(UINodeController* node)"<<endl;
		QtNode *qtnode = new QtNode(this,node);
        scene()->addItem(qtnode); 
		resizeSceneView();
        m_nodeMap.insert(make_pair(node,qtnode)); 
		
		
		//resize view if required
		
		
		
        return qtnode;
	}
	
    QtLink* QtNetwork::addLink(UILink* uiLink)
    {
    	if (uiLink)
    	{
    		//TODO CAN WE DO BETTER?
    		UINode *fromNode = uiLink->getFromTerminal()->getNode();
    		UINode *destNode = uiLink->getToTerminal()->getNode();
    		QtNode *source = m_nodeMap[fromNode];
    		QtNode *dest = m_nodeMap[destNode];
    		
    		if (source && dest)
    		{	
    			QtTerminal *sourceTerminal = source->getQtTerminal(uiLink->getFromTerminal());
    			QtTerminal *destTerminal = dest->getQtTerminal(uiLink->getToTerminal());
    			
    			if (sourceTerminal && destTerminal)
    			{
    				QtLink *link = new QtLink(sourceTerminal,destTerminal,uiLink);
    		        source->addQtLink(link);
    		        dest->addQtLink(link);
    				scene()->addItem(link);
					resizeSceneView();
    				m_linkMap.insert(make_pair(uiLink,link));					
    			}
    		}
    	}
    }
    
    
    QtLink* QtNetwork::addLink(QtTerminal *source, QtTerminal *dest, UILink* link)
    {
        if (source && dest && link)
        {
            QtLink *qtlink = new QtLink(source,dest,link);
            scene()->addItem(qtlink);
			
            m_linkMap.insert(make_pair(link,qtlink));
            return qtlink;
        }
        else
        {
            return NULL;
        }
    }
    void QtNetwork::removeNode(QtNode* node)
    { 
        cerr<<"QtNetwork::removeNode(QtNode* node)"<<endl;
        scene()->removeItem(node);
        delete node;
		resizeSceneView();
    }
    
    void QtNetwork::removeLink(QtLink* link)
    { 
        cerr<<"QtNetwork::removeLink(QtLink* link)"<<endl;
        scene()->removeItem(link);
        delete link;
		resizeSceneView();
    }
    
	//Node removed
	void QtNetwork::notifyNodeRemoved(const UINetwork *net, const UINode* node)
	{
		cerr<<"QtNetwork::notifyNodeRemoved(const UINetwork *net, const UINode* node)"<<endl;
		removeNode( m_nodeMap[const_cast<UINode*>(node)]);
		m_nodeMap.erase(const_cast<UINode*>(node));
	}
	
	//Node added
	void QtNetwork::notifyNodeAdded(const UINetwork *net, const UINode* node)
	{
		cerr<<"QtNetwork::notifyNodeAdded(const UINetwork *net, const UINode* node)"<<endl;
		addNode(const_cast<UINode*>(node));
	}
	
	//Link removed
	void QtNetwork::notifyLinkRemoved(const UINetwork *net, const UILink* link)
	{
		cerr<<"QtNetwork::notifyLinkRemoved(const UINetwork *net, const UILink* link)"<<endl;
		removeLink(m_linkMap[const_cast<UILink*>(link)]);
		m_linkMap.erase(const_cast<UILink*>(link));
	}
	
	//Link added
	void QtNetwork::notifyLinkAdded(const UINetwork *net, const UILink* link)
	{
		cerr<<"QtNetwork::notifyLinkAdded(const UINetwork *net, const UILink* link)"<<endl;
		addLink(const_cast<UILink*>(link));
	}

	//Note removed
	void QtNetwork::notifyNoteRemoved(const UINetwork *net, const UINote* note)
	{
		cerr<<"QtNetwork::notifyNoteRemoved(const UINetwork *net, const UINote* note)"<<endl;
	}
	
	//Note added
	void QtNetwork::notifyNoteAdded(const UINetwork *net, const UINote* note)
	{
		cerr<<"QtNetwork::notifyNoteAdded(const UINetwork *net, const UINote* note)"<<endl;
	}
	
	//NetTerminal removed
	void QtNetwork::notifyNetTerminalRemoved(const UINetwork *net, const UINetTerminal* terminal)
	{
		m_uiNetwork->interfaceChangeNotify();
		
		vector<UINetwork*> allNets = m_uiNetwork->getDocument()->get_networks();
		
		for (unsigned int i = 0; i < allNets.size(); i++)
		{
			allNets[i]->updateAllSubnetTerminals(m_uiNetwork->getName(), terminal->getName(), terminal->getType(),true);			
		}

		cerr<<"QtNetwork::notifyNetTerminalRemoved(const UINetwork *net, const UINetTerminal* terminal)"<<endl;
	}
	
	//NetTerminal added
	void QtNetwork::notifyNetTerminalAdded(const UINetwork *net, const UINetTerminal* terminal)
	{
		m_uiNetwork->interfaceChangeNotify();
		
		vector<UINetwork*> allNets = m_uiNetwork->getDocument()->get_networks();
		
		for (unsigned int i = 0; i < allNets.size(); i++)
		{
			allNets[i]->updateAllSubnetTerminals(m_uiNetwork->getName(), terminal->getName(), terminal->getType(),false);			
		}

		cerr<<"QtNetwork::notifyNetTerminalAdded(const UINetwork *net, const UINetTerminal* terminal)"<<endl;
	}
	
	//Name changed
	void QtNetwork::notifyNameChanged(const UINetwork *net, const std::string &name)
	{
		cerr<<"QtNetwork::notifyNameChanged(const UINetwork *net, const std::string &name)"<<endl;
	}
	
	//Description changed
	void QtNetwork::notifyDescriptionChanged(const UINetwork *net, const std::string &description)
	{
		cerr<<"QtNetwork::notifyDescriptionChanged(const UINetwork *net, const std::string &description)"<<endl;
	}
	
	//Destroyed
	void QtNetwork::notifyDestroyed(const UINetwork *net)
	{
		cerr<<"QtNetwork::notifyDestroyed(const UINetwork *net)"<<endl;
		m_uiNetwork = NULL;
	}
    
} //namespace FD
