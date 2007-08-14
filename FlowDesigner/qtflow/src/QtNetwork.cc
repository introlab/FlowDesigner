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

#include <math.h>
#include "QtTerminal.h"
#include "UINetworkController.h"
#include "UINodeController.h"
#include "UINodeRepository.h"
#include "UIDocument.h"
#include <iostream>
#include <sstream>

namespace FD
{
    
    using namespace std;
    
    QtNetwork::QtNetwork(UINetwork *uiNetwork)
    : m_uiNetwork(uiNetwork)
    {
        //Creating graphics scene
        QGraphicsScene* scene = new QGraphicsScene(this);
        scene->setItemIndexMethod(QGraphicsScene::NoIndex);
        //scene->setSceneRect(-400, -400, 400, 400);
        setScene(scene);
        setCacheMode(CacheBackground);
        setRenderHint(QPainter::Antialiasing);
        
		/*
        if (m_uiNetwork)
        {
            
            //PROCESS NODES
            std::vector<UINode *> nodes = m_uiNetwork->getNodes();
            cerr<<"QtNetwork::QtNetwork  nodes found : "<<nodes.size()<<endl;
            for (unsigned int i = 0; i < nodes.size(); i++)
            {
                QtNode *node = new QtNode(this,nodes[i]);
                scene->addItem(node);
                m_nodeMap.insert(make_pair(nodes[i],node));
                //m_nodes.push_back(node);
            }
            
            //PROCESS LINKS
            std::vector<UILink *> links = m_uiNetwork->getLinks();
            cerr<<"QtNetwork::QtNetwork  links found : "<<links.size()<<endl;
            for (unsigned int i = 0; i < links.size(); i++)
            {
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
            }
            
            
            //TODO PROCCESS PARAMETERS
            
            */
            
            //UPDATE SCENE RECT
            /*QRectF bbox = scene->itemsBoundingRect();
            qreal x1,y1,x2,y2;
            bbox.getCoords(&x1,&y1,&x2,&y2);
            bbox.setCoords(x1 - 100, y1 - 100, x2 + 100, y2 + 100);
            scene->setSceneRect(bbox);
            */
            /*
        }
		else {
			cerr<<"No UINetwork defined"<<endl;
		}
        */
        //setDragEnabled(true);
        //setAcceptDrops(true);
        //setDropIndicatorShown(true)
        
        scale(1.0, 1.0);
        setMinimumSize(400, 400);
        
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
            menu->addAction( new QAction(network[i]->getName().c_str(), this) );
            
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
        for( unsigned int i=0 ; i< network.size(); i++ )
        {
            if( network[i]->getName() == action->text().toStdString())
            {
                QPointF pos = mapToScene(m_contextMenuEvent->pos());
                UINode* node = m_uiNetwork->newNode(network[i]
                , action->text().toStdString()
                , action->text().toStdString()
                , pos.x()
                , pos.y()
                , true);
                m_uiNetwork->addNode(node);
                 
            }
        }
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
        cerr<<"QtNetwork::dragEnterEvent(QDragEnterEvent *event)"<<endl;      
        event->accept();         
    }
    
    void QtNetwork::dragMoveEvent(QDragMoveEvent *event)
    {
        cerr<<"QtNetwork::dragMoveEvent(QDragMoveEvent *event)";
		cerr<<" x: "<<event->pos().x()<<" y:"<<event->pos().y()<<endl;
        event->accept();         
    }
    
    void QtNetwork::dropEvent(QDropEvent *event)
    {
        cerr<<"QtNetwork::dropEvent(QDropEvent *event)"<<endl;	
		cerr<<"Source is "<<event->source()<<endl;					
		cerr<<"Mime data "<<event->mimeData()->text().toStdString()<<endl;
		
		if (event->mimeData()->hasText())
		{
			//We received a node type name
			event->accept();         		
			
			//create this node
			if (m_uiNetwork)
			{
                
				QPointF pos = mapToScene(event->pos());
                
				//m_uiNetwork->createNode(event->mimeData()->text().toStdString(),pos.x(),pos.y(),true);
                
				static int number = 0;
				
				stringstream nodename;
				
				nodename << event->mimeData()->text().toStdString() << "_" << number++;
                
				UINode* node = m_uiNetwork->newNode(m_uiNetwork,nodename.str(),event->mimeData()->text().toStdString(),pos.x(),pos.y(),true);
				m_uiNetwork->addNode(node);
                
                
                /*
				UINode* uiNode = m_uiNetwork->newNode(m_uiNetwork,"NAME",event->mimeData()->text().toStdString(),pos.x(),pos.y(),true);
                
				if (uiNode)
				{
					m_uiNetwork->addNode(uiNode);
					QtNode *node = new QtNode(this,uiNode);														
					scene()->addItem(node);
					
					cerr<<"DROPPED ITEM POS X:"<<node->pos().x()<<" Y:"<<node->pos().y()<<endl;
					
					m_nodeMap.insert(make_pair(uiNode,node));
					//scene()->update();
				}
				
                */	
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
				m_uiNetwork->addLink(link->getUILink());
				scene()->addItem(link);
				m_linkMap.insert(make_pair(link->getUILink(),link));
			}		
		}
	}
	
    QtNode* QtNetwork::addNode(UINode* node)
	{
		cerr<<"QtNode* QtNetwork::addNode(UINodeController* node)"<<endl;
		QtNode *qtnode = new QtNode(this,node);
        scene()->addItem(qtnode);
        m_nodeMap.insert(make_pair(node,qtnode));              
        return qtnode;
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
} //namespace FD
