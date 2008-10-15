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
#include "QtNetwork.h"
#include "QtLink.h"
#include "QtNode.h"
#include "UILink.h"
#include "QtNetworkScene.h"

#include <QDebug>
#include <QGraphicsScene>
#include <QWheelEvent>
#include <QGraphicsRectItem>
#include <QGraphicsSceneMouseEvent>
#include <QTreeWidgetItem>
#include "QtNodeTreeView.h"
#include "QtDocument.h"
#include "QtNote.h"

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
        QtNetworkScene* scene = new QtNetworkScene(this);
        scene->setItemIndexMethod(QGraphicsScene::NoIndex);
        //scene->setSceneRect(-400, -400, 400, 400);
        setScene(scene);
        setCacheMode(CacheBackground);
        setRenderHint(QPainter::Antialiasing);
        setDragMode(QGraphicsView::RubberBandDrag);

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
            }


            //TODO PROCCESS NETWORK PARAMETERS?

            //ADD NOTES
            std::vector<UINote*> notes = m_uiNetwork->getNotes();
            for (unsigned int i = 0; i < notes.size(); i++)
            {
            	addNote(notes[i]);
            }


            //register events
            m_uiNetwork->registerEvents(this);

        }
		else {
			cerr<<"No UINetworkController defined"<<endl;
		}

        //setDragEnabled(true);
        //setAcceptDrops(true);
        //setDropIndicatorShown(true)

        setSceneRect(0,0,500,500);

        scale(1.0, 1.0);
        //resize only if the scene isn't empty.
        if(scene->items().size() > 2) {
        	resizeSceneView();
        }
        update();
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
            //resize only if the scene isn't empty.
            if(scene()->items().size() > 2) {
            	fitInView(scene()->itemsBoundingRect(),Qt::KeepAspectRatio);
            }
            break;
            case Qt::Key_Delete:
            {



                while (scene()->selectedItems().size() > 0)
                {

                    QtNode * selectedNode = dynamic_cast<QtNode *>(scene()->selectedItems()[0]);
                    QtLink *selectedLink = dynamic_cast<QtLink *>(scene()->selectedItems()[0]);
                    QtNote *selectedNote = dynamic_cast<QtNote *>(scene()->selectedItems()[0]);

                    //TODO FIX MEMORY LEAKS
                    //MAKE SURE WE ARE MAKING THE RIGHT THING...

                    if(selectedNode)  //DELETE NODE
                    {
                        m_uiNetwork->removeNode(selectedNode->getUINode(),true);
                    }
                    else if (selectedLink) //DELETE LINK
                    {
                    	m_uiNetwork->removeLink(selectedLink->getUILink(),true);
                    }
                    else if (selectedNote) //DELETE NOTE
                    {
                    	m_uiNetwork->removeNote(selectedNote->getUINote(),true);
                    }
                    else
                    {
                    	cerr<<"Unknown item to delete..."<<endl;
                    	break;
                    }



                }
                break;
            }
            default:
            QGraphicsView::keyPressEvent(event);
        }
    }

    void QtNetwork::menuTriggered(QAction* action, const QPointF &pos)
    {

        std::vector<UINetwork *> network = m_uiNetwork->getDocument()->get_networks();
        QString name;
        static int number = 0;
        stringstream networkname;

        networkname << action->text().toStdString() << "_" << number++;

        UINode* node = m_uiNetwork->createNode(networkname.str()
        , action->text().toStdString()
        , pos.x()
        , pos.y());
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

    void QtNetwork::drawBackground(QPainter *painter, const QRectF &rect)
    {
        Q_UNUSED(rect);

        // Shadow
        QRectF sceneRect = this->sceneRect();
        QRectF rightShadow(sceneRect.right(), sceneRect.top() + 5, 5, sceneRect.height());
        QRectF bottomShadow(sceneRect.left() + 5, sceneRect.bottom(), sceneRect.width(), 5);
        if (rightShadow.intersects(rect) || rightShadow.contains(rect))
        	painter->fillRect(rightShadow, Qt::darkGray);
        if (bottomShadow.intersects(rect) || bottomShadow.contains(rect))
        	painter->fillRect(bottomShadow, Qt::darkGray);

        // Fill
        QLinearGradient gradient(sceneRect.topLeft(), sceneRect.bottomRight());
        gradient.setColorAt(0, Qt::white);
        if(m_uiNetwork && !m_uiNetwork->getDocument()->isEditable()) {
        	gradient.setColorAt(1, QColor(Qt::lightGray));
    	}
    	else {
    		gradient.setColorAt(1, QColor(Qt::lightGray).lighter(130));
    	}
        painter->fillRect(rect.intersect(sceneRect), gradient);
        painter->setBrush(Qt::NoBrush);
        painter->setBrush(QColor(Qt::lightGray).lighter(130));
        painter->drawRect(sceneRect);

        // Text
        /*
        QRectF textRect(sceneRect.left() + 4, sceneRect.top() + 4,
        sceneRect.width() - 4, sceneRect.height() - 4);
        QString message(tr("Click and drag the nodes around, and zoom with the mouse "
        ,"wheel or the '+' and '-' keys"));

        QFont font = painter->font();
        font.setBold(true);
        font.setPointSize(14);
        painter->setFont(font);
        painter->setPen(Qt::lightGray);
        painter->drawText(textRect.translated(2, 2), message);
        painter->setPen(Qt::black);
        painter->drawText(textRect, message);
        */
     }

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
        if(m_uiNetwork && !m_uiNetwork->getDocument()->isEditable()) {
        	event->ignore();
        }
        else {
        	event->accept();
        }
    }

    void QtNetwork::dragMoveEvent(QDragMoveEvent *event)
    {
        //cerr<<"QtNetwork::dragMoveEvent(QDragMoveEvent *event)";
		//cerr<<" x: "<<event->pos().x()<<" y:"<<event->pos().y()<<endl;
        if(m_uiNetwork && !m_uiNetwork->getDocument()->isEditable()) {
        	event->ignore();
        }
        else {
        	event->accept();
        }
    }

	void QtNetwork::resizeSceneView()
	{
	 	QRectF boundingRect = scene()->itemsBoundingRect();
	    setSceneRect(boundingRect);
	}

    void QtNetwork::dropEvent(QDropEvent *event)
    {


        //cerr<<"QtNetwork::dropEvent(QDropEvent *event)"<<endl;
		//cerr<<"Source is "<<event->source()<<endl;
		//cerr<<"Mime data "<<event->mimeData()->text().toStdString()<<endl;
		if(m_uiNetwork && !m_uiNetwork->getDocument()->isEditable()) {
        	event->ignore();
        }
        else if (event->mimeData()->hasText())
		{
			//We received a node type name
			event->accept();

			//create this node
			if (m_uiNetwork)
			{
				int number = m_uiNetwork->getNodes().size();

				QPointF pos = mapToScene(event->pos());

				if (UINodeRepository::Find(event->mimeData()->text().toStdString()))
				{

					stringstream nodename;

					//TODO : Find a better way for node names?
					nodename << event->mimeData()->text().toStdString() << "_" << number;

					UINode* node = m_uiNetwork->createNode(nodename.str(),event->mimeData()->text().toStdString(),pos.x(),pos.y());

					//This will generate an event, we will then display the node properly
					m_uiNetwork->addNode(node);
				}
			}
		}
		else
		{
			event->ignore();
		}
    }

    static QPoint lastCursorPos;
	void QtNetwork::mouseMoveEvent ( QMouseEvent * e )
	{
        if(e->buttons() & Qt::MidButton)
        {
            QScrollBar *hBar = horizontalScrollBar();
            QScrollBar *vBar = verticalScrollBar();
            QPoint delta = e->pos() - lastCursorPos;
            hBar->setValue(hBar->value() + (isRightToLeft() ? delta.x() : -delta.x()));
            vBar->setValue(vBar->value() - delta.y());
            lastCursorPos = e->pos();
            return;
        }
		QGraphicsView::mouseMoveEvent(e);
	}

    void QtNetwork::mousePressEvent (QMouseEvent * e)
    {
        if(e->button() == Qt::MidButton)
        {
            lastCursorPos = e->pos();
            setDragMode(QGraphicsView::NoDrag);
            QApplication::setOverrideCursor( QCursor(Qt::ClosedHandCursor) );
            return;
        }
        else if(m_uiNetwork && !m_uiNetwork->getDocument()->isEditable()) {
    		return;
    	}
        QGraphicsView::mousePressEvent(e);
    }

    void QtNetwork::mouseReleaseEvent (QMouseEvent * e)
    {
        if(e->button() == Qt::MidButton)
        {
            QApplication::restoreOverrideCursor();
            setDragMode(QGraphicsView::RubberBandDrag);
            return;
        }
        else if(m_uiNetwork && !m_uiNetwork->getDocument()->isEditable()) {
    		return;
    	}
        QGraphicsView::mouseReleaseEvent(e);
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

    				connect(link, SIGNAL(signalLinkProbed(int, const QString &)), this, SLOT(linkProbed(int, const QString &)));
    			}
    		}
    	}
		return NULL;
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
        scene()->removeItem(node);
        delete node;
		resizeSceneView();
    }

    void QtNetwork::removeLink(QtLink* link)
    {
       	//Make sure we own this link...resizeSceneView();	
		scene()->removeItem(link);
		delete link;
		resizeSceneView();
		
    }

	//Node removed
	void QtNetwork::notifyNodeRemoved(const UINetwork *net, const UINode* node)
	{
		cerr<<"QtNetwork::notifyNodeRemoved(const UINetwork *net, const UINode* node)"<<endl;
		
		if (m_nodeMap.find(const_cast<UINode*>(node)) != m_nodeMap.end())
		{	
			removeNode( m_nodeMap[const_cast<UINode*>(node)]);
			m_nodeMap.erase(const_cast<UINode*>(node));
		}	
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
		if (m_linkMap.find(const_cast<UILink*>(link)) != m_linkMap.end())
		{	
			removeLink(m_linkMap[const_cast<UILink*>(link)]);
			m_linkMap.erase(const_cast<UILink*>(link));
		}
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
		removeNote(m_noteMap[const_cast<UINote*>(note)]);
		m_noteMap.erase(const_cast<UINote*>(note));
	}

	//Note added
	void QtNetwork::notifyNoteAdded(const UINetwork *net, const UINote* note)
	{
		cerr<<"QtNetwork::notifyNoteAdded(const UINetwork *net, const UINote* note)"<<endl;
		addNote(const_cast<UINote*>(note));
	}

	//NetTerminal removed
	void QtNetwork::notifyNetTerminalRemoved(const UINetwork *net, const UINetTerminal* terminal)
	{
        cerr<<"QtNetwork::notifyNetTerminalRemoved(const UINetwork *net, const UINetTerminal* terminal)"<<endl;

		m_uiNetwork->interfaceChangeNotify();

		vector<UINetwork*> allNets = m_uiNetwork->getDocument()->get_networks();

		for (unsigned int i = 0; i < allNets.size(); i++)
		{
			allNets[i]->updateAllSubnetTerminals(m_uiNetwork->getName(), terminal->getName(), terminal->getType(),true);
		}
	}

	//NetTerminal added
	void QtNetwork::notifyNetTerminalAdded(const UINetwork *net, const UINetTerminal* terminal)
	{
        cerr<<"QtNetwork::notifyNetTerminalAdded(const UINetwork *net, const UINetTerminal* terminal)"<<endl;

		m_uiNetwork->interfaceChangeNotify();

		vector<UINetwork*> allNets = m_uiNetwork->getDocument()->get_networks();

		for (unsigned int i = 0; i < allNets.size(); i++)
		{
			allNets[i]->updateAllSubnetTerminals(m_uiNetwork->getName(), terminal->getName(), terminal->getType(),false);
		}
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

    QtNote* QtNetwork::addNote(UINote* uinote)
    {
		QtNote *note = new QtNote(this,uinote);

		scene()->addItem(note);
		resizeSceneView();
		m_noteMap.insert(make_pair(uinote,note));
		return note;

    }

    void QtNetwork::removeNote(QtNote *note)
    {
        scene()->removeItem(note);
        delete note;
        resizeSceneView();
    }

    void QtNetwork::mouseDoubleClickEvent ( QMouseEvent * e )
    {
    	if(m_uiNetwork && !m_uiNetwork->getDocument()->isEditable()) {
    		return;
    	}
        QGraphicsView::mouseDoubleClickEvent(e);
    }

} //namespace FD
