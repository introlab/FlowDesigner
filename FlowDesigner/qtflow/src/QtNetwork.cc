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

#include <math.h>
#include "QtTerminal.h"

#include <iostream>

namespace FD
{

    using namespace std;

    QtNetwork::QtNetwork(UINetwork *uiNetwork)
            : m_uiNetwork(uiNetwork)
    {
        //Creating graphics scene
        QGraphicsScene* scene = new QGraphicsScene(this);
        scene->setItemIndexMethod(QGraphicsScene::NoIndex);
        scene->setSceneRect(-400, -400, 400, 400);
        setScene(scene);
        setCacheMode(CacheBackground);
        setRenderHint(QPainter::Antialiasing);

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
                QtLink *link = new QtLink(sourceTerminal,destTerminal);
                
                link->adjust();
                source->addQtLink(link);
                dest->addQtLink(link);
                scene->addItem(link);
                m_linkMap.insert(make_pair(links[i],link));
            }


            //TODO PROCCESS PARAMETERS



            //UPDATE SCENE RECT
            QRectF bbox = scene->itemsBoundingRect();
            qreal x1,y1,x2,y2;
            bbox.getCoords(&x1,&y1,&x2,&y2);
            bbox.setCoords(x1 - 100, y1 - 100, x2 + 100, y2 + 100);
            scene->setSceneRect(bbox);

        }


        scale(1.0, 1.0);
        setMinimumSize(400, 400);
        setWindowTitle("Essai 1");
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
        //QtNode* newQtNode = new QtNode(this);
        //scene()->addItem(newQtNode);
        //newQtNode->setPos(mapToScene(event->pos()));
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

} //namespace FD
