/****************************************************************************
**
** Copyright (C) 2006-2006 Trolltech ASA. All rights reserved.
**
** This file is part of the example classes of the Qt Toolkit.
**
** Licensees holding valid Qt Preview licenses may use this file in
** accordance with the Qt Preview License Agreement provided with the
** Software.
**
** See http://www.trolltech.com/pricing.html or email sales@trolltech.com for
** information about Qt Commercial License Agreements.
**
** Contact info@trolltech.com if any conditions of this licensing are
** not clear to you.
**
** This file is provided AS IS with NO WARRANTY OF ANY KIND, INCLUDING THE
** WARRANTY OF DESIGN, MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
**
****************************************************************************/

#include "QtNetwork.h"
#include "QtLink.h"
#include "QtNode.h"

#include <QDebug>
#include <QGraphicsScene>
#include <QWheelEvent>
#include <QGraphicsRectItem>
#include <QGraphicsSceneMouseEvent>

#include <math.h>
#include "QtTerminal.h"

QtNetwork::QtNetwork()
{
    QGraphicsScene* scene = new QGraphicsScene(this);
    scene->setItemIndexMethod(QGraphicsScene::NoIndex);
    scene->setSceneRect(-200, -200, 400, 400);
    setScene(scene);
    setCacheMode(CacheBackground);
    setRenderHint(QPainter::Antialiasing);

    // QGraphicsRectItem*rect1 = new QGraphicsRectItem(50, 50, 100, 100);
    // rect1->setBrush(QBrush(Qt::gray, Qt::SolidPattern));
    // scene->addItem(rect1);
    
    QtNode *node1 = new QtNode(this,"QtNode1");
    node1->addQtTerminal("terminal1",QtTerminal::INPUT);
    node1->addQtTerminal("terminal2",QtTerminal::INPUT);
    node1->addQtTerminal("terminal3",QtTerminal::INPUT);
    node1->addQtTerminal("terminal4",QtTerminal::OUTPUT);
    node1->addQtTerminal("terminal5",QtTerminal::OUTPUT);
    node1->addQtTerminal("terminal6",QtTerminal::OUTPUT);
    QtNode *node2 = new QtNode(this,"QtNode2");
    node2->addQtTerminal("terminal1",QtTerminal::INPUT);
    node2->addQtTerminal("terminal2",QtTerminal::INPUT);
    node2->addQtTerminal("terminal3",QtTerminal::INPUT);
    node2->addQtTerminal("terminal4",QtTerminal::OUTPUT);
    node2->addQtTerminal("terminal5",QtTerminal::OUTPUT);
    node2->addQtTerminal("terminal6",QtTerminal::OUTPUT);
    // QtNode *node3 = new QtNode(this);
    // QtNode *node4 = new QtNode(this);
    // centerQtNode = new QtNode(this);
    // QtNode *node6 = new QtNode(this);
    // QtNode *node7 = new QtNode(this);
    // QtNode *node8 = new QtNode(this);
    // QtNode *node9 = new QtNode(this);
    scene->addItem(node1);
    scene->addItem(node2);
    // scene->addItem(node3);
    // scene->addItem(node4);
    // scene->addItem(centerQtNode);
    // scene->addItem(node6);
    // scene->addItem(node7);
    // scene->addItem(node8);
    // scene->addItem(node9);
    //scene->addItem(new Link(node1, node2));
    // scene->addItem(new Edge(node2, node3));
    // scene->addItem(new Edge(node2, centerQtNode));
    // scene->addItem(new Edge(node3, node6));
    // scene->addItem(new Edge(node4, node1));
    // scene->addItem(new Edge(node4, centerQtNode));
    // scene->addItem(new Edge(centerQtNode, node6));
    // scene->addItem(new Edge(centerQtNode, node8));
    // scene->addItem(new Edge(node6, node9));
    // scene->addItem(new Edge(node7, node4));
    // scene->addItem(new Edge(node8, node7));
    // scene->addItem(new Edge(node9, node8));
    //
    node1->setPos(-50, -50);
    node2->setPos(0, -50);
    // node3->setPos(50, -50);
    // node4->setPos(-50, 0);
    // centerQtNode->setPos(0, 0);
    // node6->setPos(50, 0);
    // node7->setPos(-50, 50);
    // node8->setPos(0, 50);
    // node9->setPos(50, 50);

    scale(0.8, 0.8);
    setMinimumSize(400, 400);
    setWindowTitle(tr("Essai 1"));
}

void QtNetwork::keyPressEvent(QKeyEvent *event)
{
    switch (event->key()) {
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
            foreach (QGraphicsItem *item, scene()->items()) {
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
    QtNode* newQtNode = new QtNode(this);
    scene()->addItem(newQtNode);
    newQtNode->setPos(mapToScene(event->pos()));
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

