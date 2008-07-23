#include <QtGui>

#include "iconeditor/QtIconScene.h"
#include "iconeditor/QtRectItem.h"
#include "iconeditor/QtLineItem.h"
#include "iconeditor/QtEllipseItem.h"
#include "iconeditor/QtPolygonItem.h"

#include <iostream>

QtIconScene::QtIconScene(QObject *parent)
    : QGraphicsScene(parent)
{
    myMode = MoveItem;
    myItemColor = Qt::white;
    myLineColor = Qt::black;
    
    m_tmpRectItem = 0;
    m_tmpLineItem = 0;
    m_tmpEllipseItem = 0;
    m_tmpPolygonItem = 0;
    m_insertingPolygon = false;
    
    m_drawBackground = true;
}

bool QtIconScene::setLineColor(const QColor &color)
{
    myLineColor = color;
    if(selectedItems().size() > 0) {
	    QAbstractGraphicsShapeItem *item = dynamic_cast<QAbstractGraphicsShapeItem *>(selectedItems().first());
	    if(item) {
		    item->setPen(myLineColor);
		    return true;
	    }
    }
    return false;
}

bool QtIconScene::setItemColor(const QColor &color)
{
    myItemColor = color;
    if(selectedItems().size() > 0) {
	    QAbstractGraphicsShapeItem *item = dynamic_cast<QAbstractGraphicsShapeItem *>(selectedItems().first());
	    if(item) {
	    	item->setBrush(myItemColor);
	    	return true;
	    }
    }
    return false;
}

void QtIconScene::setMode(Mode mode)
{
    myMode = mode;
}

void QtIconScene::mousePressEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
    if (mouseEvent->button() == Qt::LeftButton) {
        
	    if(m_insertingPolygon) {
	    	m_tmpPolygonItem->addVertex(mouseEvent->scenePos());
	    	QtLineItem* newLine = new QtLineItem(mouseEvent->scenePos(), this);
	    	m_tmpPolygonTmpLines.append(newLine);
		    newLine->setPen(myLineColor);
	    }
	    else {
		    QtIconItem *item;
		    switch (myMode) {
		    	case InsertRectItem:
		    		m_tmpRectItem = new QtRectItem(mouseEvent->scenePos(), this);
		    		m_tmpRectItem->setPen(myLineColor);
		            m_tmpRectItem->setBrush(myItemColor);
		            break;
		        case InsertLineItem:
		        	m_tmpLineItem = new QtLineItem(mouseEvent->scenePos(), this);
		            m_tmpLineItem->setPen(myLineColor);
		            break;
		        case InsertEllipseItem:
		        	m_tmpEllipseItem = new QtEllipseItem(mouseEvent->scenePos(), this);
		            m_tmpEllipseItem->setPen(myLineColor);
		            m_tmpEllipseItem->setBrush(myItemColor);
		            break;
		        case InsertPolygonItem:
		        	m_tmpPolygonItem = new QtPolygonItem(mouseEvent->scenePos(), 0);
		            m_tmpPolygonItem->setPen(myLineColor);
		            m_tmpPolygonItem->setBrush(myItemColor);
		            m_insertingPolygon = true;
		            
	    			m_tmpPolygonTmpLines.append(new QtLineItem(mouseEvent->scenePos(), this));
		    		m_tmpPolygonTmpLines.last()->setPen(myLineColor);
		            break;
		    	default:
		        	break;
		    }
	    }
    }
    else if(mouseEvent->button() == Qt::RightButton) {
    	if(m_insertingPolygon) {
	    	if(m_tmpPolygonItem != 0 && myMode == InsertPolygonItem) {
	    		this->addItem(m_tmpPolygonItem);
	    		foreach(QtLineItem* item, m_tmpPolygonTmpLines) {
	    			delete item;
	    		}
	    		m_tmpPolygonTmpLines.clear();
	    		m_tmpPolygonItem->setSelected(true);
				emit itemInserted();
			}
			m_tmpPolygonItem = 0;
	    	m_insertingPolygon = false;
    	}
    }
    else {
    	return;
    }
    QGraphicsScene::mousePressEvent(mouseEvent);
}

void QtIconScene::mouseMoveEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	QPointF mousePos = mouseEvent->scenePos();
	if (myMode == InsertRectItem && m_tmpRectItem != 0) {
		m_tmpRectItem->resize(mousePos);
		emit itemInserting(m_tmpRectItem);
    }
    else if (myMode == InsertLineItem && m_tmpLineItem != 0) {
		m_tmpLineItem->resize(m_tmpLineItem->line().p1(), mousePos);
		emit itemInserting(m_tmpLineItem);
    }
   	else if (myMode == InsertEllipseItem && m_tmpEllipseItem != 0) {
		m_tmpEllipseItem->resize(mousePos);
		emit itemInserting(m_tmpEllipseItem);
    }
    else if (myMode == InsertPolygonItem && m_tmpPolygonItem != 0) {
		m_tmpPolygonTmpLines.last()->resize(m_tmpPolygonTmpLines.last()->line().p1(), mousePos);
		emit itemInserting(m_tmpPolygonTmpLines.last());
    }
    else if (myMode == MoveItem) {
        QGraphicsScene::mouseMoveEvent(mouseEvent);
    }
}

void QtIconScene::mouseReleaseEvent(QGraphicsSceneMouseEvent *mouseEvent)
{
	if(m_tmpRectItem != 0 && myMode == InsertRectItem) {
		m_tmpRectItem->setSelected(true);
		emit itemInserted();
	}
	else if(m_tmpLineItem != 0 && myMode == InsertLineItem) {
		m_tmpLineItem->setSelected(true);
		emit itemInserted();
	}
	else if(m_tmpEllipseItem != 0 && myMode == InsertEllipseItem) {
		m_tmpEllipseItem->setSelected(true);
		emit itemInserted();
	}
    m_tmpRectItem = 0;
    m_tmpLineItem = 0;
    m_tmpEllipseItem = 0;
    QGraphicsScene::mouseReleaseEvent(mouseEvent);
}

void QtIconScene::drawBackground(QPainter *painter, const QRectF &rect)
{
	if(m_drawBackground) {
		// Back
		painter->setBrush(QBrush(Qt::gray));
	    painter->drawRect(rect);
		
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
	    gradient.setColorAt(1, QColor(Qt::lightGray));	
	    painter->fillRect(rect.intersect(sceneRect), gradient);
	    painter->setBrush(QBrush(Qt::white));
	    painter->drawRect(sceneRect);
	    
	    // Draw grid
	    int lineNumber = 9;
	    QLineF hLines[lineNumber]; //horizontal lines
	    QLineF vLines[lineNumber]; //vertical lines
	    qreal caseW = sceneRect.width() / 10; // case width
	    qreal caseH = sceneRect.height() / 10; // case height
	    
	    for(int i=0, j=1; i<lineNumber; i++, j++) {
	    	hLines[i] = QLineF(0, j*caseH, sceneRect.width(), j*caseH);
	    	vLines[i] = QLineF(j*caseW, 0, j*caseW, sceneRect.height());
	    }
	    
	    painter->drawLines(hLines, lineNumber);
	    painter->drawLines(vLines, lineNumber);
	}
}

void QtIconScene::setDrawBackground(bool drawIt)
{
	m_drawBackground = drawIt;
}
