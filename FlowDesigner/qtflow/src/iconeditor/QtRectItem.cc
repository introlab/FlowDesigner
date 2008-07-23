#include "iconeditor/QtRectItem.h"
#include <QBrush>
#include <QPen>

#include <iostream>

//===============================
// public
//===============================
QtRectItem::QtRectItem(QPointF pos, QGraphicsScene * scene) : QGraphicsRectItem(NULL, scene), m_vertexSelected(NULL)
{
	setAcceptsHoverEvents(true);
	setFlag(ItemIsMovable);
    setFlag(ItemIsSelectable);
    
    m_reversedLeft = false;
	m_reversedTop = false;

	setRect(QRectF(pos, QSize(0,0)));
	
	for(int i=0 ; i<4; i++) {
		m_vertexes[i] = new QtVertex(this, scene);
	}
	showVertexes(false);
	
	// Connect signals vertexSelected
	for(int i=0 ; i<4; i++) {
		connect(m_vertexes[i], SIGNAL(vertexSelected(bool, QtVertex*)), this, SLOT(vertexSelected(bool, QtVertex*)));
	}

	// Connect signals vertexMoved
	for(int i=0 ; i<4; i++) {
		connect(m_vertexes[i], SIGNAL(vertexMoved(QtVertex*, const QPointF &)), this, SLOT(vertexMoved(QtVertex*, const QPointF &)));
	}
}

QtRectItem::~QtRectItem()
{
}

void QtRectItem::resize(const QPointF &mousePos) 
{
	QRectF newRect;
    QPointF fixedPoint;

	QRectF tmpRect = rect();
	bool leftEdge = true; // if the left edge moves or not
	bool topEdge = true; // if the top edge moves or not
	
	fixedPoint = tmpRect.topLeft();
	
	if(m_reversedLeft && mousePos.x() > tmpRect.right()) {
		m_reversedLeft = false;
		fixedPoint.setX(tmpRect.right()); 
	}
	else if(m_reversedLeft) {
		leftEdge = false;
		fixedPoint.setX(tmpRect.right());
	}
	else if(mousePos.x() < tmpRect.left()) {
		m_reversedLeft = true;
		leftEdge = false;
	}
	
	if(m_reversedTop && mousePos.y() > tmpRect.bottom()) {
		m_reversedTop = false;
		fixedPoint.setY(tmpRect.bottom());
	}
	else if(m_reversedTop) {
		topEdge = false;
		fixedPoint.setY(tmpRect.bottom());
	}
	else if(mousePos.y() < tmpRect.top()) {
		m_reversedTop = true;
		topEdge = false;
	}

	// Set new rect size
    
    if(leftEdge && topEdge) {
    	newRect = QRectF(fixedPoint.x(), fixedPoint.y(), mousePos.x() - fixedPoint.x(), mousePos.y() - fixedPoint.y());
    }
    else if(leftEdge && !topEdge) {
    	newRect = QRectF(fixedPoint.x(), mousePos.y(), mousePos.x() - fixedPoint.x(), fixedPoint.y() - mousePos.y());
    }
    else if(!leftEdge && !topEdge) {
    	newRect = QRectF(mousePos.x(), mousePos.y(), fixedPoint.x() - mousePos.x(), fixedPoint.y() - mousePos.y());
    }
    else if(!leftEdge && topEdge) {
    	newRect = QRectF(mousePos.x(), fixedPoint.y(), fixedPoint.x() - mousePos.x(), mousePos.y() - fixedPoint.y());
    }
    
    setRect(newRect);	
	updateVertexes();
}

//===============================
// public slots
//===============================
void QtRectItem::vertexSelected(bool selected, QtVertex* vertex)
{
	if(selected) {
		int index = 0;
		for(int i=0; i<4; i++) {
			if(m_vertexes[i] == vertex) {
				m_vertexSelected = vertex;
				index = i;
				break;	
			}
		}
		//init reversed flag
		if(index == 0) { //topLeft
			m_reversedLeft = true;
			m_reversedTop = true;
		}
		else if(index == 1) { //bottomLeft
			m_reversedLeft = true;
			m_reversedTop = false;
		}
		else if(index == 2) { //bottomRight
			m_reversedLeft = false;
			m_reversedTop = false;
		}
		else if(index == 3) { //topRight
			m_reversedLeft = false;
			m_reversedTop = true;
		}
	}
	else {
		m_vertexSelected = NULL;
	}
}

void QtRectItem::vertexMoved(QtVertex* vertex, const QPointF &newPos)
{
	resize(newPos);
}


//============================
// protected
//============================
void QtRectItem::hoverEnterEvent(QGraphicsSceneHoverEvent* event)
{
	showVertexes(true);
	QGraphicsRectItem::hoverEnterEvent(event);
}

void QtRectItem::hoverLeaveEvent(QGraphicsSceneHoverEvent* event)
{
	//if(!isSelected()) {
		showVertexes(false);
	//}
	QGraphicsRectItem::hoverLeaveEvent(event);
}

void QtRectItem::mousePressEvent(QGraphicsSceneMouseEvent* event)
{
	QGraphicsRectItem::mousePressEvent(event);
}

void QtRectItem::mouseMoveEvent(QGraphicsSceneMouseEvent* event)
{
	QGraphicsRectItem::mouseMoveEvent(event);
}

QVariant QtRectItem::itemChange(GraphicsItemChange change, const QVariant &value)
{
    return QGraphicsRectItem::itemChange(change, value);
}

//==========================
// private
//==========================
void QtRectItem::updateVertexes()
{
	QRectF r = rect();
	m_vertexes[0]->setPosition(r.topLeft());
	m_vertexes[1]->setPosition(r.bottomLeft());
	m_vertexes[2]->setPosition(r.bottomRight());
	m_vertexes[3]->setPosition(r.topRight());
}

void QtRectItem::showVertexes(bool show)
{
	for(int i=0; i<4; i++) {
		m_vertexes[i]->setVisible(show);
	}
	this->update();
}