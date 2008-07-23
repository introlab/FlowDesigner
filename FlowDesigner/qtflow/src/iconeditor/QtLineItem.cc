#include "iconeditor/QtLineItem.h"
#include <QPen>

#include <iostream>

//===============================
// public
//===============================
QtLineItem::QtLineItem(QPointF pos, QGraphicsScene * scene) : QGraphicsLineItem(NULL, scene), m_vertexSelected(NULL)
{
	setAcceptsHoverEvents(true);
	setFlag(ItemIsMovable);
    setFlag(ItemIsSelectable);

	setLine(QLineF(pos,pos));
	
	for(int i=0 ; i<2; i++) {
		m_vertexes[i] = new QtVertex(this, scene);
	}
	showVertexes(false);
	
	// Connect signals vertexSelected
	for(int i=0 ; i<2; i++) {
		connect(m_vertexes[i], SIGNAL(vertexSelected(bool, QtVertex*)), this, SLOT(vertexSelected(bool, QtVertex*)));
	}

	// Connect signals vertexMoved
	for(int i=0 ; i<2; i++) {
		connect(m_vertexes[i], SIGNAL(vertexMoved(QtVertex*, const QPointF &)), this, SLOT(vertexMoved(QtVertex*, const QPointF &)));
	}
}

QtLineItem::~QtLineItem()
{
}

void QtLineItem::resize(const QPointF &startPos, const QPointF &mousePos) 
{
	QLineF newLine(startPos, mousePos);
    setLine(newLine);
	updateVertexes();
}

//===============================
// public slots
//===============================
void QtLineItem::vertexSelected(bool selected, QtVertex* vertex)
{
	if(selected) {
		for(int i=0; i<2; i++) {
			if(m_vertexes[i] == vertex) {
				m_vertexSelected = vertex;
				break;	
			}
		}
	}
	else {
		m_vertexSelected = NULL;
	}
}

void QtLineItem::vertexMoved(QtVertex* vertex, const QPointF &newPos)
{
	if(vertex == m_vertexes[0]) {
		resize(newPos, line().p2());
	}
	else if(vertex == m_vertexes[1]) {
		resize(line().p1(), newPos);
	}
}


//============================
// protected
//============================
void QtLineItem::hoverEnterEvent(QGraphicsSceneHoverEvent* event)
{
	showVertexes(true);
	QGraphicsLineItem::hoverEnterEvent(event);
}

void QtLineItem::hoverLeaveEvent(QGraphicsSceneHoverEvent* event)
{
	//if(!isSelected()) {
		showVertexes(false);
	//}
	QGraphicsLineItem::hoverLeaveEvent(event);
}

void QtLineItem::mousePressEvent(QGraphicsSceneMouseEvent* event)
{
	QGraphicsLineItem::mousePressEvent(event);
}

void QtLineItem::mouseMoveEvent(QGraphicsSceneMouseEvent* event)
{
	QGraphicsLineItem::mouseMoveEvent(event);
}

QVariant QtLineItem::itemChange(GraphicsItemChange change, const QVariant &value)
{
    return QGraphicsLineItem::itemChange(change, value);
}

//==========================
// private
//==========================
void QtLineItem::updateVertexes()
{
	QLineF l = line();
	m_vertexes[0]->setPosition(l.p1());
	m_vertexes[1]->setPosition(l.p2());
}

void QtLineItem::showVertexes(bool show)
{
	for(int i=0; i<2; i++) {
		m_vertexes[i]->setVisible(show);
	}
	this->update();
}