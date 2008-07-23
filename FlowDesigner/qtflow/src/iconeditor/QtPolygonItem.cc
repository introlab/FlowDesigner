#include "iconeditor/QtPolygonItem.h"
#include <QPen>
#include <QBrush>

#include <iostream>

//===============================
// public
//===============================
QtPolygonItem::QtPolygonItem(QPointF pos, QGraphicsScene * scene) : QGraphicsPolygonItem(NULL, scene), m_vertexSelected(NULL)
{
	setAcceptsHoverEvents(true);
	setFlag(ItemIsMovable);
    setFlag(ItemIsSelectable);

	this->addVertex(pos);
}

QtPolygonItem::~QtPolygonItem()
{
}

void QtPolygonItem::resize(int vertexIndex, const QPointF &newPos) 
{
	QPolygonF tmpPolygon = polygon();
	if(vertexIndex < tmpPolygon.size()) {
		tmpPolygon[vertexIndex] = newPos;
		setPolygon(tmpPolygon);
	}
	updateVertexes();
}

void QtPolygonItem::addVertex(const QPointF &point)
{
	QPolygonF tmpPolygon = polygon();
	tmpPolygon << point;
	setPolygon(tmpPolygon);
	
	QtVertex* vertex = new QtVertex(this, this->scene());
	// Connect signals vertexSelected
	connect(vertex, SIGNAL(vertexSelected(bool, QtVertex*)), this, SLOT(vertexSelected(bool, QtVertex*)));
	// Connect signals vertexMoved
	connect(vertex, SIGNAL(vertexMoved(QtVertex*, const QPointF &)), this, SLOT(vertexMoved(QtVertex*, const QPointF &)));
	m_vertexes.append(vertex);
	
	updateVertexes();
}

//===============================
// public slots
//===============================
void QtPolygonItem::vertexSelected(bool selected, QtVertex* vertex)
{
	/*if(selected) {
		for(int i=0; i<2; i++) {
			if(m_vertexes[i] == vertex) {
				m_vertexSelected = vertex;
				break;	
			}
		}
	}
	else {
		m_vertexSelected = NULL;
	}*/
}

void QtPolygonItem::vertexMoved(QtVertex* vertex, const QPointF &newPos)
{
	for(int i=0; i<m_vertexes.size(); i++) {
		if(m_vertexes[i] == vertex) {
			resize(i, newPos);
		}
	}
}


//============================
// protected
//============================
void QtPolygonItem::hoverEnterEvent(QGraphicsSceneHoverEvent* event)
{
	showVertexes(true);
	QGraphicsPolygonItem::hoverEnterEvent(event);
}

void QtPolygonItem::hoverLeaveEvent(QGraphicsSceneHoverEvent* event)
{
	//if(!isSelected()) {
		showVertexes(false);
	//}
	QGraphicsPolygonItem::hoverLeaveEvent(event);
}

void QtPolygonItem::mousePressEvent(QGraphicsSceneMouseEvent* event)
{
	QGraphicsPolygonItem::mousePressEvent(event);
}

void QtPolygonItem::mouseMoveEvent(QGraphicsSceneMouseEvent* event)
{
	QGraphicsPolygonItem::mouseMoveEvent(event);
}

QVariant QtPolygonItem::itemChange(GraphicsItemChange change, const QVariant &value)
{
    return QGraphicsPolygonItem::itemChange(change, value);
}

//==========================
// private
//==========================
void QtPolygonItem::updateVertexes()
{
	QPolygonF p = polygon();
	for(int i=0; i<p.size() && i<m_vertexes.size(); i++) {
		m_vertexes[i]->setPosition(p[i]);
	}
}

void QtPolygonItem::showVertexes(bool show)
{
	foreach(QtVertex* vertex, m_vertexes) {
		vertex->setVisible(show);
	}
	this->update();
}