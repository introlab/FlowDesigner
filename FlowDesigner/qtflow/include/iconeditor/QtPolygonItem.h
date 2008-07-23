#ifndef QTPOLYGONITEM_H_
#define QTPOLYGONITEM_H_

#include <QObject>
#include <QGraphicsPolygonItem>
#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include "QtVertex.h"

class QtPolygonItem : public QObject, public QGraphicsPolygonItem
{
	Q_OBJECT;

private:
	QtVertex* m_vertexSelected;
	QVector<QtVertex*> m_vertexes;
	
public:
	QtPolygonItem(QPointF pos, QGraphicsScene * scene);
	~QtPolygonItem();
	
	void resize(int vertexIndex, const QPointF &newPos);
	void addVertex(const QPointF &point);

public slots:
	void vertexSelected(bool selected, QtVertex* vertex);
	void vertexMoved(QtVertex* vertex, const QPointF &newPos);

protected:
	virtual void hoverEnterEvent(QGraphicsSceneHoverEvent* event);
	virtual void hoverLeaveEvent(QGraphicsSceneHoverEvent* event);
	virtual void mousePressEvent(QGraphicsSceneMouseEvent* event);
	virtual void mouseMoveEvent(QGraphicsSceneMouseEvent* event);
	virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);
	
private:
	void updateVertexes();
	void showVertexes(bool show);
};

#endif /*QTPOLYGONITEM_H_*/
