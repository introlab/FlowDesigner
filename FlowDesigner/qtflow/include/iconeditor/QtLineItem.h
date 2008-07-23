#ifndef QTLINEITEM_H_
#define QTLINEITEM_H_

#include <QObject>
#include <QGraphicsLineItem>
#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include "QtVertex.h"

class QtLineItem : public QObject, public QGraphicsLineItem
{
	Q_OBJECT;

private:
	QtVertex* m_vertexSelected;
	QtVertex* m_vertexes[2];
	
public:
	QtLineItem(QPointF pos, QGraphicsScene * scene);
	~QtLineItem();
	
	void resize(const QPointF &startPos, const QPointF &mousePos);

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

#endif /*QTLINEITEM_H_*/
