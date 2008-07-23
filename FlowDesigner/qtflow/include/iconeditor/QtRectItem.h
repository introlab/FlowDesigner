#ifndef QTRECTITEM_H_
#define QTRECTITEM_H_

#include <QObject>
#include <QGraphicsRectItem>
#include <QGraphicsSceneMouseEvent>
#include <QPainter>
#include "QtVertex.h"

class QtRectItem : public QObject, public QGraphicsRectItem
{
	Q_OBJECT;

private:
	QtVertex* m_vertexSelected;
	QtVertex* m_vertexes[4];
	bool m_reversedLeft;
    bool m_reversedTop;
	
public:
	QtRectItem(QPointF pos, QGraphicsScene * scene);
	~QtRectItem();
	
	void resize(const QPointF &mousePos);

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

#endif /*QTRECTITEM_H_*/
