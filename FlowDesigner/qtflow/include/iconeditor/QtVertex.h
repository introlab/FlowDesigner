#ifndef QTVERTEX_H_
#define QTVERTEX_H_

#include <QObject>
#include <QGraphicsRectItem>
#include <QGraphicsSceneHoverEvent>
#include <QPainter>

class QtVertex : public QObject, public QGraphicsRectItem
{
	Q_OBJECT;
	
public:
	static const int SIZE = 10;
private:
	
public:
	QtVertex(QGraphicsItem * parent, QGraphicsScene * scene);
	~QtVertex();
	
	void setPosition(QPointF pos);
		
signals:
	void vertexSelected(bool selected, QtVertex* vertexRect);
	void vertexMoved(QtVertex* vertexRect, QPointF newPos);
	
protected:
	virtual void hoverEnterEvent(QGraphicsSceneHoverEvent* event);
	virtual void hoverLeaveEvent(QGraphicsSceneHoverEvent* event);
	virtual void mouseMoveEvent(QGraphicsSceneMouseEvent* event);
	virtual QVariant itemChange(GraphicsItemChange change, const QVariant &value);
};

#endif /*QTVERTEX_H_*/
