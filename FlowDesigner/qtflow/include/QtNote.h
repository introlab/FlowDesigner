#ifndef _QTNOTE_H_
#define _QTNOTE_H_

#include <QGraphicsRectItem>
#include <QGraphicsTextItem>
#include <QGraphicsSceneMouseEvent>
#include <string>
#include "UINote.h"

namespace FD
{

	class QtNetwork;

	class QtNote : public QGraphicsRectItem
	{
	protected:
		UINote *m_uiNote;
		QGraphicsTextItem *m_textItem;
		void mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event);
		QVariant itemChange(GraphicsItemChange change, const QVariant &value);
		void update();
	public:
		QtNote(QtNetwork *parent, UINote *note);
	};



}//namespace FD

#endif