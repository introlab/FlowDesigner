#include "QtNote.h"
#include "QtNetwork.h"
#include <QBrush>
#include <QDialog>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QLineEdit>
#include <QPushButton>
#include <iostream>
#include <QObject>

namespace FD
{
	using namespace std;

	QtNote::QtNote(QtNetwork *parent, UINote *note)
		: m_uiNote(note), m_textItem(NULL)
	{
		
		if (m_uiNote)
		{
			
			
			double x,y;	
			m_uiNote->getPos(x,y);
			setPos(x,y);
			
			//Create a label
			m_textItem = new QGraphicsTextItem(m_uiNote->getLabel().c_str(),this);

			//Resize Rect
			QRectF rect = m_textItem->boundingRect();
			setRect(rect);
			
			//change color
			QPen myPen= pen();
			QBrush myBrush = myPen.brush();
			myBrush.setColor(Qt::yellow);
			myPen.setBrush(myBrush);
			setPen(myPen);
			setBrush(myBrush);
			setToolTip(m_uiNote->getText().c_str());
			
			//Notes can be moved and selected.
	        setFlag(ItemIsMovable);
	        setFlag(ItemIsSelectable);
			
		}
	}
	
	void QtNote::update()
	{
		m_textItem->setPlainText(m_uiNote->getLabel().c_str());
		//Resize Rect
		QRectF rect = m_textItem->boundingRect();
		setRect(rect);
		setToolTip(m_uiNote->getText().c_str());
	}
	
	void QtNote::mouseDoubleClickEvent ( QGraphicsSceneMouseEvent * event)
	{

			if (event->button() == Qt::LeftButton)
			{
				event->accept();
				QDialog *myDialog = new QDialog();
				QVBoxLayout *vlayout = new QVBoxLayout(myDialog);
				
				//Create line edit
				QLineEdit *myLineEdit = new QLineEdit(myDialog);
				vlayout->addWidget(myLineEdit);
				myLineEdit->setText(m_uiNote->getLabel().c_str());
				
				//Create text edit
				QTextEdit *myTextEdit = new QTextEdit(myDialog);
				vlayout->addWidget(myTextEdit);
				myTextEdit->setText(m_uiNote->getText().c_str());
				
				QPushButton *myButton = new QPushButton("OK",myDialog);
				vlayout->addWidget(myButton);
				
				//Clicking on OK will close the dialog
				QObject::connect(myButton,SIGNAL(clicked()), myDialog, SLOT(accept()));
				
				
				//Exec dialog
				myDialog->exec();
				
				//Get label from dialog
				m_uiNote->setLabel(myLineEdit->text().toStdString());
				
				//Get text from dialog
				m_uiNote->setText(myTextEdit->toPlainText().toStdString());
				
				//Destroy dialog
				delete myDialog;
				
				update();
							
			}
	}
	
	
	
	QVariant QtNote::itemChange(GraphicsItemChange change, const QVariant &value)
	{
	        if (change == ItemPositionChange && scene()) 
	        {
	       		//value is the new position.
	         	QPointF newPos = value.toPointF();
	            
	            //emit position changed signal
	         	if (m_uiNote)
	         	{	
	         		m_uiNote->setPos(newPos.x(),newPos.y());
	         	}
	     	}
	        return QGraphicsRectItem::itemChange(change, value);
	}
	
	        
} //namespace FD


