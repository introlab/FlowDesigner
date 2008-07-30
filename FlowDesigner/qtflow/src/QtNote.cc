/***********************************************************************************
** Copyright (C) 2006-2008 Laborius (http://www.gel.usherbrooke.ca/laborius/). 
** All rights reserved. 
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License
** as published by the Free Software Foundation; either version 2
** of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA
***********************************************************************************/
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
		: m_qtNetwork(parent), m_uiNote(note), m_textItem(NULL)
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
	    	if (!m_uiNote->isVisible())
	        {
	    		hide();
	        }
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
	         	
	         	m_qtNetwork->resizeSceneView();
	     	}
	        return QGraphicsRectItem::itemChange(change, value);
	}
	
	        
} //namespace FD


