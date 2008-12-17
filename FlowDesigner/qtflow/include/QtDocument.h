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
#ifndef _QTDOCUMENT_H_
#define _QTDOCUMENT_H_

#include <QtGui/QWidget>
#include <QtGui/QMainWindow>
#include <QtGui/QDialog>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QButtonGroup>
#include "UINetwork.h"
#include "UIDocument.h"



namespace FD
{
    
    //forward declaration
    class QtNetwork;
    class UIDocument;
	class UINetwork;
	class QtFlowDesigner;
	
    class QtDocument : public QDialog, public UIDocument::UIDocumentObserverIF
    {         
        
        Q_OBJECT;
        
        public:
		
        QtDocument(QtFlowDesigner *parent, const std::string &name = "Untitled");
		
		~QtDocument();
        
        void open(const std::string &file);       
        
        void save(const std::string &file);
        
		QString getFullPath(){return m_uiDoc->getPath().c_str();}
		
		QString getDocName(){return m_uiDoc->getName().c_str();}
		
		
        QtNetwork* addNetwork(UINetwork* net);
		
		void networkNameChanged(QtNetwork *net, const QString &name);
		
        void addNetwork(const QString &name, UINetwork::Type type);   
        
        bool isNetworkExist(const QString &name);
        
		public slots:
        
        void onRunDocument();
		void onViewSourceDocument();
		void onAddNoteDocument();
		void onDocumentProperties();
		void onNetworkNameChange();
		void onNetworkRemove();
        
        void tabWidgetChanged(int index);
        
        //Global event for changes
		virtual void notifyChanged(const UIDocument* doc);
			
		//Network removed
		virtual void notifyNetworkRemoved(const UIDocument *doc, const UINetwork* net);
		
		//Network Added
		virtual void notifyNetworkAdded(const UIDocument *doc, const UINetwork* net);
					
		//Parameters changed
		virtual void notifyParametersChanged(const UIDocument *doc, const ItemInfo *param);
		
		//Name changed
		virtual void notifyNameChanged(const UIDocument *doc, const std::string &name);
		
		//Path changed
		virtual void notifyPathChanged(const UIDocument *doc, const std::string path);
		
		//Category changed
		virtual void notifyCategoryChanged(const UIDocument *doc, const std::string &category);
		
		//Comments changed
		virtual void notifyCommentsChanged(const UIDocument *doc, const std::string &comments);
					
		//Destroyed
		virtual void notifyDestroyed(const UIDocument *doc);	
              
        protected:
        
        /**
         * Called before the document is closed. The event can 
         * be accepted or rejected. This method is used 
         * to ask to user to save the document before it 
         * closes. 
         * @author Mathieu Labbe
         * @param event the close event
         */
        virtual void closeEvent ( QCloseEvent * event );
        
        /**
         * Ask to user if he wants to save or not the document 
         * when it is closed.
         * @author Mathieu Labbe
         * @return true if the document can be closed, otherwise false
         */
        bool maybeSave();
        
        QVBoxLayout *m_vboxLayout;
        QHBoxLayout *m_hboxLayout;
        std::string m_name;
        QTabWidget *m_tabWidget;         
        QButtonGroup *m_buttonGroup;
        QtFlowDesigner *m_flowdesigner;
        
        UIDocument *m_uiDoc;
        
        std::map<UINetwork*, QtNetwork*> m_networkMap;

    };
} //namespace FD  
#endif
