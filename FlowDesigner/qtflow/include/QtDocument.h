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
        
        void open(const std::string &file);       
        
        void save(const std::string &file);
        
		QString getFullPath(){return m_uiDoc->getPath().c_str();}
		
		QString getDocName(){return m_uiDoc->getName().c_str();}
		
		
        QtNetwork* addNetwork(UINetwork* net);

        void addNetwork(const QString &name, UINetwork::Type type);   
        
        bool isNetworkExist(const QString &name);
        
		public slots:
        
        void onRunDocument();
		void onViewSourceDocument();
		void onAddNoteDocument();
		void onDocumentProperties();
        
        void tabWidgetChanged(int index);
        
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
