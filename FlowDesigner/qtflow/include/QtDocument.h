#ifndef _QTDOCUMENT_H_
#define _QTDOCUMENT_H_

#include <QtGui/QWidget>
#include <QtGui/QMainWindow>
#include <QtGui/QDialog>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QButtonGroup>
#include "UINetworkController.h"
#include "UIDocument.h"



namespace FD
{
    
    //forward declaration
    class QtNetwork;
    class UIDocumentController;
	class UINetwork;
	class QtFlowDesigner;
	
    class QtDocument : public QDialog, public UIDocument::UIDocumentObserverIF
    {         
        
        Q_OBJECT;
        
        public:
		
        QtDocument(QtFlowDesigner *parent, const std::string &name = "Untitled");
        
        void open(const std::string &file);       
        
        void save(const std::string &file);
        
        QtNetwork* addNetwork(UINetworkController* net);

        void addNetwork(const QString &name, UINetwork::Type type);   
        
        std::vector<QtNetwork*> getNetworks() { return m_networks; }
        
        bool isNetworkExist(const QString &name);
        
		public slots:
        
        void onRunDocument();
        
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
        std::string m_name;
        QTabWidget *m_tabWidget;         
        QButtonGroup *m_buttonGroup;
        QtFlowDesigner *m_flowdesigner;
        
        UIDocumentController *m_doc;
        
        std::vector<QtNetwork*> m_networks;

    };
} //namespace FD  
#endif
