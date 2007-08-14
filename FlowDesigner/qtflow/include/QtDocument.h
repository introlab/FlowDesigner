#ifndef _QTDOCUMENT_H_
#define _QTDOCUMENT_H_

#include <QtGui/QWidget>
#include <QtGui/QMainWindow>
#include <QtGui/QDialog>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QButtonGroup>



namespace FD
{
    
    //forward declaration
    class QtNetwork;
    class UIDocumentController;
	class UINetworkController;
	
    class QtDocument : public QDialog
    {         
        
        Q_OBJECT;
        
        public:
		
        QtDocument(QWidget *parent, const std::string &name);
        QtDocument(QWidget *parent, UIDocumentController *doc);
		
        //QtDocument(QWidget *parent, const std::string &name = "Untitled");
        
        void open(const std::string &file);       
        
        void save(const std::string &file);
        
        QtNetwork* addNetwork(UINetworkController* net);   
        void addSubnetNetwork(QString name);
        void addIteratorNetwork(QString name);
        std::vector<QtNetwork*> getNetworks() { return m_networks; }
        
		public slots:
        
        void onRunDocument();
        
        
        protected:
        QVBoxLayout *m_vboxLayout;
        std::string m_name;
        QTabWidget *m_tabWidget;         
        QButtonGroup *m_buttonGroup;
        
        UIDocumentController *m_doc;
        
        std::vector<QtNetwork*> m_networks;
        
        
    };
} //namespace FD  
#endif
