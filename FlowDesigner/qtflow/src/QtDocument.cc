#include "QtDocument.h"
#include "QtNetwork.h"
#include <iostream>
#include <QtGui/QLabel>

namespace FD
{
    using namespace std;
    
    QtDocument::QtDocument(QWidget *parent, const std::string &name)
    : QMainWindow(parent), m_name(name)         
    {
        cerr<<"QtDocument created"<<endl;      
        m_doc = new UIDocument (m_name);

        m_vboxLayout = new QVBoxLayout(this);
        m_vboxLayout->setSpacing(6);
        m_vboxLayout->setMargin(0);
        m_vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        
        m_tabWidget = new QTabWidget(this);
        m_tabWidget->setObjectName(QString::fromUtf8("tabWidget"));
        
        //QtNetwork *tab = new QtNetwork();
        //tab->setObjectName(QString::fromUtf8("tab"));
        //m_tabWidget->addTab(tab, "tab");
     
        
        m_vboxLayout->addWidget(m_tabWidget);
        setCentralWidget(m_tabWidget);          
        //resize(800,600);
   
    }      

    void QtDocument::open(const std::string &fname) {
        
        int pos = fname.rfind("/");

        string doc_name;

        if (pos != string::npos) {
            doc_name = fname.substr(pos + 1,fname.size() - (pos  +1));
        }
        else {
            doc_name = fname;
        }

        m_doc = new UIDocument(doc_name);
        m_doc->setFullPath(fname);

        try {
            m_doc->load();
            cerr<<"loading document : "<<fname<<endl;         
        } catch (BaseException *e) {
            //stringstream except;
            e->print(cerr);
            //doc->less_print (except.str());
        } catch (...) {
            //doc->less_print ("Unknown exception caught while loading document");
                     
        }
        m_doc->resetModified();
        
        
        if (m_doc)
        {         
            //create Qt networks
            std::vector<UINetwork *> nets = m_doc->get_networks();
            cerr<<"networks found :"<<nets.size()<<endl;
            for (unsigned int i = 0; i < nets.size(); i++)
            {
                m_networks.push_back(new QtNetwork(nets[i]));
                m_networks.back()->setObjectName(QString::fromUtf8(nets[i]->getName().c_str()));
                m_tabWidget->addTab(m_networks.back(), nets[i]->getName().c_str());
            }            
        }
    }      

}//namespace FD
