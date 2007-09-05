#include "QtDocument.h"
#include "QtNetwork.h"
#include <iostream>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include "QtRunContext.h"

#include "UIDocumentController.h"
#include "UINetworkController.h"

namespace FD
{
    using namespace std;
    
    QtDocument::QtDocument(QWidget *parent, const std::string &name)
    : QDialog(parent), m_doc(NULL), m_name(name)         
    {
        cerr<<"QtDocument created"<<endl;      
        
        m_vboxLayout = new QVBoxLayout(this);
        m_vboxLayout->setSpacing(6);
        m_vboxLayout->setMargin(0);		
        m_vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        
        //Create button group
        m_buttonGroup = new QButtonGroup(this);
        
        
        
        //m_vboxLayout->insertLayout(0,m_buttonGroup);
        
        //Create run button
        QPushButton *runButton = new QPushButton("RUN", this);	
        m_buttonGroup->addButton(runButton);
        
        //connect signal
        connect(runButton,SIGNAL(clicked()),this, SLOT(onRunDocument()));
		
		//create tab widget
        m_tabWidget = new QTabWidget(NULL);
        m_tabWidget->setObjectName(QString::fromUtf8("tabWidget"));        
        connect(m_tabWidget, SIGNAL(currentChanged(int)),this, SLOT(tabWidgetChanged(int)));   
        m_vboxLayout->addWidget(runButton);		
        m_vboxLayout->addWidget(m_tabWidget);
        
        setLayout(m_vboxLayout);
        
        resize(800,600);
        
        open(name);
        
    }      
    
    
    
	QtDocument::QtDocument(QWidget *parent, UINetwork::Type type, UIDocumentController* doc)
    : QDialog(parent), m_doc(doc)
	{
		cerr<<"QtDocument::QtDocument(QWidget *parent, UIDocumentController* doc)"<<__FILE__<<__LINE__<<endl;
        
        m_vboxLayout = new QVBoxLayout(this);
        m_vboxLayout->setSpacing(6);
        m_vboxLayout->setMargin(0);		
        m_vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        
		//Create button group
		m_buttonGroup = new QButtonGroup(this);
		
		
		
		//m_vboxLayout->insertLayout(0,m_buttonGroup);
		
		//Create run button
		QPushButton *runButton = new QPushButton("RUN", this);	
		m_buttonGroup->addButton(runButton);
		
		//connect signal
		connect(runButton,SIGNAL(clicked()),this, SLOT(onRunDocument()));
		
		//create tab widget
        m_tabWidget = new QTabWidget(NULL);
        m_tabWidget->setObjectName(QString::fromUtf8("tabWidget"));
        connect(m_tabWidget, SIGNAL(currentChanged(int)),this, SLOT(tabWidgetChanged(int)));
		m_vboxLayout->addWidget(runButton);		
		m_vboxLayout->addWidget(m_tabWidget);
		
		setLayout(m_vboxLayout);
        
        resize(800,600);	
        
		
		if (m_doc == NULL)
		{
			//Create a new document with a MAIN network
			m_doc = new UIDocumentController("Untitled",this);		
			m_doc->addNetwork("MAIN",type);
		}
		else
		{
			//OPEN ALREADY EXISTING DOCUMENT
		}
        
	}
    
    bool QtDocument::isNetworkExist(const QString &name)
    {
        for(unsigned int i=0; i<m_networks.size(); i++)
            if(m_networks[i]->getName() == name.toStdString())
                return true;
        return false;
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
        
        m_doc = new UIDocumentController(doc_name,this);
        m_doc->setFullPath(fname);
        
        try {
            m_doc->load();
            m_doc->updateView();
            cerr<<"loading document : "<<fname<<endl;         
        } catch (BaseException *e) {
            //stringstream except;
            e->print(cerr);
            //doc->less_print (except.str());
        } catch (...) {
            //doc->less_print ("Unknown exception caught while loading document");
            
        }
        m_doc->resetModified();
        
        /*
        if (m_doc)
        {         
            //create Qt networks
            std::vector<UINetwork *> nets = m_doc->get_networks();
            cerr<<"networks found :"<<nets.size()<<endl;
            for (unsigned int i = 0; i < nets.size(); i++)
            {
                m_networks.push_back(new QtNetwork(dynamic_cast<UINetworkController*>(nets[i])));
                m_networks.back()->setObjectName(QString::fromUtf8(nets[i]->getName().c_str()));
                m_tabWidget->addTab(m_networks.back(), nets[i]->getName().c_str());
            }            
        }
        */
    }
    
	void QtDocument::save(const std::string &file)
	{
		if (m_doc)
		{
			m_doc->setFullPath(file);
			m_doc->save();
		}
	}
	
	
	void QtDocument::onRunDocument()
	{
		cerr<<"Run clicked..."<<endl;
        
		if (m_doc)
		{
			ParameterSet params;
			QtRunContext context(m_doc,params);
			context.run();
		}
		
	}
	
	QtNetwork* QtDocument::addNetwork(UINetworkController* net)
	{		
        QtNetwork *qtnet = new QtNetwork(net);
		net->setQtNetwork(qtnet);
		m_networks.push_back(qtnet);
		qtnet->setObjectName(QString::fromUtf8(net->getName().c_str()));
        m_tabWidget->addTab(qtnet, net->getName().c_str());
		return qtnet;	
	}
    
	void QtDocument::addNetwork(const QString &name, UINetwork::Type type)
	{	
        m_doc->addNetwork(name.toStdString(),type);
	}
    
    void QtDocument::tabWidgetChanged(int index)
    {
        m_doc->updateView();
    }
}//namespace FD
