#include "QtDocument.h"
#include "QtNetwork.h"
#include <iostream>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include "QtRunContext.h"

namespace FD
{
    using namespace std;
    
    QtDocument::QtDocument(QWidget *parent, const std::string &name)
    : QDialog(parent), m_name(name)         
    {
        cerr<<"QtDocument created"<<endl;      
        m_doc = new UIDocument (m_name);

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
        
		m_vboxLayout->addWidget(runButton);		
		m_vboxLayout->addWidget(m_tabWidget);
		
		setLayout(m_vboxLayout);
 
        resize(800,600);
   
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
}//namespace FD
