#include "QtDocument.h"
#include "QtNetwork.h"
#include <iostream>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include "UIDocument.h"
#include "UINetwork.h"
#include "QtFlowDesigner.h"
#include <QProcess>
#include <QMessageBox>
#include <QGraphicsScene>

namespace FD
{
    using namespace std;
    
    QtDocument::QtDocument(QtFlowDesigner *parent, const std::string &name)
    : QDialog(parent), m_uiDoc(NULL), m_name(name), m_flowdesigner(parent)
    {
        cerr<<"QtDocument created"<<endl;      
        
        m_vboxLayout = new QVBoxLayout(this);
        m_vboxLayout->setSpacing(6);
        m_vboxLayout->setMargin(0);		
        m_vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        
        //Create button group
        m_buttonGroup = new QButtonGroup(this);
        
             
        //Create run button
        QPushButton *runButton = new QPushButton("RUN", this);	
        m_buttonGroup->addButton(runButton);
		
		QPushButton *viewSource = new QPushButton("VIEW SOURCE",this);
		m_buttonGroup->addButton(viewSource);
        
        //connect signal
        connect(runButton,SIGNAL(clicked()),this, SLOT(onRunDocument()));
		connect(viewSource,SIGNAL(clicked()),this,SLOT(onViewSourceDocument()));
		
		//create tab widget
        m_tabWidget = new QTabWidget(NULL);
        m_tabWidget->setObjectName(QString::fromUtf8("tabWidget"));        
        connect(m_tabWidget, SIGNAL(currentChanged(int)),this, SLOT(tabWidgetChanged(int)));   
        m_vboxLayout->addWidget(runButton);		
        m_vboxLayout->addWidget(m_tabWidget);
        
        setLayout(m_vboxLayout);
        
        resize(800,600);
        
        //Create a new empty document 
        m_uiDoc = new UIDocument(name);	
        
        //Register to document events
        m_uiDoc->registerEvents(this);
               
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
        
        //m_uiDoc = new UIDocumentController(doc_name,this);
        m_uiDoc->setFullPath(fname);
        
        try {
            m_uiDoc->load();
            cerr<<"loading document : "<<fname<<endl;         
        } catch (BaseException *e) {
            //stringstream except;
            e->print(cerr);
            //doc->less_print (except.str());
        } catch (...) {
            //doc->less_print ("Unknown exception caught while loading document");
            
        }
        m_uiDoc->resetModified();
        
    }


    
	void QtDocument::save(const std::string &file)
	{
		if (m_uiDoc)
		{
			m_uiDoc->setFullPath(file);
			m_uiDoc->save();
		}
	}
	
	
	void QtDocument::onRunDocument()
	{
		cerr<<"Run clicked..."<<endl;
        
		if (m_uiDoc)
		{	
			if (m_flowdesigner)
			{
				m_flowdesigner->newProcess(m_uiDoc);			
			}			
		}
		
	}
	
	void QtDocument::onViewSourceDocument()
	{
		if (m_uiDoc)
		{
			int size = 0;
			char *data = m_uiDoc->saveToMemory(size);
			
			QMessageBox message(this);
			message.resize(800,600);
			message.setText(QString(data));			
			free(data);
			
			message.exec();
		}	
	
	}
	
	QtNetwork* QtDocument::addNetwork(UINetwork* net)
	{		
        QtNetwork *qtnet = new QtNetwork(this,net);
		qtnet->fitInView(qtnet->scene()->itemsBoundingRect(),Qt::KeepAspectRatio);
		qtnet->centerOn(qtnet->scene()->itemsBoundingRect().topLeft());
		m_networks.push_back(qtnet);
		qtnet->setObjectName(QString::fromUtf8(net->getName().c_str()));
        m_tabWidget->addTab(qtnet, net->getName().c_str());
		return qtnet;	
	}
    
	void QtDocument::addNetwork(const QString &name, UINetwork::Type type)
	{	
        m_uiDoc->addNetwork(name.toStdString(),type);
	}
    
    void QtDocument::tabWidgetChanged(int index)
    {
       // m_uiDoc->updateView();
    }
    
    //Network removed
    void QtDocument::notifyNetworkRemoved(const UIDocument *doc, const UINetwork* net)
    {
    	cerr<<"QtDocument::notifyNetworkRemoved(const UIDocument *doc, const UINetwork* net)"<<endl;
    }

    //Network Added
    void QtDocument::notifyNetworkAdded(const UIDocument *doc, const UINetwork* net)
    {
    	cerr<<"QtDocument::notifyNetworkAdded(const UIDocument *doc, const UINetwork* net)"<<endl;
    	
    	//add this network
    	addNetwork(const_cast<UINetwork*>(net));   	
    }
    			
    //Parameters changed
    void QtDocument::notifyParametersChanged(const UIDocument *doc, const ItemInfo *param)
    {
    	cerr<<"QtDocument::notifyParametersChanged(const UIDocument *doc, const ItemInfo *param)"<<endl;
    }

    //Name changed
    void QtDocument::notifyNameChanged(const UIDocument *doc, const std::string &name)
    {
    	cerr<<"QtDocument::notifyNameChanged(const UIDocument *doc, const std::string &name)"<<endl;
    }

    //Path changed
    void QtDocument::notifyPathChanged(const UIDocument *doc, const std::string path)
    {
    	cerr<<"QtDocument::notifyPathChanged(const UIDocument *doc, const std::string path)"<<endl;
    	setWindowTitle(path.c_str());
    }

    //Category changed
    void QtDocument::notifyCategoryChanged(const UIDocument *doc, const std::string &category)
    {
    	cerr<<"QtDocument::notifyCategoryChanged(const UIDocument *doc, const std::string &category)"<<endl;
    }

    //Comments changed
    void QtDocument::notifyCommentsChanged(const UIDocument *doc, const std::string &comments)
    {
    	cerr<<"QtDocument::notifyCommentsChanged(const UIDocument *doc, const std::string &comments)"<<endl;
    }
    			
    //Destroyed
    void QtDocument::notifyDestroyed(const UIDocument *doc)
    {
    	cerr<<"QtDocument::notifyDestroyed(const UIDocument *doc)"<<endl;
    }
    
}//namespace FD
