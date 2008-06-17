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
#include <QDialog>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QLineEdit>
#include <QPushButton>
#include <QObject>
#include <QScrollBar>
#include "QtDocumentParameters.h"

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
        
        //Create horizontal layout for buttons
        m_hboxLayout = new QHBoxLayout(NULL); 
        m_vboxLayout->addLayout(m_hboxLayout);
        
        //Create button group
        m_buttonGroup = new QButtonGroup(this);
              
        //Create run button
        QPushButton *runButton = new QPushButton("RUN", this);	
        m_buttonGroup->addButton(runButton);
        m_buttonGroup->setId(runButton,0);
        m_hboxLayout->addWidget(runButton);
        
		QPushButton *viewSource = new QPushButton("XML-SOURCE",this);
		m_buttonGroup->addButton(viewSource);
		m_buttonGroup->setId(viewSource,1);
		m_hboxLayout->addWidget(viewSource);
		
		QPushButton  *addNote = new QPushButton("Add Note",this);
		m_buttonGroup->addButton(addNote);
		m_buttonGroup->setId(addNote,2);
		m_hboxLayout->addWidget(addNote);
		
		QPushButton *docProperties = new QPushButton("Document Properties",this);
		m_buttonGroup->addButton(docProperties);
		m_buttonGroup->setId(docProperties,3);
		m_hboxLayout->addWidget(docProperties);
        
        //connect signal
        connect(runButton,SIGNAL(clicked()),this, SLOT(onRunDocument()));
		connect(viewSource,SIGNAL(clicked()),this,SLOT(onViewSourceDocument()));
		connect(addNote,SIGNAL(clicked()), this, SLOT(onAddNoteDocument()));
		connect(docProperties,SIGNAL(clicked()), this, SLOT(onDocumentProperties()));
		
		//create tab widget
        m_tabWidget = new QTabWidget(NULL);
        m_tabWidget->setObjectName(QString::fromUtf8("tabWidget"));        
        connect(m_tabWidget, SIGNAL(currentChanged(int)),this, SLOT(tabWidgetChanged(int)));   
        	
        m_vboxLayout->addWidget(m_tabWidget);
        
        setLayout(m_vboxLayout);
        
        resize(800,600);
        
        //Create a new empty document 
        m_uiDoc = new UIDocument(name);	
        
        setWindowTitle(m_uiDoc->getName().c_str());
        
        //Register to document events
        m_uiDoc->registerEvents(this);
               
    }      
    
    bool QtDocument::isNetworkExist(const QString &name)
    {
    
    	if (m_uiDoc)
    	{
    		if (m_uiDoc->getNetworkNamed(name.toStdString()))
    		{
    			return true;
    		}
    		else
    		{
    			return false;
    		}
    	}
    	
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

			QDialog *myDialog = new QDialog();
			QVBoxLayout *vlayout = new QVBoxLayout(myDialog);
			myDialog->resize(640,480);
			
			//Create line edit
			QLineEdit *myLineEdit = new QLineEdit(myDialog);
			vlayout->addWidget(myLineEdit);
			myLineEdit->setText(m_uiDoc->getName().c_str());
			myLineEdit->setReadOnly(true);
			
			//Create text edit
			QTextEdit *myTextEdit = new QTextEdit(myDialog);
			vlayout->addWidget(myTextEdit);
			myTextEdit->setText(data);
			myTextEdit->setReadOnly(true);
			
			QPushButton *myButton = new QPushButton("OK",myDialog);
			vlayout->addWidget(myButton);
			
			//Clicking on OK will close the dialog
			QObject::connect(myButton,SIGNAL(clicked()), myDialog, SLOT(accept()));
			
			
			//Exec dialog
			myDialog->exec();
			
			delete myDialog;
			
			free(data);
			
			
		}	
	
	}
	
	void QtDocument::onAddNoteDocument()
	{
		//Get active view
		int index = m_tabWidget->currentIndex();
		
		cerr<<"Current index  :"<<index<<endl;
		
		if (index >= 0)
		{
			QString netName = m_tabWidget->tabText(index);
			if (m_uiDoc)
			{
				UINetwork *net = m_uiDoc->getNetworkNamed(netName.toStdString());
				cerr<<"network named "<<netName.toStdString()<<endl;
				if (net)
				{
					QtNetwork *qtNet = m_networkMap[net];
					
					if (qtNet)
					{
						
						QRectF sceneRect = qtNet->sceneRect();
						
						double x = sceneRect.topLeft().x();
						double y = sceneRect.topLeft().y();
						
						cerr<<"Creating note"<<endl;
						UINote* note = net->newNote("New Note","Type description here",x,y,true);
						net->addNote(note);
					}
				}
			}
		}
	}
	
	void QtDocument::onDocumentProperties()
	{
		QtDocumentParameters myParams(m_uiDoc);
		
		myParams.exec();
	}
	
	QtNetwork* QtDocument::addNetwork(UINetwork* net)
	{		
        QtNetwork *qtnet = new QtNetwork(this,net);
		//qtnet->fitInView(qtnet->scene()->itemsBoundingRect(),Qt::KeepAspectRatio);
		qtnet->centerOn(qtnet->scene()->itemsBoundingRect().topLeft());
		
		m_networkMap.insert(make_pair(net,qtnet));
		
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
    	
    	setWindowTitle(name.c_str());
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
