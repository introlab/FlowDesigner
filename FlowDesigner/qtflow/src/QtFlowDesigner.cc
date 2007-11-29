//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca)

#include "QtFlowDesigner.h"
#include "QtNetwork.h"
#include "QtNodeTreeView.h"
#include <QFileDialog>
#include <QMessageBox>
#include "UINetwork.h"
#include <iostream>
#include "QtProcessWindow.h"


namespace FD
{
    
    using namespace std;
    
    QtFlowDesigner::QtFlowDesigner (QWidget* parent)
    : QMainWindow(parent)
    {
        setupUi();
        
        connect(actionNewNetwork, SIGNAL(triggered()),this,SLOT(newNetworkClicked()));
        connect(actionNewIteratorNetwork, SIGNAL(triggered()),this,SLOT(newIteratorNetworkClicked()));
        connect(actionNew_Document, SIGNAL(triggered()), this, SLOT(newDocumentClicked()));
        connect(actionLoad_Document, SIGNAL(triggered()),this,SLOT(openDocumentClicked()));
		connect(actionSave_Document, SIGNAL(triggered()),this,SLOT(saveDocumentClicked()));
		connect(actionSaveAs_Document, SIGNAL(triggered()),this,SLOT(saveAsDocumentClicked()));
    }
    
    QtFlowDesigner::~QtFlowDesigner()
    {
    }
    
    void QtFlowDesigner::setupUi()
    {
        //Action menus
        setObjectName(QString::fromUtf8("QtFlowDesigner"));
        
        actionNew_Document = new QAction(this);
        actionNew_Document->setObjectName(QString::fromUtf8("actionNew_Document"));
        actionLoad_Document = new QAction(this);
        actionLoad_Document->setObjectName(QString::fromUtf8("actionLoad_Document"));
        actionSave_Document = new QAction(this);		
        actionSave_Document->setObjectName(QString::fromUtf8("actionSave_Document"));       
        actionSaveAs_Document = new QAction(this);		
        actionSaveAs_Document->setObjectName(QString::fromUtf8("actionSaveAs_Document"));
        
        
        
        
        
        actionNewNetwork = new QAction(this);
        actionNewNetwork->setObjectName(QString::fromUtf8("actionNewNetwork"));
        actionNewIteratorNetwork = new QAction(this);
        actionNewIteratorNetwork->setObjectName(QString::fromUtf8("actionNewIteratorNetwork"));
        
        
        //Workspace
        m_workspace = new QWorkspace(this);
        m_workspace->setObjectName(QString::fromUtf8("workspace"));
        
        std::cerr<<"workspace created"<<std::endl;
        //vboxMainLayout = new QVBoxLayout(centralwidget);
        //vboxMainLayout->setObjectName(QString::fromUtf8("vboxMainLayout"));
        //vboxMainLayout->setSpacing(6);
        //vboxMainLayout->setMargin(1);
        
        
        
        //hboxLayout = new QHBoxLayout(NULL);
        //hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        //hboxLayout->setSpacing(6);
        //hboxLayout->setMargin(1);
        //vboxMainLayout->addLayout(hboxLayout);
        
        
        
        //Text edit
        m_textEdit = new QTextEdit(this);
        m_textEdit->setLineWrapMode (QTextEdit::NoWrap);
        m_textEdit->setReadOnly(true);
        m_textEdit->setText(QString("<b> FlowDesigner ") + QString("FLOWDESIGNER_VERSION") +
        QString(" by Jean-Marc Valin & Dominic Letourneau <br>") +
        QString("(C) Copyright 1999-2007 <b><br>----<br>"));
        QWidget *window = m_workspace->addWindow(m_textEdit);
        window->show();
        
        /*
        verticalLayout = new QWidget(centralwidget);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        QSizePolicy sizePolicy(static_cast<QSizePolicy::Policy>(7), static_cast<QSizePolicy::Policy>(7));
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(verticalLayout->sizePolicy().hasHeightForWidth());
        verticalLayout->setSizePolicy(sizePolicy);
        vboxLayout = new QVBoxLayout(verticalLayout);
        vboxLayout->setSpacing(6);
        vboxLayout->setMargin(9);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        horizontalLayout_2 = new QWidget(verticalLayout);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        hboxLayout1 = new QHBoxLayout(horizontalLayout_2);
        hboxLayout1->setSpacing(6);
        hboxLayout1->setMargin(0);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        */
        
        //tree view
        treeView = new QtNodeTreeView(this);
        treeView->setObjectName(QString::fromUtf8("treeView"));
        m_workspace->addWindow(treeView);
        //hboxLayout->addWidget(treeView);
        
        //tab widget
        //tabWidget = new QTabWidget(this);
        //tabWidget->setObjectName(QString::fromUtf8("tabWidget"));
        //tab = new QWidget();
        //tab->setObjectName(QString::fromUtf8("tab"));
        //tabWidget->addTab(tab, QApplication::translate("this", "Tab 1", 0, QApplication::UnicodeUTF8));
        //tab_2 = new QWidget();
        //tab_2->setObjectName(QString::fromUtf8("tab_2"));
        //tabWidget->addTab(tab_2, QApplication::translate("this", "Tab 2", 0, QApplication::UnicodeUTF8));
        
        //hboxLayout->addWidget(tabWidget);
        
        
        //vboxLayout->addWidget(horizontalLayout_2);
        
        
        //hboxLayout->addWidget(verticalLayout);
        
        this->setCentralWidget(m_workspace);
        
        
        
        
        
        
        
        //SETUP MENUS
        
        menubar = new QMenuBar(this);
        menubar->setObjectName(QString::fromUtf8("menubar"));
        menubar->setGeometry(QRect(0, 0, 800, 26));
        menu_About = new QMenu(menubar);
        menu_About->setObjectName(QString::fromUtf8("menu_About"));
        menuFile = new QMenu(menubar);
        menuFile->setObjectName(QString::fromUtf8("menuFile"));
        menuPreferences = new QMenu(menubar);
        menuPreferences->setObjectName(QString::fromUtf8("menuPreferences"));
        menuNetwork = new QMenu(menubar);
        menuNetwork->setObjectName(QString::fromUtf8("menuNetwork"));
        this->setMenuBar(menubar);
        statusbar = new QStatusBar(this);
        statusbar->setObjectName(QString::fromUtf8("statusbar"));
        this->setStatusBar(statusbar);
        
        menubar->addAction(menuFile->menuAction());
        menubar->addAction(menuNetwork->menuAction());
        menubar->addAction(menuPreferences->menuAction());
        menubar->addAction(menu_About->menuAction());
        
        menuFile->addAction(actionNew_Document);
        menuFile->addAction(actionLoad_Document);
		menuFile->addAction(actionSave_Document);
		menuFile->addAction(actionSaveAs_Document);
		
		menuNetwork->addAction(actionNewNetwork);      
        menuNetwork->addAction(actionNewIteratorNetwork);
        //menuNetwork->addSeparator();

        retranslateUi();
        
        QSize size(1024, 768);
        size = size.expandedTo(this->minimumSizeHint());
        resize(size);
        
        QMetaObject::connectSlotsByName(this);   
    }
    

    
    void QtFlowDesigner::retranslateUi()
    {
        setWindowTitle(QApplication::translate("QtFlowDesigner", "FlowDesigner", 0, QApplication::UnicodeUTF8));  
        actionNew_Document->setText(QApplication::translate("QtFlowDesigner", "New Document", 0, QApplication::UnicodeUTF8));
        actionLoad_Document->setText(QApplication::translate("QtFlowDesigner", "Load Document", 0, QApplication::UnicodeUTF8));
		actionSave_Document->setText(QApplication::translate("QtFlowDesigner", "Save Document", 0, QApplication::UnicodeUTF8));
		actionSaveAs_Document->setText(QApplication::translate("QtFlowDesigner", "Save Document As", 0, QApplication::UnicodeUTF8));
		actionNewNetwork->setText(QApplication::translate("QtFlowDesigner", "newNetwork", 0, QApplication::UnicodeUTF8));
        actionNewIteratorNetwork->setText(QApplication::translate("QtFlowDesigner", "newIteratorNetwork", 0, QApplication::UnicodeUTF8));
        //tabWidget->setTabText(tabWidget->indexOf(tab), QApplication::translate("QtFlowDesigner", "Tab 1", 0, QApplication::UnicodeUTF8));
        //tabWidget->setTabText(tabWidget->indexOf(tab_2), QApplication::translate("QtFlowDesigner", "Tab 2", 0, QApplication::UnicodeUTF8));
        menu_About->setTitle(QApplication::translate("QtFlowDesigner", "&About", 0, QApplication::UnicodeUTF8));
        menuFile->setTitle(QApplication::translate("QtFlowDesigner", "&File", 0, QApplication::UnicodeUTF8));
        menuPreferences->setTitle(QApplication::translate("QtFlowDesigner", "&Preferences", 0, QApplication::UnicodeUTF8));
        menuNetwork->setTitle(QApplication::translate("QtFlowDesigner", "Network", 0, QApplication::UnicodeUTF8));
        
    } // retranslateUi
     
    void QtFlowDesigner::newProcess(UIDocument *doc)
    {
    	QtProcessWindow *processWindow = new QtProcessWindow(this,doc);
    	QWidget *window = m_workspace->addWindow(processWindow);
    	window->setWindowTitle("Process");
    	m_workspace->setActiveWindow(window);
    	window->show();
    }
    
    void QtFlowDesigner::newDocumentClicked()
    {
    	QtDocument *doc = new QtDocument(this,"Untitled");
    	
    	//ADD MAIN NETWORK
    	doc->addNetwork("MAIN", UINetwork::subnet);
    	
        QWidget *window = m_workspace->addWindow(doc);
        window->setWindowTitle("QtDocument - Untitled");
        m_workspace->setActiveWindow(window);
        window->show();	
    }
    
    void QtFlowDesigner::saveAsDocumentClicked()
    {	
		QtDocument *activeDocument =
				dynamic_cast<QtDocument*>(m_workspace->activeWindow());
	
		if (activeDocument) 
		{
			QFileDialog dialog(this);
			dialog.setFileMode(QFileDialog::AnyFile);
	
			QStringList filters;
			filters << "FlowDesigner Document (*.n)" << "Any files (*)";
	
			dialog.setFilters(filters);
			dialog.setAcceptMode(QFileDialog::AcceptSave);
			if (dialog.exec()) 
			{
				QStringList fileNames = dialog.selectedFiles();
	
				for (QStringList::iterator iter = fileNames.begin(); iter
						!= fileNames.end(); iter++) 
				{
					activeDocument->save((*iter).toStdString());
				}
			}
		} 
    }
    
    
    void QtFlowDesigner::newNetworkClicked()
    {
    	
        QtDocument* activeDoc = activeDocument();
        
        if(activeDoc)
        {
            bool ok;
            QString name = QInputDialog::getText ( NULL,QString ( "New network" ),
            QString ( "Network name : " ),QLineEdit::Normal, "subnet",&ok ); 
            if(name=="")      
            {
                QMessageBox::critical( this
                , tr("New Network ERROR")
                , tr("Name NULL"));
                return;
            }
            if(activeDoc->isNetworkExist(name))
            {
                QMessageBox::critical( this
                , tr("New Network ERROR")
                , tr("This name is already used"));
                return;
            }
            activeDoc->addNetwork(name, UINetwork::subnet);
        }  
    }
    
    
    
    void QtFlowDesigner::newIteratorNetworkClicked()
    { 	
        QtDocument* activeDoc = activeDocument();
        
        if(activeDoc)
        {
            bool ok;
            QString name = QInputDialog::getText ( NULL,QString ( "New iterator network" ),
            QString ( "Iterator name : " ),QLineEdit::Normal, "loop",&ok ); 
            if(name=="")      
            {
                QMessageBox::critical( this
                , tr("New Iterator ERROR")
                , tr("Name NULL"));
                return;
            }
            else if(activeDoc->isNetworkExist(name))
            {
                QMessageBox::critical( this
                , tr("New Iterator ERROR")
                , tr("This name is already used"));
                return;
            }
            activeDoc->addNetwork(name, UINetwork::iterator);
        }
    }
    
    QtDocument *QtFlowDesigner::activeDocument()
    {
        QWidget *window =  m_workspace->activeWindow ();      
        QtDocument *doc = dynamic_cast<QtDocument*>(window);
        return doc;    
    }
    
    void QtFlowDesigner::openDocumentClicked()
    {
        cerr<<"Open document clicked"<<endl;
        QFileDialog dialog(this);
        dialog.setFileMode(QFileDialog::ExistingFiles);
        
        QStringList filters;
        filters << "FlowDesigner Document (*.n)"
        << "Any files (*)";
        
        dialog.setFilters(filters);      
        
        if (dialog.exec())
        {
            QStringList fileNames = dialog.selectedFiles();
            
            for (QStringList::iterator iter = fileNames.begin();
            iter != fileNames.end(); iter++)
            {
                loadDocument((*iter).toStdString());                                 
            }
            m_workspace->tile();
        }         
    }
    
    
    void QtFlowDesigner::saveDocumentClicked()
    {
        cerr<<"Save document clicked"<<endl;
        
        QtDocument *activeDocument = dynamic_cast<QtDocument*>(m_workspace->activeWindow());
        
        if (activeDocument)
        {
            cerr<<"ACTIVE DOCUMENT FOUND"<<endl;
            QFileDialog dialog(this);
            dialog.setFileMode(QFileDialog::AnyFile);
            
            QStringList filters;
            filters << "FlowDesigner Document (*.n)"
            << "Any files (*)";
            
            dialog.setFilters(filters); 
            dialog.setAcceptMode(QFileDialog::AcceptSave);
            if (dialog.exec())
            {
                QStringList fileNames = dialog.selectedFiles();
                
                for (QStringList::iterator iter = fileNames.begin();
                iter != fileNames.end(); iter++)
                {
                    activeDocument->save((*iter).toStdString());                                 
                }
            }
        }
        else
        {
            cerr<<"ACTIVE DOCUMENT NOT FOUND"<<endl;
		}
        
        
    }
    
    void QtFlowDesigner::loadDocument(const std::string &path)
    {
        QtDocument *doc =  new QtDocument(this,path);
        doc->open(path);
        QWidget *window = m_workspace->addWindow(doc);
        window->setWindowTitle(path.c_str());
        m_workspace->setActiveWindow(window);
        window->show();              
    }      
    
}//namespace FD
