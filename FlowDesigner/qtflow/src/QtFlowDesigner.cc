/***********************************************************************************
** Copyright (C) 2006-2008 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca).
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
#include "QtFlowDesigner.h"
#include "QtNetwork.h"
#include "QtNodeTreeView.h"
#include <QFileDialog>
#include <QMessageBox>
#include "UINetwork.h"
#include <iostream>
#include "QtProcessWindow.h"
#include "QtDLManager.h"
#include "QtProbeRegistry.h"

namespace FD
{

    using namespace std;

    QtFlowDesigner::QtFlowDesigner (QWidget* parent)
    : QMainWindow(parent)
    {
        setupUi();

        connect(actionNewNetwork, SIGNAL(triggered()),this,SLOT(newNetworkClicked()));
		connect(actionRenameNetwork, SIGNAL(triggered()),SLOT(renameNetworkClicked()));
        connect(actionNewIteratorNetwork, SIGNAL(triggered()),this,SLOT(newIteratorNetworkClicked()));
        connect(actionNew_Document, SIGNAL(triggered()), this, SLOT(newDocumentClicked()));
        connect(actionLoad_Document, SIGNAL(triggered()),this,SLOT(openDocumentClicked()));
		connect(actionSave_Document, SIGNAL(triggered()),this,SLOT(saveDocumentClicked()));
		connect(actionSaveAs_Document, SIGNAL(triggered()),this,SLOT(saveAsDocumentClicked()));
		connect(actionConnect_Document, SIGNAL(triggered()), this, SLOT(connectDocumentClicked()));
		connect(actionFlowDesignerInfo, SIGNAL(triggered()), this, SLOT(onInfoFlowDesignerClicked()));
		connect(actionFlowDesignerAuthors,SIGNAL(triggered()), this, SLOT(onAuthorsFlowDesignerClicked()));
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
        actionConnect_Document = new QAction(this);
        actionConnect_Document->setObjectName(QString::fromUtf8("actionConnect_Document"));


        actionFlowDesignerInfo = new QAction(this);
        actionFlowDesignerInfo->setObjectName(QString::fromUtf8("actionFlowDesigner_Info"));

        actionFlowDesignerAuthors = new QAction(this);
        actionFlowDesignerAuthors->setObjectName(QString::fromUtf8("actionFlowDesigner_Authors"));


        actionNewNetwork = new QAction(this);
        actionNewNetwork->setObjectName(QString::fromUtf8("actionNewNetwork"));
        actionNewIteratorNetwork = new QAction(this);
        actionNewIteratorNetwork->setObjectName(QString::fromUtf8("actionNewIteratorNetwork"));
		
		
		actionRenameNetwork = new QAction(this);
		actionRenameNetwork->setObjectName(QString::fromUtf8("actionRenameNetwork"));



        //Workspace
        m_workspace = new QWorkspace(this);
        m_workspace->setObjectName(QString::fromUtf8("workspace"));

        //vboxMainLayout = new QVBoxLayout(centralwidget);
        //vboxMainLayout->setObjectName(QString::fromUtf8("vboxMainLayout"));
        //vboxMainLayout->setSpacing(6);
        //vboxMainLayout->setMargin(1);



        //hboxLayout = new QHBoxLayout(NULL);
        //hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        //hboxLayout->setSpacing(6);
        //hboxLayout->setMargin(1);
        //vboxMainLayout->addLayout(hboxLayout);





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
        QDockWidget *dock = new QDockWidget(tr("Toolbox"), this);
        dock->setAllowedAreas(Qt::LeftDockWidgetArea | Qt::RightDockWidgetArea);
        dock->setFeatures(QDockWidget::DockWidgetMovable | QDockWidget::DockWidgetFloatable);

        treeView = new QtNodeTreeView(this);
        treeView->setObjectName(QString::fromUtf8("treeView"));

        dock->setWidget(treeView);
        addDockWidget(Qt::LeftDockWidgetArea, dock);




        //QWidget*window = m_workspace->addWindow(treeView);


        //SETUP IO REDIRECTORS

        /*
        QMainWindow *coutWindow = new QMainWindow(this);
        coutWindow->setWindowTitle("Standard output");
        coutWindow->resize(640,480);
        m_coutTextEdit = new QTextEdit(coutWindow);
        coutWindow->setCentralWidget(m_coutTextEdit);
        m_workspace->addWindow(coutWindow);

        QMainWindow *cerrWindow = new QMainWindow(this);
        cerrWindow->setWindowTitle("Standard error");
        cerrWindow->resize(640,480);
        m_cerrTextEdit = new QTextEdit(cerrWindow);
        cerrWindow->setCentralWidget(m_cerrTextEdit);
        m_workspace->addWindow(cerrWindow);
        */
        m_cerrTextEdit = NULL;
        m_coutTextEdit = NULL;

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
        menuAbout = new QMenu(menubar);
        menuAbout->setObjectName(QString::fromUtf8("menuAbout"));
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
        menubar->addAction(menuAbout->menuAction());

        menuFile->addAction(actionNew_Document);
        menuFile->addAction(actionLoad_Document);
		menuFile->addAction(actionSave_Document);
		menuFile->addAction(actionSaveAs_Document);
		menuFile->addSeparator();
		menuFile->addAction(actionConnect_Document);



		menuNetwork->addAction(actionNewNetwork);
        menuNetwork->addAction(actionNewIteratorNetwork);
        menuNetwork->addSeparator();
		menuNetwork->addAction(actionRenameNetwork);

        menuAbout->addAction(actionFlowDesignerInfo);
        menuAbout->addAction(actionFlowDesignerAuthors);

        retranslateUi();

        QSize size(1024, 768);
        size = size.expandedTo(this->minimumSizeHint());
        resize(size);

        //Create the Connect to a document dialog
        m_connectDialog = new QtConnectDialog(this);

        //QMetaObject::connectSlotsByName(this);
    }



    void QtFlowDesigner::retranslateUi()
    {
        setWindowTitle(QApplication::translate("QtFlowDesigner", "FlowDesigner", 0, QApplication::UnicodeUTF8));
        actionNew_Document->setText(QApplication::translate("QtFlowDesigner", "New Document", 0, QApplication::UnicodeUTF8));
        actionLoad_Document->setText(QApplication::translate("QtFlowDesigner", "Load Document", 0, QApplication::UnicodeUTF8));
		actionSave_Document->setText(QApplication::translate("QtFlowDesigner", "Save Document", 0, QApplication::UnicodeUTF8));
		actionSaveAs_Document->setText(QApplication::translate("QtFlowDesigner", "Save Document As", 0, QApplication::UnicodeUTF8));
		actionConnect_Document->setText(QApplication::translate("QtFlowDesigner", "Connect to a remote process", 0, QApplication::UnicodeUTF8));
		actionNewNetwork->setText(QApplication::translate("QtFlowDesigner", "newNetwork", 0, QApplication::UnicodeUTF8));
		actionRenameNetwork->setText(QApplication::translate("QtFlowDesigner", "renameNetwork", 0, QApplication::UnicodeUTF8));
        actionNewIteratorNetwork->setText(QApplication::translate("QtFlowDesigner", "newIteratorNetwork", 0, QApplication::UnicodeUTF8));
        //tabWidget->setTabText(tabWidget->indexOf(tab), QApplication::translate("QtFlowDesigner", "Tab 1", 0, QApplication::UnicodeUTF8));
        //tabWidget->setTabText(tabWidget->indexOf(tab_2), QApplication::translate("QtFlowDesigner", "Tab 2", 0, QApplication::UnicodeUTF8));
        menuAbout->setTitle(QApplication::translate("QtFlowDesigner", "&About", 0, QApplication::UnicodeUTF8));
        menuFile->setTitle(QApplication::translate("QtFlowDesigner", "&File", 0, QApplication::UnicodeUTF8));
        menuPreferences->setTitle(QApplication::translate("QtFlowDesigner", "&Preferences", 0, QApplication::UnicodeUTF8));
        menuNetwork->setTitle(QApplication::translate("QtFlowDesigner", "Network", 0, QApplication::UnicodeUTF8));
        actionFlowDesignerInfo->setText(QApplication::translate("QtFlowDesigner", "FlowDesigner Info", 0, QApplication::UnicodeUTF8));
        actionFlowDesignerAuthors->setText(QApplication::translate("QtFlowDesigner", "FlowDesigner Authors", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

    void QtFlowDesigner::newProcess(UIDocument *doc)
    {
    	QtProcessWindow *processWindow = new QtProcessWindow(this,doc);
    	processWindow->show();
    }

    void QtFlowDesigner::newDocumentClicked()
    {
    	QtDocument *doc = new QtDocument(this,"Untitled");

    	//ADD MAIN NETWORK
    	doc->addNetwork("MAIN", UINetwork::subnet);

        QWidget *window = m_workspace->addWindow(doc);
        m_workspace->setActiveWindow(window);
        window->showMaximized();
    }

    bool QtFlowDesigner::saveAsDocumentClicked()
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
				return true; // save accepted
			}
		}
		return false; //save cancelled
    }

    void QtFlowDesigner::connectDocumentClicked()
    {
    	if(m_connectDialog && m_connectDialog->exec())
    	{
    		QString host = m_connectDialog->getHost();
    		int port = m_connectDialog->getPort();
    		QtProcessWindow *processWindow = new QtProcessWindow(this, host, port);
    		processWindow->show();
    	}
	}

	
	void QtFlowDesigner::renameNetworkClicked()
    {
		QtDocument* activeDoc = activeDocument();
		
		if (activeDoc)
		{
			activeDoc->onNetworkNameChange();
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


    bool QtFlowDesigner::saveDocumentClicked()
    {
        QtDocument *activeDocument = dynamic_cast<QtDocument*>(m_workspace->activeWindow());

        if (activeDocument)
        {
            cerr<<"ACTIVE DOCUMENT FOUND"<<endl;
            QFileDialog dialog(this);
            dialog.setFileMode(QFileDialog::AnyFile);

			dialog.setDirectory(activeDocument->getFullPath());
			dialog.selectFile(activeDocument->getDocName());

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
                return true; // save accepted
            }
        }
        else
        {
            cerr<<"ACTIVE DOCUMENT NOT FOUND"<<endl;
		}
		return false; // save cancelled


    }

    void QtFlowDesigner::loadDocument(const std::string &path)
    {
        QtDocument *doc =  new QtDocument(this,path);
        doc->open(path);
        QWidget *window = m_workspace->addWindow(doc);
        m_workspace->setActiveWindow(window);
        window->show();
    }

    void QtFlowDesigner::onAuthorsFlowDesignerClicked()
    {
		QDialog *myDialog = new QDialog(this);
		QVBoxLayout *vlayout = new QVBoxLayout(myDialog);
		myDialog->resize(640,480);


		//Create text edit
		QTextEdit *myTextEdit = new QTextEdit(myDialog);

		vlayout->addWidget(myTextEdit);


		myTextEdit->append(QString("<b>Lead Developpers :</b> "));
		myTextEdit->append(QString("Dominic Letourneau (maestro@users.sourceforge.net)"));
		myTextEdit->append(QString("Jean-Marc Valin (jmvalin@users.sourceforge.net)"));

		myTextEdit->append(QString("<br><b>Contributors :</b> "));
		myTextEdit->append(QString("Francois Michaud"));
		myTextEdit->append(QString("Julien D'Ascenzio"));
		myTextEdit->append(QString("Mathieu Labbe"));
		myTextEdit->append(QString("Carle Cote"));
		myTextEdit->append(QString("Pierre Lepage"));
		myTextEdit->append(QString("Mathieu Lemay"));
		myTextEdit->append(QString("Victor Bao-Long Tran"));
		myTextEdit->append(QString("Yannick Brosseau"));
		myTextEdit->append(QString("Etienne Robichaud"));
		myTextEdit->append(QString("Pierre Moisan"));
		myTextEdit->append(QString("Nynon Gagne"));
		myTextEdit->append(QString("Brad Chapman"));
		myTextEdit->append(QString("Andre Charbonneau"));

		myTextEdit->setReadOnly(true);

		QPushButton *myButton = new QPushButton("Thank you!",myDialog);
		vlayout->addWidget(myButton);

		//Clicking on OK will close the dialog
		QObject::connect(myButton,SIGNAL(clicked()), myDialog, SLOT(accept()));


		//Exec dialog
		myDialog->exec();

		delete myDialog;
    }

    void QtFlowDesigner::onInfoFlowDesignerClicked()
    {
		QDialog *myDialog = new QDialog(this);
		QVBoxLayout *vlayout = new QVBoxLayout(myDialog);
		myDialog->resize(640,480);


		//Create text edit
		QTextEdit *myTextEdit = new QTextEdit(myDialog);

		vlayout->addWidget(myTextEdit);

		//Append FlowDesigner information
        myTextEdit->append(QString("<b> FlowDesigner ") + QString(FLOWDESIGNER_VERSION) +
        QString(" by Jean-Marc Valin & Dominic Letourneau <br>") +
        QString("(C) Copyright 1999-2008 <b><br>") +
        QString("<a href='http://flowdesigner.sourceforge.net'>http://flowdesigner.sourceforge.net.</a><br>----<br>"));






		//Append libraries loaded
		myTextEdit->append("<b>Loaded Toolboxes :</b>");
		std::list<QLibrary*> &libsList = QtDLManager::getLoadedLibraries();

		for (std::list<QLibrary*>::iterator iter = libsList.begin(); iter != libsList.end(); iter++)
		{
			myTextEdit->append((*iter)->fileName() + QString(" [OK]"));
		}
		libsList =  QtDLManager::getFailedLibraries();
		for (std::list<QLibrary*>::iterator iter = libsList.begin(); iter != libsList.end(); iter++)
		{
			myTextEdit->append((*iter)->fileName() + QString(" [") + (*iter)->errorString() + QString("]"));
		}

		//Get nodes count
		myTextEdit->append("<br>----<br>");
		myTextEdit->append(QString::number(UINodeRepository::Available().size()) + QString(" Nodes available."));

		myTextEdit->append("<br><br><b>Registered Probes :</b>");
		std::map<QString,QtProbeBaseFactory*> &factoryMap =  QtProbeRegistry::getFactoryMap();

		for (std::map<QString,QtProbeBaseFactory*>::iterator iter = factoryMap.begin(); iter != factoryMap.end(); iter++)
		{
			myTextEdit->append(iter->first);
		}


		myTextEdit->append("<br><br><b>Loaded Probe Toolboxes :</b>");

		std::vector<QLibrary*> &loadedProbes = QtProbeRegistry::getLoadedLibraries();

		for (unsigned int i = 0; i < loadedProbes.size(); i++)
		{
			myTextEdit->append(loadedProbes[i]->fileName() + QString(" [OK]"));
		}


		std::vector<QLibrary*> &failedProbes = QtProbeRegistry::getFailedLibraries();

		for (unsigned int i = 0; i < failedProbes.size(); i++)
		{
			myTextEdit->append(failedProbes[i]->fileName() + QString(" [") + failedProbes[i]->errorString() + QString("]"));
		}

		myTextEdit->setReadOnly(true);

		QPushButton *myButton = new QPushButton("OK",myDialog);
		vlayout->addWidget(myButton);

		//Clicking on OK will close the dialog
		QObject::connect(myButton,SIGNAL(clicked()), myDialog, SLOT(accept()));


		//Exec dialog
		myDialog->exec();

		delete myDialog;

    }

    void QtFlowDesigner::newStderrOutput(const char * s, std::streamsize n )
    {
    	if (m_cerrTextEdit)
    		m_cerrTextEdit->append(QString(s));
    }

    void QtFlowDesigner::newStdoutOutput(const char * s, std::streamsize n )
    {
    	if (m_coutTextEdit)
    		m_coutTextEdit->append(QString(s));
    }

	void QtFlowDesigner::closeEvent(QCloseEvent *event)
	{
		// Close process windows
		bool close = true;
		QList<QtProcessWindow *> processWindows = this->findChildren<QtProcessWindow *>();
		for(int i=0; i<processWindows.size(); i++) {
			if(!processWindows[i]->close()) {
				close = false;
				break;
			}
		}

		// Close documents
		if(close) {
			m_workspace->closeAllWindows();
			if (activeDocument()) {
				close = false;
			}
		}

		//write settings before quit?

		event->setAccepted(close);
	}

}//namespace FD
