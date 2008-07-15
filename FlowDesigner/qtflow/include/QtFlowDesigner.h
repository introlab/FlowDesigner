//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#ifndef _QTFLOWDESIGNER_H_
#define _QTFLOWDESIGNER_H_

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QMainWindow>
#include <QtGui/QMenu>
#include <QtGui/QMenuBar>
#include <QtGui/QStatusBar>
#include <QtGui/QTabWidget>
#include <QtGui/QTreeView>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include <QtGui/QTextEdit>
#include <QtGui/QWorkspace>
#include <QtGui/QInputDialog>
#include "UINetwork.h"
#include <string>
#include "QtIORedirector.h"
#include "QtDocument.h"
#include "QtConnectDialog.h"

namespace FD
{
    
    //forward declaration
    class QtNodeTreeView;
    
    class QtFlowDesigner : public QMainWindow
    {
        Q_OBJECT;
        
        public:
        
        QtFlowDesigner (QWidget* parent = NULL);
        virtual ~QtFlowDesigner();
        
        void newNetwork(const std::string name);
        void newProcess(UIDocument *doc);
        void newDocument(const std::string &name);
        void loadDocument(const std::string &path);
        
        QWorkspace* workspace() {return m_workspace;}
        
        public slots:
        void newNetworkClicked();
        void newIteratorNetworkClicked();
        void newStderrOutput(const char * s, std::streamsize n );
        void newStdoutOutput(const char * s, std::streamsize n );
        
        //FILE Menu actions
        void newDocumentClicked();
        void openDocumentClicked();               
        bool saveDocumentClicked();
        bool saveAsDocumentClicked();
        void connectDocumentClicked();
        void onInfoFlowDesignerClicked();

        protected:
        
        virtual void closeEvent ( QCloseEvent * event );

        QtDocument *activeDocument();	
        void setupUi();
        void retranslateUi();
        
        QAction *actionNew_Document;
        QAction *actionLoad_Document;
        QAction *actionSave_Document;
        QAction *actionSaveAs_Document;
        QAction *actionConnect_Document;
        
        
        QAction *actionNewNetwork;
        QAction *actionNewIteratorNetwork;
        
        QAction *actionFlowDesignerInfo;
        
        QWidget *centralwidget;
        QHBoxLayout *hboxLayout;
        QWidget *verticalLayout;
        QVBoxLayout *vboxLayout;
        QVBoxLayout *vboxMainLayout;   
        QWidget *horizontalLayout_2;
        QHBoxLayout *hboxLayout1;
        QTreeView *treeView;
        QTabWidget *tabWidget;
        QWidget *tab;
        QWidget *tab_2;
        QMenuBar *menubar;
        QMenu *menuAbout;
        QMenu *menuFile;
        QMenu *menuPreferences;
        QMenu *menuNetwork;
        QStatusBar *statusbar;
        QtNodeTreeView *m_treeView;
        QWorkspace *m_workspace; 
        
        QtConnectDialog *m_connectDialog;
        
        QTextEdit *m_coutTextEdit;
        QTextEdit *m_cerrTextEdit;
    };
    
}//namespace FD
#endif

