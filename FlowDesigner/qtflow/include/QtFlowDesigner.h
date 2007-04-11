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

#include <string>

namespace FD
{

//forward declaration
class QtNodeTreeView;

class QtFlowDesigner : public QMainWindow
{
    Q_OBJECT
    
    public:
        
    QtFlowDesigner (QWidget* parent = NULL);
      
    virtual ~QtFlowDesigner();
    
    void newNetwork(const std::string name);
    
    
    void newDocument(const std::string &name);
    void loadDocument(const std::string &path);      
    
    public slots:
        void newNetworkClicked();
        void openDocumentClicked();               
		void saveDocumentClicked(); 

	protected:

     void setupUi();
     void setupUiv2();
     void retranslateUi();


    QAction *actionLoad_Document;
	QAction *actionSave_Document;
    QAction *actionNewNetwork;
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
    QMenu *menu_About;
    QMenu *menuFile;
    QMenu *menuPreferences;
    QMenu *menuNetwork;
    QStatusBar *statusbar;
    QtNodeTreeView *m_treeView;
    QTextEdit *m_textEdit;
    QWorkspace *m_workspace;      
};

}//namespace FD
#endif

