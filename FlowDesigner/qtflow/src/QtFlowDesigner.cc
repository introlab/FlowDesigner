//Copyright (C) 2006 Dominic Letourneau (Dominic.Letourneau@USherbrooke.ca) 

#include "QtFlowDesigner.h"
#include "QtNetwork.h"

namespace FD
{

QtFlowDesigner::QtFlowDesigner (QWidget* parent)
    : QMainWindow(parent)
{
    setupUi();
     //setupUi(this);
    //setupUi(this);
    
    connect(actionNewNetwork, SIGNAL(triggered()),this,SLOT(newNetworkClicked()));
}   
      
QtFlowDesigner::~QtFlowDesigner()
{
          
}

void QtFlowDesigner::setupUi()
{
    setObjectName(QString::fromUtf8("QtFlowDesigner"));
    actionLoad_Document = new QAction(this);
    actionLoad_Document->setObjectName(QString::fromUtf8("actionLoad_Document"));
    actionNewNetwork = new QAction(this);
    actionNewNetwork->setObjectName(QString::fromUtf8("actionNewNetwork"));
    centralwidget = new QWidget(this);
    centralwidget->setObjectName(QString::fromUtf8("centralwidget"));
    hboxLayout = new QHBoxLayout(centralwidget);
    hboxLayout->setSpacing(6);
    hboxLayout->setMargin(9);
    hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
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
    treeView = new QTreeView(horizontalLayout_2);
    treeView->setObjectName(QString::fromUtf8("treeView"));

    hboxLayout1->addWidget(treeView);

    tabWidget = new QTabWidget(horizontalLayout_2);
    tabWidget->setObjectName(QString::fromUtf8("tabWidget"));
    tab = new QWidget();
    tab->setObjectName(QString::fromUtf8("tab"));
    tabWidget->addTab(tab, QApplication::translate("this", "Tab 1", 0, QApplication::UnicodeUTF8));
    tab_2 = new QWidget();
    tab_2->setObjectName(QString::fromUtf8("tab_2"));
    tabWidget->addTab(tab_2, QApplication::translate("this", "Tab 2", 0, QApplication::UnicodeUTF8));

    hboxLayout1->addWidget(tabWidget);


    vboxLayout->addWidget(horizontalLayout_2);


    hboxLayout->addWidget(verticalLayout);

    this->setCentralWidget(centralwidget);
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
    menuFile->addAction(actionLoad_Document);
    menuNetwork->addAction(actionNewNetwork);
    menuNetwork->addSeparator();

    retranslateUi();

    QSize size(1024, 768);
    size = size.expandedTo(this->minimumSizeHint());
    resize(size);

    QMetaObject::connectSlotsByName(this);
}

void QtFlowDesigner::retranslateUi()
{
    setWindowTitle(QApplication::translate("QtFlowDesigner", "FlowDesigner", 0, QApplication::UnicodeUTF8));
    actionLoad_Document->setText(QApplication::translate("QtFlowDesigner", "Load Document", 0, QApplication::UnicodeUTF8));
    actionNewNetwork->setText(QApplication::translate("QtFlowDesigner", "newNetwork", 0, QApplication::UnicodeUTF8));
    tabWidget->setTabText(tabWidget->indexOf(tab), QApplication::translate("QtFlowDesigner", "Tab 1", 0, QApplication::UnicodeUTF8));
    tabWidget->setTabText(tabWidget->indexOf(tab_2), QApplication::translate("QtFlowDesigner", "Tab 2", 0, QApplication::UnicodeUTF8));
    menu_About->setTitle(QApplication::translate("QtFlowDesigner", "&About", 0, QApplication::UnicodeUTF8));
    menuFile->setTitle(QApplication::translate("QtFlowDesigner", "&File", 0, QApplication::UnicodeUTF8));
    menuPreferences->setTitle(QApplication::translate("QtFlowDesigner", "&Preferences", 0, QApplication::UnicodeUTF8));
    menuNetwork->setTitle(QApplication::translate("QtFlowDesigner", "Network", 0, QApplication::UnicodeUTF8));
  } // retranslateUi

void QtFlowDesigner::newNetwork(const std::string name)
{
       
    tabWidget->addTab (new QtNetwork(), name.c_str());

}

void QtFlowDesigner::newNetworkClicked()
{
    newNetwork("TESTING");         
}

}//namespace FD

