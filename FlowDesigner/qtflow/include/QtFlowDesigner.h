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
		void renameNetworkClicked();
        void newIteratorNetworkClicked();
        void newStderrOutput(const char * s, std::streamsize n );
        void newStdoutOutput(const char * s, std::streamsize n );

        //FILE Menu actions
        void newDocumentClicked();
        void openDocumentClicked();
        bool saveDocumentClicked();
        bool saveAsDocumentClicked();
        void onInfoFlowDesignerClicked();
        void onAuthorsFlowDesignerClicked();

        /**
         * Called when the action "Connect to a document"
         * from the "File" menu is selected. This method
         * opens a QtConectDialog.
         * @author Mathieu Labbe
         */
        void connectDocumentClicked();

        protected:

        /**
         * Called before the application is closed. The event can
         * be accepted or rejected. FlowDesigner will try to close
         * all documents and process windows. If one window can't
         * be closed (a user cancelled the closing of a document),
         * the event is rejected and the application doesn't close.
         * @author Mathieu Labbe
         * @param event the close event
         */
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
		QAction *actionRenameNetwork;
        QAction *actionNewIteratorNetwork;

        QAction *actionFlowDesignerInfo;
        QAction *actionFlowDesignerAuthors;

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

