/***********************************************************************************
** Copyright (C) 2006-2008 Laborius (http://www.gel.usherbrooke.ca/laborius/). 
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
#ifndef _QTPROCESSWINDOW_H_
#define _QTPROCESSWINDOW_H_

#include <QtCore/QProcess>
#include <QtNetwork/QTcpSocket>
#include <QtGui/QGridLayout>
#include <QtGui/QVBoxLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QPushButton>
#include <QtGui/QCloseEvent>
#include <QtGui/QAction>
#include <QtGui/QDockWidget>
#include <QtGui/QGroupBox>
#include <QtGui/QMainWindow>
#include <QtGui/QTabWidget>
#include <QtGui/QTextBrowser>
#include <QtGui/QWidget>

#include "QtNetwork.h"
#include "QtDocument.h"

namespace FD
{
    
	//forward declarations
	class QtFlowDesigner;
	class QtProbeConsole;
	
	/**
	 * A process window is used to check the status of local 
	 * and remote processes. It supports live probing on links of 
	 * the network.
	 * @author Mathieu Labbe
	 */
    class QtProcessWindow : public QMainWindow, public UIDocument::UIDocumentObserverIF
    {         
        Q_OBJECT;
        
        private:
        	/**
        	 * Max time to wait when trying to connect to a server.
        	 */
        	static const int DEFAULT_WAIT_TIME_MS = 3000; //3 sec
        
        public:
        	/**
        	 * The constructor when binded to a local process.
        	 * @param parent the QtFlowDesigner reference
        	 * @param doc the document to run
        	 */
	    	QtProcessWindow(QtFlowDesigner *parent, UIDocument *doc);
	    	
	    	/**
        	 * The constructor when binded to a remote process.
        	 * @param parent the QtFlowDesigner reference
        	 * @param host the host of the computer of the remote process
        	 * @param port the port of the remote process
        	 */
	    	QtProcessWindow(QtFlowDesigner *parent, QString host, int port);
	    	
	    	/*
	    	 * The destructor
	    	 */
	    	~QtProcessWindow();
	    	
	    	/**
	    	 * @return the process port
	    	 */
	    	QString getProcessHost() const {return m_processHost;}
	    	
	    	/**
	    	 * return the process port
	    	 */
	        int getProcessPort() const {return m_processPort;}
		
		protected: 
			/**
	         * Called before the process window is closed. The event can 
	         * be accepted or rejected. This method is used 
	         * to ask to user if a process is running to stop it.
	         * @param event the close event
	         */
       		void closeEvent(QCloseEvent *event);
       		
       	private slots:
       		///////////////////
       		/// Window slots
       		///////////////////
       		/**
       		 * Called when the probes button has been clicked.
       		 */
	        void probesButtonClicked();
	        
	        /**
       		 * Called when the stop button has been clicked.
       		 */
	       	void stopButtonClicked();
	       	
	       	///////////////////
	       	/// Process slots
	       	///////////////////
	       	/**
       		 * Called when an error occurs in the local QtFlow process.
       		 * @param error the process error
       		 */
	        void error(QProcess::ProcessError error);
	        
	        /**
       		 * Called when the local QtFlow process is finished.
       		 * @param exitCode the exit code
       		 * @param exitStatus the exit status
       		 */
	        void finished (int exitCode, QProcess::ExitStatus exitStatus);
	        
	        /**
       		 * Called when the error output "cerr" of the local QtFlow process occurs.
       		 */
	        void readyReadStandardError();
	        
	        /**
       		 * Called when the standard output "cout" of the local QtFlow process occurs.
       		 */
	        void readyReadStandardOutput();
	        
	        /**
       		 * Called when the local QtFlow process is started.
       		 */
	        void started();
	        
	        /**
       		 * Called when the process state has changed.
       		 * @param newState the new process state
       		 */
	        void stateChanged(QProcess::ProcessState newState);
	        
	        ///////////////////
	        /// Socket slots
	        ///////////////////
	        /**
       		 * Called when the connection to the remote 
       		 * process is lost.
       		 */
	        void remoteDisconnected();
	        
	        ///////////////////
	        /// Other
	        ///////////////////
	        /**
       		 * Called when a UINetwork is added. It creates a QtNetwork.
       		 * @param doc the UIDocument of the UINetwork
       		 * @param net the UINetwork added
       		 */
	        void notifyNetworkAdded(const UIDocument *doc, const UINetwork* net);
	        
	        /**
	    	 * Called when a link is probed.
	    	 * @param linkId the link id
	    	 * @param probeType the probe type
	    	 */
	        void linkProbed(int linkId, const QString &probeType);
       		
		private:
			/**
			 * Setup the interface of the window.
			 */
		    void setupUi();
		    
		    /**
		     * Add a network.
		     * @param net the UINetwork to add.
		     * @return a Qt representation (QtNetwork) of the UINetwork.
		     */
        	QtNetwork* addNetwork(UINetwork* net);	
			
			/**
	    	 * Start a QtFlow process with a UIDocument.
	    	 * @param mem the memory representation of a UIDocument
	    	 * @param size the size of the memory
	    	 */
	    	void start(char *mem, int size);

        protected:
        	
	    	QtFlowDesigner *m_flowdesigner;
	    	QProcess *m_process;
	    	QTextEdit *m_textBrowser;
	    	QTabWidget *m_tabWidget;
	    	QDockWidget* m_mainOutputDockWidget;
        
        private:        	
        	UIDocument* m_uiDocView;
        	QString m_processHost;
        	int m_processPort;
               	
    };
    
    /**
     * A dialog to see the list of available probing link.
     */
	class QtProbesDialog : public QDialog
	{
		Q_OBJECT;
		
	public:
		
		QtProbesDialog(QtProcessWindow *parent);
		~QtProbesDialog();
		
	public slots:
	
	 	void connected();
	 	void readyRead ();
	protected:
		
		QtProcessWindow *m_processWindow;
		QTcpSocket *m_socket;
		QTextEdit *m_textEdit;
		QVBoxLayout *m_mainLayout;
		
	};
    
}
#endif /*QTPROCESSWINDOW_H_*/

