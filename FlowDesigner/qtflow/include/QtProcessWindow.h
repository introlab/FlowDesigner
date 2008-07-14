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
	
    class QtProcessWindow : public QMainWindow, public UIDocument::UIDocumentObserverIF
    {         
        Q_OBJECT;
        
        protected: 

       		void closeEvent(QCloseEvent *event);

        public:
        	
	    	QtProcessWindow(QtFlowDesigner *parent, UIDocument *doc);
	    	~QtProcessWindow();
	    	void start();
	        int getProcessPort() {return 2938;}
    	
        public slots:
        
	        void probesButtonClicked();
	       	void stopButtonClicked();
	        void error ( QProcess::ProcessError error );
	        void finished ( int exitCode, QProcess::ExitStatus exitStatus );
	        void readyReadStandardError ();
	        void readyReadStandardOutput ();
	        void started ();
	        void stateChanged ( QProcess::ProcessState newState );
	        void linkProbed(int linkId);
	        
	        //Network Added
	        void notifyNetworkAdded(const UIDocument *doc, const UINetwork* net);

        protected:
        	
	    	QtFlowDesigner *m_flowdesigner;
	    	UIDocument *m_document;
	    	QProcess *m_process;
	    	QTextEdit *m_textBrowser;
	    	QTabWidget *m_tabWidget;
	    	QDockWidget* m_mainOutputDockWidget;
        
        private:
        	void setupUi(QMainWindow *MainWindow);
        	QtNetwork* addNetwork(UINetwork* net);
        	UIDocument* m_uiDocView;
               	
    };
    
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
	
	class QtProbeConsole : public QDockWidget
	{
		Q_OBJECT;
		
	public:
		
		QtProbeConsole(QtProcessWindow *parent, int linkId);
		~QtProbeConsole();
		
	public slots:
	
	 	void connected();
	 	void readyRead ();
	protected:
		
		QtProcessWindow *m_processWindow;
		QTcpSocket *m_socket;
		QTextBrowser *m_textBrowser;
		int m_linkId;
		
	};
    
}
#endif /*QTPROCESSWINDOW_H_*/

