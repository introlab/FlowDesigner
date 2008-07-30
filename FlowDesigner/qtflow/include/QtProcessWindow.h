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
        
        private:
        	static const int DEFAULT_WAIT_TIME_MS = 3000; //3 sec
        
        protected: 

       		void closeEvent(QCloseEvent *event);

        public:
        	
	    	QtProcessWindow(QtFlowDesigner *parent, UIDocument *doc);
	    	QtProcessWindow(QtFlowDesigner *parent, QString host, int port);
	    	~QtProcessWindow();
	    	void start(char *mem, int size);
	    	QString getProcessHost() const {return m_processHost;}
	        int getProcessPort() const {return m_processPort;}
    	
        public slots:
        
        	//Window slots
	        void probesButtonClicked();
	       	void stopButtonClicked();
	       	
	       	//Process slots
	        void error ( QProcess::ProcessError error );
	        void finished ( int exitCode, QProcess::ExitStatus exitStatus );
	        void readyReadStandardError ();
	        void readyReadStandardOutput ();
	        void started ();
	        void stateChanged ( QProcess::ProcessState newState );
	        
	        //Socket
	        void remoteDisconnected();
	        
	        //Network Added
	        void notifyNetworkAdded(const UIDocument *doc, const UINetwork* net);
	        
	        void linkProbed(int linkId, const QString &probeType);

        protected:
        	
	    	QtFlowDesigner *m_flowdesigner;
	    	QProcess *m_process;
	    	QTextEdit *m_textBrowser;
	    	QTabWidget *m_tabWidget;
	    	QDockWidget* m_mainOutputDockWidget;
        
        private:
        	void setupUi();
        	QtNetwork* addNetwork(UINetwork* net);
        	
        	UIDocument* m_uiDocView;
        	QString m_processHost;
        	int m_processPort;
               	
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
    
}
#endif /*QTPROCESSWINDOW_H_*/

