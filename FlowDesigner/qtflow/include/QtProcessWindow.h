#ifndef _QTPROCESSWINDOW_H_
#define _QTPROCESSWINDOW_H_

#include <QtGui/QDialog>
#include <QProcess>
#include <QTextEdit>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QPushButton>
#include <QTcpSocket>

namespace FD
{
    
	//forward declarations
	class UIDocument;
	class QtFlowDesigner;
	
    class QtProcessWindow : public QDialog
    {         
        Q_OBJECT;
        
        public:
        	
    	QtProcessWindow(QtFlowDesigner *parent, UIDocument *doc);
    	~QtProcessWindow();
    	void start();
        int getProcessPort() {return 2938;}
    	
        public slots:
        
        void probesButtonClicked();
        void error ( QProcess::ProcessError error );
        void finished ( int exitCode, QProcess::ExitStatus exitStatus );
        void readyReadStandardError ();
        void readyReadStandardOutput ();
        void started ();
        void stateChanged ( QProcess::ProcessState newState );
        	
        protected:
        	
    	QtFlowDesigner *m_flowdesigner;
    	UIDocument *m_document;
    	QProcess *m_process;
    	QTextEdit *m_textEdit;
    	QVBoxLayout *m_mainLayout;
    	QHBoxLayout *m_buttonLayout;
    	QPushButton *m_probesButton;
        	
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
