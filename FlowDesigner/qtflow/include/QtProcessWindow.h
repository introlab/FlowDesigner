#ifndef _QTPROCESSWINDOW_H_
#define _QTPROCESSWINDOW_H_

#include <QtGui/QDialog>
#include <QProcess>
#include <QTextEdit>
#include <QVBoxLayout>

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
        	void start();
        	
        public slots:
        
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
        	
    };
}
#endif /*QTPROCESSWINDOW_H_*/
