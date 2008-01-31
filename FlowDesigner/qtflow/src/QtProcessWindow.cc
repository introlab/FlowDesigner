#include "QtProcessWindow.h"
#include "QtFlowDesigner.h"
#include "UIDocument.h"
#include <QTextEdit>

namespace FD 
{

	QtProcessWindow::QtProcessWindow(QtFlowDesigner *parent, UIDocument *doc)
	 : QDialog(parent), m_flowdesigner(parent), m_document(doc), m_process(NULL)
	{
		m_mainLayout = new QVBoxLayout(this);
		setLayout(m_mainLayout);
		m_textEdit = new QTextEdit(this);
		m_textEdit->setReadOnly(true);
		m_mainLayout->addWidget(m_textEdit);
		resize(640,480);
		start();
	}

	void QtProcessWindow::start()
	{
		
		if (m_document)
		{
			//Wil launch a process with qtflow
			m_process = new QProcess(this);
			
			
			//QStringList env = QProcess::systemEnvironment();
			
			//env.replaceInStrings(QRegExp("^PATH=(.*)", Qt::CaseInsensitive), "PATH=\\1:/usr/local/bin");
			
			//m_process->setEnvironment(env);
			
			//for (QStringList::iterator iter = env.begin(); iter != env.end(); iter++)
			//	m_textEdit->append(*iter);
			
			//m_process->setProcessChannelMode(QProcess::ForwardedChannels);
					
			//Connect QProcess signals
			connect(m_process,SIGNAL(error(QProcess::ProcessError)),this,SLOT(error(QProcess::ProcessError)));
			connect(m_process,SIGNAL(finished(int, QProcess::ExitStatus)),this,SLOT(finished(int, QProcess::ExitStatus)));
			connect(m_process,SIGNAL(readyReadStandardError()),this,SLOT(readyReadStandardError()));
			connect(m_process,SIGNAL(readyReadStandardOutput()),this,SLOT(readyReadStandardOutput()));
			connect(m_process,SIGNAL(started()),this,SLOT(started()));
			connect(m_process,SIGNAL(stateChanged ( QProcess::ProcessState)),this,SLOT(stateChanged ( QProcess::ProcessState)));
			
			
			QStringList args;			
			args.append("/dev/stdin");
			
			//save to memory
			int size = 0;
			char* mem = m_document->saveToMemory(size);
			
			//Launch qtflow			
			m_process->start(QString(INSTALL_PREFIX) + "/bin/qtflow",args);
			m_process->write(mem,size);
			
			//This will close stdin, enabling to exit
			//read loop in qtflow
			m_process->closeWriteChannel();
			
			//The memory was allocated in libXML (C)
			free(mem);
		}
	}

    void QtProcessWindow::error ( QProcess::ProcessError error )
    {
    	
    	switch(error)
    	{
    	case QProcess::FailedToStart:
    		m_textEdit->append("<b>Process error : FailedToStart</b>");
    		break;
    		
    	case QProcess::Crashed:
    		m_textEdit->append("<b>Process error : Crashed</b>");
    		break;
    		
    	case QProcess::Timedout:
    		m_textEdit->append("<b>Process error : Timedout</b>");
    		break;
    	
    	case QProcess::WriteError:
    		m_textEdit->append("<b>Process error : Write Error</b>");
    		break;
    	
    	case QProcess::ReadError:
    		m_textEdit->append("<b>Process error : ReadError</b>");
    		break;
    	
    	case QProcess::UnknownError:
    		m_textEdit->append("<b>Process error : UnknownError</b>");
    		break;

    	default:
    		m_textEdit->append("<b>Process error, unknown cause.</b>");
    		break;
    	}
    }
    
    void QtProcessWindow::finished ( int exitCode, QProcess::ExitStatus exitStatus )
    {
    	m_textEdit->append(QString("<b>Finished wih exit code :") + QString::number(exitCode) + QString("</b>"));
    	
    	if (m_process)
    	{
    		delete m_process;
    		m_process = NULL;
    	}
    }
    
    void QtProcessWindow::readyReadStandardError ()
    {
    	if (m_process)
    	{
    		QColor oldColor = m_textEdit->textColor();
    		m_textEdit->setTextColor(Qt::red);
    		QByteArray output = m_process->readAllStandardError();
    		m_textEdit->append(output);
    		m_textEdit->setTextColor(oldColor);
    	}
    }
    	
    void QtProcessWindow::readyReadStandardOutput ()
    {
    	if (m_process)
    	{
    		QColor oldColor = m_textEdit->textColor();
    		m_textEdit->setTextColor(Qt::green);
    		QByteArray output = m_process->readAllStandardOutput();
    		m_textEdit->append(output);
    		m_textEdit->setTextColor(oldColor);
    	}
    }
    
    void QtProcessWindow::started ()
    {
    	m_textEdit->append("<b>Process started</b>");
    }
    
    void QtProcessWindow::stateChanged ( QProcess::ProcessState newState )
    {
    	//m_textEdit->append("State Changed");
    }
	
	
	
} //namespace FD


