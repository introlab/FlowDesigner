#include "QtProcessWindow.h"
#include "QtFlowDesigner.h"
#include <QPushButton>
#include "UIDocument.h"
#include <QTextEdit>
#include <QDialog>
#include <QMessageBox>

namespace FD 
{

	QtProcessWindow::QtProcessWindow(QtFlowDesigner *parent, UIDocument *doc)
	 : QDialog(parent), m_flowdesigner(parent), m_document(doc), m_process(NULL)
	{
		m_mainLayout = new QVBoxLayout(this);
		
		
		//Add button layout
		m_buttonLayout = new QHBoxLayout(this);
		m_mainLayout->addLayout(m_buttonLayout);
		
		//Add "Probes" Button
		m_probesButton = new QPushButton("Probes",this);
		m_buttonLayout->addWidget(m_probesButton);
		connect(m_probesButton,SIGNAL(clicked()),this,SLOT(probesButtonClicked()));
		
		//Add "Stop" Button
		m_stopButton = new QPushButton("Stop",this);
		m_buttonLayout->addWidget(m_stopButton);
		connect(m_stopButton,SIGNAL(clicked()),this,SLOT(stopButtonClicked()));
		
		
		m_textEdit = new QTextEdit(this);
		m_textEdit->setReadOnly(true);
		m_mainLayout->addWidget(m_textEdit);
		resize(640,480);
		start();
	}

	QtProcessWindow::~QtProcessWindow()
	{
		if (m_process)
		{
			m_process->close();
			m_process->terminate();
			m_process->kill();
			delete m_process;
		}
	}

    void QtProcessWindow::closeEvent(QCloseEvent *event)
    {
        if (m_process)
    	{
    		int ret = QMessageBox::warning(this, tr("QtFlow"),
                           tr("A process is still running.\n"
                              "Do you want to terminate the process?"),
                           QMessageBox::Yes | QMessageBox::Cancel,
                           QMessageBox::Cancel);
            if(ret == QMessageBox::Yes) {
                m_process->terminate();
                event->accept();
            }
            else {
                event->ignore();
            }
    	}
        else
        {
            event->accept();
        }
    }
	
	void QtProcessWindow::probesButtonClicked()
	{
		QtProbesDialog *dialog = new QtProbesDialog(this);
		dialog->exec();
		delete dialog;
	}
	
	void QtProcessWindow::stopButtonClicked()
	{
		if (m_process)
    	{
    		m_process->terminate();
    	}
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
	
	
    QtProbesDialog::QtProbesDialog(QtProcessWindow *parent)
    	: QDialog(parent), m_processWindow(parent), m_socket(NULL)
    {
    	m_mainLayout = new QVBoxLayout(this);
    	m_textEdit = new QTextEdit(this);
    	m_mainLayout->addWidget(m_textEdit);
    	
    	m_socket = new QTcpSocket(this);
    	m_socket->connectToHost("localhost",m_processWindow->getProcessPort());
    	
    	connect(m_socket,SIGNAL(connected()),this,SLOT(connected()));
    	connect(m_socket,SIGNAL(readyRead()),this,SLOT(readyRead()));
    }
    
   QtProbesDialog::~QtProbesDialog()
   {
	   if (m_socket)
	   {
		   m_socket->close();
		   delete m_socket;
	   }
   }
    
   void QtProbesDialog::readyRead ()
   {
	    //Try to read lines
	    while (m_socket->canReadLine())
	    {
			QByteArray data = m_socket->readLine();			
			QString info(data);
			m_textEdit->append(info);
	    }
   }
    
    void QtProbesDialog::connected()
    {
    	m_textEdit->append("Connected");   	
    	m_socket->write("list\n\r",6);
    }
    
	
} //namespace FD



