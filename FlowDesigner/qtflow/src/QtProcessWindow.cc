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
	 : QMainWindow(parent), 
	   m_flowdesigner(parent), 
	   m_uiDocView(NULL), 
	   m_process(NULL),
	   m_processHost("localhost"), // default
	   m_processPort(51115) // default
	{
		// Initialize the window in process mode
		setupUi();

		if(doc) {
			//save to memory
			int size = 0;
			char* mem = doc->saveToMemory(size);
			
			//Add networks to the view
			m_uiDocView->loadFromMemory(mem, size);
			m_uiDocView->setEditable(false);
			
			// Get the connection port for probing
			m_processPort = m_uiDocView->getConnectionPort();
			
			// Start a QtFlow process with the document
			start(mem, size);
			
			this->setWindowTitle(tr("Process \"%1\"").arg(m_uiDocView->getName().c_str()));

			//Free the memory
			free(mem);
		}
	}
	
	QtProcessWindow::QtProcessWindow(QtFlowDesigner *parent, QString host, int port)
	 : QMainWindow(parent), 
	   m_flowdesigner(parent), 
	   m_uiDocView(NULL), 
	   m_process(NULL),
	   m_processHost(host),
	   m_processPort(port)
	{
		// Initialize the window
		setupUi();
        
		//Get the UIDocument from the host
		QTcpSocket* socket = new QTcpSocket(this);
    	socket->connectToHost(m_processHost, m_processPort);
    	if (socket->waitForConnected(3000))
    	{
    		connect(socket, SIGNAL(disconnected()), this, SLOT(remoteDisconnected()));
    		m_textBrowser->append(tr("Connected."));
	    	QString buf = QString("which\n");     	
	    	socket->write(buf.toStdString().c_str(), buf.size());
	    	socket->waitForBytesWritten(3000);
	    	if(socket->waitForReadyRead(3000))
	    	{
	    		m_textBrowser->append(tr("UIDocument downloaded."));
				int size = socket->bytesAvailable();
				char* mem = new char[size];
				socket->read(mem, size);
				m_uiDocView->loadFromMemory(mem,size);
				m_uiDocView->setEditable(false);
				
				this->setWindowTitle(tr("Remote process \"%1\"").arg(m_uiDocView->getName().c_str()));
				
				free(mem);
	    	}
    	}
    	else
    	{
    		m_textBrowser->append(tr("Can't connect to host : %1:%2").arg(host).arg(port));
    	}
	}

	QtProcessWindow::~QtProcessWindow()
	{
		if (m_process)
		{
			if(m_process->state() != QProcess::NotRunning) {
				m_process->terminate();
				if(!m_process->waitForFinished())
				{
					m_process->kill();
				}
			}
			
    		delete m_process;
    		m_process = NULL;
		}
		if(m_uiDocView) {
			m_uiDocView->unregisterEvents(this);
			delete m_uiDocView;
			m_uiDocView = NULL;
		}
	}
	
	void QtProcessWindow::setupUi()
    {
    	// Delete on close
		this->setAttribute(Qt::WA_DeleteOnClose);		
		
	    this->setWindowTitle(tr("QtProcessWindow"));
	    this->resize(700, 600);
	    
	    QWidget* centralwidget = new QWidget(this);   
	    this->setCentralWidget(centralwidget);
	    QVBoxLayout* mainLayout = new QVBoxLayout(centralwidget);
	    
	    QGroupBox* buttonGroupBox = new QGroupBox(centralwidget);
	    buttonGroupBox->setTitle(tr("Controls"));	    
	    QHBoxLayout* buttonLayout = new QHBoxLayout(buttonGroupBox);
	    //Add "Probes" Button
		QPushButton* probesButton = new QPushButton("Probes",this);
		buttonLayout->addWidget(probesButton);
		connect(probesButton,SIGNAL(clicked()),this,SLOT(probesButtonClicked()));
		//Add "Stop" Button
		QPushButton* stopButton = new QPushButton("Stop",this);
		buttonLayout->addWidget(stopButton);
		connect(stopButton,SIGNAL(clicked()),this,SLOT(stopButtonClicked()));
		mainLayout->addWidget(buttonGroupBox);
	
	    QGroupBox* groupBox = new QGroupBox(centralwidget);
	    groupBox->setTitle(tr("Network"));
	    QGridLayout* gridLayout_4 = new QGridLayout(groupBox);
	    m_tabWidget = new QTabWidget(groupBox);
	    gridLayout_4->addWidget(m_tabWidget, 0, 0, 1, 1);
	    mainLayout->addWidget(groupBox);

	    m_mainOutputDockWidget = new QDockWidget(this);
	    m_mainOutputDockWidget->setWindowTitle(tr("Main output"));
	    m_mainOutputDockWidget->setGeometry(QRect(0, 252, 251, 150));
	    m_mainOutputDockWidget->setFeatures(QDockWidget::DockWidgetFloatable|QDockWidget::DockWidgetMovable);
	    QWidget* dockWidgetContents = new QWidget();
	    dockWidgetContents->setObjectName(QString::fromUtf8("dockWidgetContents"));
	    QGridLayout* gridLayout_3 = new QGridLayout(dockWidgetContents);
	    gridLayout_3->setObjectName(QString::fromUtf8("gridLayout_3"));
	    m_textBrowser = new QTextBrowser(dockWidgetContents);
	    m_textBrowser->setObjectName(QString::fromUtf8("textBrowser"));
	    m_textBrowser->setReadOnly(false);
	    gridLayout_3->addWidget(m_textBrowser, 1, 0, 1, 1);
	    m_mainOutputDockWidget->setWidget(dockWidgetContents);
	    this->addDockWidget(static_cast<Qt::DockWidgetArea>(8), m_mainOutputDockWidget);
	    
	    //Disable dock animation
        this->setAnimated(false);
        
	    //Create a new empty document
		m_uiDocView = new UIDocument("");
		//Register to document events
        m_uiDocView->registerEvents(this);
    } // setupUi

    void QtProcessWindow::closeEvent(QCloseEvent *event)
    {
        if (m_process && m_process->state() != QProcess::NotRunning)
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
    	QList<QtProbeConsole *> consoles = this->findChildren<QtProbeConsole *>();
		for(int i=0; i<consoles.size(); i++) {
			consoles[i]->stop();
		}
	}
	
	void QtProcessWindow::remoteDisconnected()
	{
    	m_textBrowser->append(tr("Connection lost with the host : %1:%2").arg(m_processHost).arg(m_processPort));
	}
	
	void QtProcessWindow::start(char *mem, int size)
	{
		
		if (mem && size>0)
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

			//Launch qtflow			
			m_process->start(QString(INSTALL_PREFIX) + "/bin/qtflow",args);
			m_process->write(mem,size);
			
			//This will close stdin, enabling to exit
			//read loop in qtflow
			m_process->closeWriteChannel();
		}
	}

    void QtProcessWindow::error ( QProcess::ProcessError error )
    {
    	
    	switch(error)
    	{
    	case QProcess::FailedToStart:
    		m_textBrowser->append(tr("<b>Process error : FailedToStart</b>"));
    		break;
    		
    	case QProcess::Crashed:
    		m_textBrowser->append(tr("<b>Process error : Crashed</b>"));
    		break;
    		
    	case QProcess::Timedout:
    		m_textBrowser->append(tr("<b>Process error : Timedout</b>"));
    		break;
    	
    	case QProcess::WriteError:
    		m_textBrowser->append(tr("<b>Process error : Write Error</b>"));
    		break;
    	
    	case QProcess::ReadError:
    		m_textBrowser->append(tr("<b>Process error : ReadError</b>"));
    		break;
    	
    	case QProcess::UnknownError:
    		m_textBrowser->append(tr("<b>Process error : UnknownError</b>"));
    		break;

    	default:
    		m_textBrowser->append(tr("<b>Process error, unknown cause.</b>"));
    		break;
    	}
    }
    
    void QtProcessWindow::finished ( int exitCode, QProcess::ExitStatus exitStatus )
    {
    	m_textBrowser->append(tr("<b>Finished wih exit code : %1</b>").arg(exitCode));
    }
    
    void QtProcessWindow::readyReadStandardError ()
    {
    	if (m_process)
    	{
    		QColor oldColor = m_textBrowser->textColor();
    		m_textBrowser->setTextColor(Qt::red);
    		QByteArray output = m_process->readAllStandardError();
    		m_textBrowser->append(output);
    		m_textBrowser->setTextColor(oldColor);
    	}
    }
    	
    void QtProcessWindow::readyReadStandardOutput ()
    {
    	if (m_process)
    	{
    		QColor oldColor = m_textBrowser->textColor();
    		m_textBrowser->setTextColor(Qt::green);
    		QByteArray output = m_process->readAllStandardOutput();
    		m_textBrowser->append(output);
    		m_textBrowser->setTextColor(oldColor);
    	}
    }
    
    void QtProcessWindow::started ()
    {
    	m_textBrowser->append(tr("<b>Process started</b>"));
    }
    
    void QtProcessWindow::stateChanged ( QProcess::ProcessState newState )
    {
    	//m_textEdit->append("State Changed");
    }
	
	QtNetwork* QtProcessWindow::addNetwork(UINetwork* net)
	{		
        QtNetwork *qtnet = new QtNetwork(0,net);
		//qtnet->fitInView(qtnet->scene()->itemsBoundingRect(),Qt::KeepAspectRatio);
		qtnet->centerOn(qtnet->scene()->itemsBoundingRect().topLeft());
		
		//m_networkMap.insert(make_pair(net,qtnet));
		
		qtnet->setObjectName(QString::fromUtf8(net->getName().c_str()));
        m_tabWidget->addTab(qtnet, net->getName().c_str());
        
        connect(qtnet, SIGNAL(signalLinkProbed(int)), this, SLOT(linkProbed(int)));
        
		return qtnet;	
	}
	
	void QtProcessWindow::linkProbed(int linkId)
	{
		QtProbeConsole *console = new QtProbeConsole(this, linkId);
		this->addDockWidget(Qt::BottomDockWidgetArea, console);
		console->setFloating(true);
		console->move(QCursor::pos());
	}
	
    //Network Added
    void QtProcessWindow::notifyNetworkAdded(const UIDocument *doc, const UINetwork* net)
    {
    	std::cerr<<"QtProcessWindow::notifyNetworkAdded(const UIDocument *doc, const UINetwork* net)"<<std::endl;
    	
    	//add this network
    	addNetwork(const_cast<UINetwork*>(net));   	
    }   
	
	//========================================
	// QtProbesDialog stuff
	//========================================
    QtProbesDialog::QtProbesDialog(QtProcessWindow *parent)
    	: QDialog(parent), m_processWindow(parent), m_socket(NULL)
    {
    	m_mainLayout = new QVBoxLayout(this);
    	m_textEdit = new QTextEdit(this);
    	m_mainLayout->addWidget(m_textEdit);
    	
    	m_socket = new QTcpSocket(this);
    	m_socket->connectToHost(m_processWindow->getProcessHost(),m_processWindow->getProcessPort());
    	
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
    	m_textEdit->append(tr("Connected"));   	
    	m_socket->write("list\n\r",6);
    }
    
    //================================
    // QtProbeConsole stuff
	//================================
	QtProbeConsole::QtProbeConsole(QtProcessWindow *parent, int linkId)
    	: QDockWidget(parent), m_processWindow(parent), m_socket(NULL), m_linkId(linkId)
    {
    	this->setWindowTitle(tr("Link %1").arg(linkId));
	    this->setGeometry(QRect(0, 252, 251, 150));
	    this->setFeatures(QDockWidget::DockWidgetFloatable|QDockWidget::DockWidgetMovable|QDockWidget::DockWidgetClosable);
	    this->setAttribute(Qt::WA_DeleteOnClose);
	    
	    QWidget* dockWidgetContents = new QWidget(this);
    	QVBoxLayout* dockLayout = new QVBoxLayout(dockWidgetContents);
    	m_textBrowser = new QTextBrowser(dockWidgetContents);
    	dockLayout->addWidget(m_textBrowser);
    	this->setWidget(dockWidgetContents);
    	
    	m_socket = new QTcpSocket(this);
    	m_socket->connectToHost(m_processWindow->getProcessHost(),m_processWindow->getProcessPort());
    	
    	connect(m_socket,SIGNAL(connected()),this,SLOT(connected()));
    	connect(m_socket,SIGNAL(readyRead()),this,SLOT(readyRead()));
    	connect(m_socket,SIGNAL(error(QAbstractSocket::SocketError)),this,SLOT(error(QAbstractSocket::SocketError)));
    }
    
    QtProbeConsole::~QtProbeConsole()
    {
		if (m_socket)
	    {
			if(m_socket->state() == QAbstractSocket::ConnectedState) {
			    QString buf = QString("disconnect %1\n").arg(m_linkId);     	
			    m_socket->write(buf.toStdString().c_str(), buf.size());
			    m_socket->waitForBytesWritten();
			}
		    m_socket->close();
		    delete m_socket;
	    }
    }
   
    void QtProbeConsole::stop()
    {
    	m_textBrowser->append(tr("Stopped."));
		if(m_socket->state() == QAbstractSocket::ConnectedState) {
	   	    QString buf = QString("disconnect %1\n").arg(m_linkId);     	
    	    m_socket->write(buf.toStdString().c_str(), buf.size());
    	    m_socket->waitForBytesWritten();
   		}
   		m_socket->close();
    }
    
    void QtProbeConsole::readyRead ()
    {
		//Try to read lines
	    while (m_socket->canReadLine())
	    {
			QByteArray data = m_socket->readLine();			
			QString info(data);
			m_textBrowser->append(info);
	    }
    }
    
    void QtProbeConsole::connected()
    {
    	m_textBrowser->append(tr("Connected"));
    	QString buf = QString("connect %1\n").arg(m_linkId);     	
    	m_socket->write(buf.toStdString().c_str(), buf.size());
    }
    
    void QtProbeConsole::error(QAbstractSocket::SocketError socketError)
    {
    	switch(socketError)
    	{
	    	case QAbstractSocket::ConnectionRefusedError:
	    		m_textBrowser->append(tr("Host unavailable."));
	    		break;
	    	case QAbstractSocket::RemoteHostClosedError:
	    		m_textBrowser->append(tr("Connection lost."));
	    		break;
	    	case QAbstractSocket::HostNotFoundError:
	    		m_textBrowser->append(tr("Host not found."));
	    		break;
	    	default:
	    		m_textBrowser->append(tr("QTcpSocket: Unknown error occured!"));
	    		break;
    	}
    	
    	if(m_socket)
    	{
	    	m_socket->close();
    	}
    }
	
} //namespace FD



