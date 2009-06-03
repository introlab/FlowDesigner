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
#include "QtProcessWindow.h"
#include "QtFlowDesigner.h"
#include "QtProbeRegistry.h"
#include "QtProbe.h"

#include "UIDocument.h"

#include <QPushButton>
#include <QTextEdit>
#include <QDialog>
#include <QMessageBox>
#include <QTcpServer>

namespace FD 
{

	QtProcessWindow::QtProcessWindow(QtFlowDesigner *parent, UIDocument *doc)
	 : QMainWindow(parent), 
	   m_flowdesigner(parent), 
	   m_process(NULL),
	   m_uiDocView(NULL), 
	   m_processHost("localhost"), // default
	   m_processPort(0) // default
	{
		// Initialize the window in local process mode
		setupUi();

		if(doc) {
			// Test if the port is not used. If it is used, 
			// get automaticaly a port.
			int newPort = 0;
			bool portChanged = false;
			QTcpServer tmpServer;
			if (doc->getConnectionPort() == 0 || !tmpServer.listen(QHostAddress::Any, doc->getConnectionPort()))
			{
				tmpServer.listen(QHostAddress::Any, 0);
				newPort = tmpServer.serverPort();
				portChanged = true;
				
				if(doc->getConnectionPort() != 0) {
					QMessageBox::warning(this, tr("Warning"), tr("Port '%1' is already used. The port '%2' was chosen automatically.").arg(doc->getConnectionPort()).arg(newPort), QMessageBox::Ok);
				}
			}
			tmpServer.close();
			
			//Change temporary the port
			int tmpPort;
			if(portChanged) {
				tmpPort = doc->getConnectionPort();
				doc->setConnectionPort(newPort);	
			}
			
			//save to memory
			int size = 0;
			char* mem = doc->saveToMemory(size);
			
			//Restore the port
			if(portChanged) {
				doc->setConnectionPort(tmpPort);	
			}
			
			//Add networks to the view
			m_uiDocView->loadFromMemory(mem, size);
			m_uiDocView->setEditable(false);
			
			// Get the connection port for probing
			m_processPort = m_uiDocView->getConnectionPort();
			
			// Start a QtFlow process with the document
			start(mem, size);

			//Free the memory
			free(mem);
			
			this->setWindowTitle(tr("Process \"%1\" - port %2").arg(m_uiDocView->getName().c_str()).arg(m_uiDocView->getConnectionPort()));
		}
	}
	
	QtProcessWindow::QtProcessWindow(QtFlowDesigner *parent, QString host, int port)
	 : QMainWindow(parent), 
	   m_flowdesigner(parent), 
	   m_process(NULL),
	   m_uiDocView(NULL), 
	   m_processHost(host),
	   m_processPort(port)
	{
		// Initialize the window in remote mode
		setupUi();
        
		//Get the UIDocument from the host
		QTcpSocket* socket = new QTcpSocket(this);
    	socket->connectToHost(m_processHost, m_processPort);
    	if (socket->waitForConnected(DEFAULT_WAIT_TIME_MS))
    	{
    		connect(socket, SIGNAL(disconnected()), this, SLOT(remoteDisconnected()));
    		m_textBrowser->append(tr("Connected on %1:%2.").arg(host).arg(port));
	    	QString buf = QString("which\n");     	
	    	socket->write(buf.toStdString().c_str(), buf.size());
	    	socket->waitForBytesWritten(DEFAULT_WAIT_TIME_MS);
	    	if(socket->waitForReadyRead(DEFAULT_WAIT_TIME_MS))
	    	{	
	    		QByteArray data;
	    		
				int bytesReceived = socket->bytesAvailable();
				if(bytesReceived<4) {
					//TODO
					return;
				}
				int dataSize;
				socket->read((char*)(&dataSize), 4);
				
				while(data.size() < dataSize) {
					if(!socket->bytesAvailable()) {
						if(!socket->waitForReadyRead(DEFAULT_WAIT_TIME_MS)) {
							//Prob here
							return;
						}
					}
					bytesReceived = socket->bytesAvailable();
					data.append(socket->read(dataSize - data.size()));
				}
				
				//For debug
				//std::cerr << "data.size() = " << data.size() << std::endl;
				//std::cerr << "dataSize = " << dataSize << std::endl;
				//std::cerr.write(data.data(), dataSize);

				m_uiDocView->loadFromMemory(data.data(), data.size());
				m_uiDocView->setEditable(false);
				
				m_textBrowser->append(tr("Document \'%1\' downloaded (size = %2).").arg(m_uiDocView->getName().c_str()).arg(data.size()));
				
				this->setWindowTitle(tr("Remote \"%1\" - host \"%2\" - port %3").arg(m_uiDocView->getName().c_str()).arg(host).arg(port));
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
    	QList<QtProbe*> probes = this->findChildren<QtProbe*>();
		for(int i=0; i<probes.size(); i++) {
			probes[i]->stop();
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
			m_process->start("qtflow",args);
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
        
        connect(qtnet, SIGNAL(signalLinkProbed(int, const QString &)), this, SLOT(linkProbed(int, const QString &)));
        
		return qtnet;	
	}
	
	void QtProcessWindow::linkProbed(int linkId, const QString &probeType)
	{
		QtProbe* probe = QtProbeRegistry::createProbe(probeType, this, this->getProcessHost(), this->getProcessPort(), linkId);
		if(probe)
		{
			probe->init();
			probe->move(QCursor::pos());
			probe->show();
		}
		else {
			QMessageBox::warning(this, tr("QtFlow"),
            	tr("The probe '%1' is unavailable.").arg(probeType),
           		QMessageBox::Ok, QMessageBox::Ok);
		}
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
    	m_socket->connectToHost(m_processWindow->getProcessHost(), m_processWindow->getProcessPort());
    	
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
    
    
	
} //namespace FD



