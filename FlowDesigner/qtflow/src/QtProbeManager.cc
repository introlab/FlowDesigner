#include "QtProbeManager.h"
#include <iostream>
#include "QtRunContext.h"

namespace FD
{
	using namespace std;

	
	FlowDesignerTCPServerClient::FlowDesignerTCPServerClient(FlowDesignerTCPServer *server, int socketDescriptor)
	: m_server(server), m_socketDescriptor(socketDescriptor) {
				
	}
	
	void FlowDesignerTCPServerClient::run ()
	{
		cerr<<"void FlowDesignerTCPServerClient::run ()"<<endl;
		
		//Socket will be associated to this thread
		QTcpSocket tcpSocket; 
		
		//Make sure the socket is valid
		if (!tcpSocket.setSocketDescriptor(m_socketDescriptor)) {
	         //emit error(tcpSocket.error());
			 cerr << tcpSocket.error() <<endl;
	         return;
		}
		
		
		while(m_server->running())
		{
			
			//TODO Can we do better with event loop and signals?
			
			if (!tcpSocket.isValid()) break;

			//Will wait for 1000ms for input data on socket
			tcpSocket.waitForReadyRead(1000);
	
			//Try to read line
			QByteArray data = tcpSocket.readLine();
			
			//DEBUG OUTPUT
			cerr<<"FlowDesignerTCPServerClient::run got bytes : "<<data.size()<<endl;
			for(unsigned int i = 0; i<data.size(); i++)
			{
				cerr << data[i];
			}
			cerr<<endl;
			
			if (data.size() > 0)
			{	
				
				QString command(data);
				
				if (command.contains("which"))
				{
					cerr<<"which" <<endl;
					
					UIDocument* doc = m_server->getProbeManager()->getContext()->getDocument();
					
					//WRITE XML TO A STREAM
					int size;
					char *xmldata = doc->saveToMemory(size);
					tcpSocket.write(xmldata,size);
					tcpSocket.flush();
					
					//This was allocated by libxml
					free(xmldata);
				}
				else if (command.contains("connect"))
				{
					cerr << "connect"<<endl;
				}
				else
				{
					cerr << "unknown command : "<<command.toStdString()<<endl;
				}
			}
		}
		
		//Execute event loop
		exec();	
	}
	
	
	FlowDesignerTCPServer::FlowDesignerTCPServer(QtProbeManager *manager, unsigned int port)
		: QTcpServer(this), m_manager(manager), m_running(true)
	{
		
		setMaxPendingConnections(100);
		
		if (listen(QHostAddress::Any, port))
		{
			cerr<<"FlowDesignerTCPServer starting on port : "<<port<<endl;
		}
	}
	

	
	FlowDesignerTCPServer::~FlowDesignerTCPServer()
	{
		m_running = false;
		
		//wait for clients
		for (unsigned int i = 0; i <m_clients.size(); i++)
		{
			m_clients[i]->exit();
			m_clients[i]->wait();
			delete m_clients[i];
		}
	}
	
	
	void FlowDesignerTCPServer::incomingConnection ( int socketDescriptor )
	{
		
		cerr<<"FlowDesignerTCPServer::incomingConnection ( int socketDescriptor )"<<endl;			
		//Create client that will handle this connection
		FlowDesignerTCPServerClient *client = new FlowDesignerTCPServerClient(this,socketDescriptor);
		//Make sure the thread is deleted when finished
		connect(client, SIGNAL(finished()), client, SLOT(deleteLater()));
		m_clients.push_back(client);
		client->start();
		
	}
	
	QtProbeManager::QtProbeManager(QtRunContext *context)
		: m_context(context)
	{
		m_server = new FlowDesignerTCPServer(this);
	}
	
	QtRunContext* QtProbeManager::getContext()
	{
		return m_context;
	}
	
			
	
	
} //namespace FD




