#include "QtProbeManager.h"
#include <iostream>

namespace FD
{
	using namespace std;

	
	FlowDesignerTCPServerClient::FlowDesignerTCPServerClient(FlowDesignerTCPServer &server, QTcpSocket *socket)
	: m_server(server), m_socket(socket) {
		
		assert(m_socket);
		
		//Connect signals
		connect(m_socket,SIGNAL(readyRead()),this,SLOT(readyRead()));
		cerr<<"FlowDesignerTCPServerClient::FlowDesignerTCPServerClient(FlowDesignerTCPServer &server, QTcpSocket *socket)"<<endl;
		//m_socket->setTextModeEnabled(true);
		
		
		
	}
	
	void FlowDesignerTCPServerClient::readyRead()
	{
		cerr<<"FlowDesignerTCPServerClient::readyRead()"<<endl;
	}
	
	void FlowDesignerTCPServerClient::run ()
	{
		cerr<<"void FlowDesignerTCPServerClient::run ()"<<endl;
		while(m_server.running())
		{
			usleep(1000); //sleep 1ms
			
			if (!m_socket->isValid()) break;
			
			//if (m_socket->bytesAvailable() > 0)
				//cerr<<"Thread bytes available : "<<m_socket->bytesAvailable()<<endl;
			
			
			
			if (m_socket->canReadLine())
			{
				QByteArray data = m_socket->readLine();
				cerr<<"FlowDesignerTCPServerClient::run got bytes : "<<data.size()<<endl;
				for(unsigned int i = 0; i<data.size(); i++)
				{
					cerr << data[i];
				}
				cerr<<endl;
			}

		}
		
		//Execute event loop
		exec();	
	}
	
	
	FlowDesignerTCPServer::FlowDesignerTCPServer(QObject* parent, unsigned int port)
		: QTcpServer(parent), m_running(true)
	{
		
		setMaxPendingConnections(100);
		
		if (listen(QHostAddress::Any, port))
			cerr<<"FlowDesignerTCPServer starting on port : "<<port<<endl;
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
		QTcpServer::incomingConnection(socketDescriptor);
		
		cerr<<"FlowDesignerTCPServer::incomingConnection ( int socketDescriptor )"<<endl;
		
		QTcpSocket *socket =  nextPendingConnection();
		
		//Create client
		if (socket)
		{
			FlowDesignerTCPServerClient *client = new FlowDesignerTCPServerClient(*this,socket);
			m_clients.push_back(client);
			client->start();
		}
	}

	
	
	QtProbeManager::QtProbeManager(QObject* parent)
		: QObject(parent)
	{
		m_server = new FlowDesignerTCPServer(this);
	}
	
} //namespace FD




