#include "QtProbeManager.h"
#include <iostream>
#include "QtRunContext.h"
#include "UIProbeLink.h"
#include "UITerminal.h"
#include <map>
#include "CompositeType.h"
#include <QApplication>

namespace FD
{
	using namespace std;

	
	FlowDesignerTCPServerClient::FlowDesignerTCPServerClient(FlowDesignerTCPServer *server, int socketDescriptor)
	: m_server(server), m_socketDescriptor(socketDescriptor), m_tcpSocket(NULL) 
	{			
	}
	
	FlowDesignerTCPServerClient::~FlowDesignerTCPServerClient()
	{
		if (m_tcpSocket)
		{
			delete m_tcpSocket;
		}
	}
	
	void FlowDesignerTCPServerClient::notify (ObjectRef object, int count)
	{
		if (m_tcpSocket)
		{
			//serialize object
			stringstream output;

			Object &objectValue = *object;
			
			output << objectValue << endl;
			
			//Add it to the list
			m_tcpSocket->write(output.str().c_str(),output.str().size());
			m_tcpSocket->flush();
		}
	}
	
	void FlowDesignerTCPServerClient::run ()
	{
		//Socket will be associated to this thread
		m_tcpSocket = new QTcpSocket();
		
		//Make sure the socket is valid
		if (!m_tcpSocket->setSocketDescriptor(m_socketDescriptor)) {
	         //emit error(tcpSocket.error());
			 cerr << m_tcpSocket->error() <<endl;
	         return;
		}
		
		
		while(m_server->running())
		{
			
			//TODO Can we do better with event loop and signals?

			if(!m_tcpSocket->waitForReadyRead(-1)) {
				break;
			}
	
			//Try to read line
			QByteArray data = m_tcpSocket->readLine();
			
			//DEBUG OUTPUT
			/*cerr<<"FlowDesignerTCPServerClient::run got bytes : "<<data.size()<<endl;
			for(unsigned int i = 0; i<data.size(); i++)
			{
				cerr << data[i];
			}
			cerr<<endl;*/
			
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
					m_tcpSocket->write(xmldata,size);

					
					//This was allocated by libxml
					free(xmldata);
				}
				else if (command.contains("disconnect"))
				{
					cerr << "QtProbeManager::disconnect" << endl;
					stringstream input(command.toStdString());
					stringstream output;
					
					string cmd;
					int id;
					
					input >> cmd >> id;
					
					//cerr<<"Trying to disconnect to id : "<<id<<endl;
					map<UIProbeLink*,UIProbeLinkNode*> linkMap = UIProbeLink::getProbeDictionary();
					
					bool found = false;
					for(map<UIProbeLink*,UIProbeLinkNode*>::iterator it = linkMap.begin(); it != linkMap.end(); it++)
					{
						if((*it).first->getId() == id)
						{
							(*it).first->unregisterIF(this);
							found = true;
							break;
						}
					}
					if(found) {
						output<<"disconnect ok "<<id<<endl;
					}
					else {
						output<<"disconnect failed "<<id<<endl;
					}
					cerr << output.str().c_str() << endl;
					m_tcpSocket->write(output.str().c_str(),output.str().size());
				}
				else if (command.contains("connect"))
				{
					stringstream input(command.toStdString());
					stringstream output;
					
					string cmd;
					int id;
					
					input >> cmd >> id;
					
					//cerr<<"Trying to connect to id : "<<id<<endl;
					map<UIProbeLink*,UIProbeLinkNode*> linkMap = UIProbeLink::getProbeDictionary();
					
					bool found = false;
					for(map<UIProbeLink*,UIProbeLinkNode*>::iterator it = linkMap.begin(); it != linkMap.end(); it++)
					{
						if((*it).first->getId() == id)
						{
							(*it).first->registerIF(this);
							found = true;
							break;
						}
					}
					if(found) {
						output<<"connect ok "<<id<<endl;
					}
					else {
						output<<"connect failed "<<id<<endl;
					}
					
					
					m_tcpSocket->write(output.str().c_str(),output.str().size());
				}
				else if (command.contains("list"))
				{
					//This will make a copy of actual map.
					//Is this what we want?
					map<UIProbeLink*,UIProbeLinkNode*> linkMap = UIProbeLink::getProbeDictionary();
					
					stringstream output;
					output << "size: "<<linkMap.size()<<endl;
					
					for(map<UIProbeLink*,UIProbeLinkNode*>::iterator iter = linkMap.begin(); iter != linkMap.end(); iter++)
					{
						UIProbeLink *link = iter->first;
						UITerminal *from = link->getFromTerminal();
						UITerminal *to = link->getToTerminal();
						
						output << (unsigned int)link << " " << link->getId() << " "<<from->getName()<<" "<<from->getType()
							   <<" "<<to->getName()<<" "<<to->getType()<<endl;
						
					}
						
					
					m_tcpSocket->write(output.str().c_str(),output.str().size());

					
				}
				else
				{
					cerr << "unknown command : "<<command.toStdString()<<endl;
				}
				
				//Make sure we flush data from socket
				m_tcpSocket->flush();
			}
		}
		
		QApplication::postEvent(m_server, new ClientEvent(this));
	}
	
	
	FlowDesignerTCPServer::FlowDesignerTCPServer(QtProbeManager *manager, unsigned int port)
		: QTcpServer(manager), m_manager(manager), m_running(true)
	{
		
		setMaxPendingConnections(100);
		
		if (listen(QHostAddress::Any, port))
		{
			//cerr<<"FlowDesignerTCPServer starting on port : "<<port<<endl;
		}
	}
	

	
	FlowDesignerTCPServer::~FlowDesignerTCPServer()
	{
		m_running = false;
		
		for(int i=0; i<m_clients.size(); i++)
		{
			delete m_clients[i];
		}
		m_clients.clear();
	}
	
	
	void FlowDesignerTCPServer::incomingConnection ( int socketDescriptor )
	{	
		//Create client that will handle this connection
		FlowDesignerTCPServerClient *client = new FlowDesignerTCPServerClient(this,socketDescriptor);
		//Make sure the thread is deleted when finished
		connect(client, SIGNAL(finished()), client, SLOT(deleteLater()));
		m_clients.push_back(client);
		client->start();
		
	}
	
	bool FlowDesignerTCPServer::event(QEvent *e)
	{
		
		if (e->type() == QEvent::User)
		{
			ClientEvent *pEvent = (ClientEvent *) e;
			
			int index = -1;
			for(int i=0; i<m_clients.size(); i++)
			{
				if(m_clients[i] == pEvent->getClient()) {
					delete m_clients[i];
					index = i;
				}
			}
			// Client is found?
			if(index >= 0) {
				m_clients.removeAt(index);
			}

			return true;
		}
	
		return QTcpServer::event(e);
	}
	
	
	QtProbeManager::QtProbeManager(QtRunContext *context)
		: m_context(context)
	{
		m_server = new FlowDesignerTCPServer(this, m_context->getDocument()->getConnectionPort());
	}
	
	QtRunContext* QtProbeManager::getContext()
	{
		return m_context;
	}
	
			
	
	
} //namespace FD




