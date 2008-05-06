#include "QtProbeManager.h"
#include <iostream>
#include "QtRunContext.h"
#include "UIProbeLink.h"
#include "UITerminal.h"
#include <map>
#include "CompositeType.h"

namespace FD
{
	using namespace std;

	
	FlowDesignerTCPServerClient::FlowDesignerTCPServerClient(FlowDesignerTCPServer *server, int socketDescriptor)
	: m_server(server), m_socketDescriptor(socketDescriptor), m_tcpSocket(NULL) {
				
	}
	
	void FlowDesignerTCPServerClient::notify (ObjectRef object, int count)
	{
		cerr<<"FlowDesignerTCPServerClient::notify (ObjectRef object)"<<endl;
		
		if (m_tcpSocket)
		{
			//serialize object
			stringstream output;
			
			CompositeType composite;
			
			composite.addField("count",ObjectRef(Int::alloc(count)));
			composite.addField("object", object);
			
			//composite.printOn(output);
			composite.serialize(output);
			
			output <<endl;
			
			//Add it to the list
			m_tcpSocket->write(output.str().c_str(),output.str().size());
			m_tcpSocket->flush();
		}
	}
	
	void FlowDesignerTCPServerClient::run ()
	{
		cerr<<"void FlowDesignerTCPServerClient::run ()"<<endl;
		
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
			
			if (!m_tcpSocket->isValid()) break;

			//Will wait for 1000ms for input data on socket
			m_tcpSocket->waitForReadyRead(1000);
	
			//Try to read line
			QByteArray data = m_tcpSocket->readLine();
			
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
					m_tcpSocket->write(xmldata,size);

					
					//This was allocated by libxml
					free(xmldata);
				}
				else if (command.contains("disconnect"))
				{
					cerr << "disconnect"<<endl;
					
					stringstream input(command.toStdString());
					stringstream output;
					
					string cmd;
					int id;
					
					input >> cmd >> id;
					
					cerr<<"Trying to disconnect to id : "<<id<<endl;
					map<UIProbeLink*,UIProbeLinkNode*> linkMap = UIProbeLink::getProbeDictionary();
					
					if (linkMap.find((UIProbeLink*)id) != linkMap.end())
					{
						UIProbeLink* link = (UIProbeLink*) id;
						link->unregisterIF(this);
						output<<"disconnect ok "<<id<<endl;
					}
					else
					{
						output<<"disconnect failed "<<id<<endl;
					}
					
					m_tcpSocket->write(output.str().c_str(),output.str().size());
				}
				else if (command.contains("connect"))
				{
					cerr << "connect"<<endl;
					
					stringstream input(command.toStdString());
					stringstream output;
					
					string cmd;
					int id;
					
					input >> cmd >> id;
					
					cerr<<"Trying to connect to id : "<<id<<endl;
					map<UIProbeLink*,UIProbeLinkNode*> linkMap = UIProbeLink::getProbeDictionary();
					
					if (linkMap.find((UIProbeLink*)id) != linkMap.end())
					{
						UIProbeLink* link = (UIProbeLink*) id;
						link->registerIF(this);
						output<<"connect ok "<<id<<endl;
					}
					else
					{
						output<<"connect failed "<<id<<endl;
					}
					
					m_tcpSocket->write(output.str().c_str(),output.str().size());
				}
				else if (command.contains("list"))
				{
					cerr<<"list" <<endl;				
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
						
						output << (unsigned int)link << " "<<from->getName()<<" "<<from->getType()
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
		
		//Execute event loop
		exec();	
	}
	
	
	FlowDesignerTCPServer::FlowDesignerTCPServer(QtProbeManager *manager, unsigned int port)
		: QTcpServer(manager), m_manager(manager), m_running(true)
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




