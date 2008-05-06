#ifndef _QTPROBEMANAGER_H_
#define _QTPROBEMANAGER_H_

#include <QTcpServer>
#include <QThread>
#include <QTcpSocket>
#include <vector>
#include "UIProbeLink.h"
#include <list>

namespace FD 
{

	class FlowDesignerTCPServer;
	class QtRunContext;
	
	class FlowDesignerTCPServerClient : public QThread, public UIObserverIF
	{
		Q_OBJECT;
		
	public:
		
		FlowDesignerTCPServerClient(FlowDesignerTCPServer *server, int socketDescriptor);
		virtual void run ();
		virtual void notify (ObjectRef object);
	protected:
		
		int m_socketDescriptor;
		FlowDesignerTCPServer *m_server;
		std::list<std::string> m_messageList;
		QTcpSocket *m_tcpSocket; 
	};

	
	class QtProbeManager;
	
	class FlowDesignerTCPServer : public QTcpServer
	{
	
		Q_OBJECT;
	public:
		FlowDesignerTCPServer(QtProbeManager *manager, unsigned int port = 2938);
		
		~FlowDesignerTCPServer();
		
		QtProbeManager* getProbeManager() {return m_manager;}
		
		bool running() {return m_running;}
		
	protected:
		QtProbeManager *m_manager;
		virtual void incomingConnection ( int socketDescriptor );
		bool m_running;
		std::vector<FlowDesignerTCPServerClient*> m_clients;
	};


	
	class QtProbeManager : public QObject
	{
		Q_OBJECT;
		
	public:
		
		QtProbeManager(QtRunContext *context);
		QtRunContext* getContext();
		
	protected:
		FlowDesignerTCPServer *m_server;
		QtRunContext *m_context;
	};

} //namespace FD

#endif
