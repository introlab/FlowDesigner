#ifndef _QTPROBEMANAGER_H_
#define _QTPROBEMANAGER_H_

#include <QTcpServer>
#include <QThread>
#include <QTcpSocket>
#include <vector>

namespace FD 
{

	class FlowDesignerTCPServer;
	
	class FlowDesignerTCPServerClient : public QThread
	{
		Q_OBJECT;
		
	public:
		
		FlowDesignerTCPServerClient(FlowDesignerTCPServer &server, QTcpSocket *socket);
		virtual void run ();
		
	public slots:
		void readyRead();
	protected:
		
		FlowDesignerTCPServer &m_server;
		QTcpSocket *m_socket;
	};


	class FlowDesignerTCPServer : public QTcpServer
	{
	
		Q_OBJECT;
	public:
		FlowDesignerTCPServer(QObject *parent=NULL, unsigned int port = 2938);
		~FlowDesignerTCPServer();
		bool running() {return m_running;}
		
	protected:
		virtual void incomingConnection ( int socketDescriptor );
		bool m_running;
		std::vector<FlowDesignerTCPServerClient*> m_clients;
	};

	class QtProbeManager : public QObject
	{
		Q_OBJECT;
		
	public:
		QtProbeManager(QObject* parent=NULL);
		
	protected:
		FlowDesignerTCPServer *m_server;
	};

} //namespace FD

#endif
