#ifndef QTFLOWIPBROADCASTER_H_
#define QTFLOWIPBROADCASTER_H_

#include <QObject>
#include <QTimer>
#include <QUdpSocket>
#include <QHostInfo>

class QtFlowIpBroadcaster : public QObject
{
	Q_OBJECT;
	
	private:
		static const int BROADCAST_PORT = 51515;
		static const int BROADCAST_INTERVAL_MS = 3000;
		
		QString m_host;
		int m_port;
		
		QTimer* m_timer;
		QUdpSocket* m_udpSocket;
	
	public:
		QtFlowIpBroadcaster(QObject *parent, int port)
			: QObject(parent), m_port(port)
		{
			m_host = QHostInfo::localHostName();
			m_timer = new QTimer(this);
			m_udpSocket = new QUdpSocket(this);
			
			//start broadcasting
			m_timer->start(BROADCAST_INTERVAL_MS);
			
			connect(m_timer, SIGNAL(timeout()), this, SLOT(broadcastDatagram()));
		}
		
		~QtFlowIpBroadcaster()
		{
		}
		
	public slots:	
		void broadcastDatagram()
		{
		    QByteArray datagram = "<QtFlow host=" + m_host.toLatin1() + " port=" + QByteArray::number(m_port) + ">";
		    m_udpSocket->writeDatagram(datagram.data(), datagram.size(),
		                              QHostAddress::Broadcast, BROADCAST_PORT);
		}

};

#endif /*QTFLOWIPBROADCASTER_H_*/
