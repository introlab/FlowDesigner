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
#ifndef QTFLOWIPBROADCASTER_H_
#define QTFLOWIPBROADCASTER_H_

#include <QObject>
#include <QTimer>
#include <QUdpSocket>
#include <QHostInfo>

namespace FD {
	
	/**
	 * Broadcast the document name, ip, port of the 
	 * runnning process from a specified udp socket.
	 * Format of the datagram: QtFlow|documentName.n|hostName|port
	 * @author Mathieu Labbe
	 */
	class QtFlowIpBroadcaster : public QObject
		{
			Q_OBJECT;
			
		public:
			/**
			 * The broadcast port.
			 */
			static const int BROADCAST_PORT = 51515;
			
			/**
			 * The broadcast interval.
			 */
			static const int BROADCAST_INTERVAL_MS = 3000; //3 sec
			
		public:
			/**
			 * The constructor.
			 * @param parent the QObject parent
			 * @param documentName the UIDocument name of the process
			 * @param port the port for connection on the process (the UIDocument connection port)
			 */
			QtFlowIpBroadcaster(QObject *parent, const QString &documentName, int port) : 
			QObject(parent), 
			m_documentName(documentName), 
			m_port(port)
			{
				m_host = QHostInfo::localHostName();
				QTimer* timer = new QTimer(this);
				m_udpSocket = new QUdpSocket(this);
				
				//start broadcasting
				timer->start(BROADCAST_INTERVAL_MS);
				
				connect(timer, SIGNAL(timeout()), this, SLOT(broadcastDatagram()));
			}
			
			/**
			 * The destructor.
			 */
			~QtFlowIpBroadcaster()
			{
			}
			
			public slots:
			/**
			 * Broadcast a dtagram. 
			 * The format is QtFlow|documentName.n|hostName|port 
			 */
			void broadcastDatagram()
			{
				QByteArray datagram = "QtFlow|" + m_documentName.toLatin1() + "|" + m_host.toLatin1() + "|" + QByteArray::number(m_port);
				m_udpSocket->writeDatagram(datagram.data(), datagram.size(),
										   QHostAddress::Broadcast, BROADCAST_PORT);
				m_udpSocket->close();
			}
			
		private:
			QString m_documentName;
			QString m_host;
			int m_port;
			
			QUdpSocket* m_udpSocket;
		};
} //namespace FD
#endif /*QTFLOWIPBROADCASTER_H_*/
