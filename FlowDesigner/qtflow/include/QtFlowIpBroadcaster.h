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

class QtFlowIpBroadcaster : public QObject
{
	Q_OBJECT;
	
public:
	static const int BROADCAST_PORT = 51515;
	static const int BROADCAST_INTERVAL_MS = 3000; //3 sec

public:
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
	
	~QtFlowIpBroadcaster()
	{
	}
	
public slots:	
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

#endif /*QTFLOWIPBROADCASTER_H_*/
