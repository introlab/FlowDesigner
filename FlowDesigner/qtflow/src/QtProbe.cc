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
#include "QtProbe.h"

#include <iostream>

namespace FD
{
	
	
int probe_id_count = 0;	
		
QtProbe::QtProbe(QWidget *parent, const QString &processHost, const int &processPort, const int &linkId) : 
	QMainWindow(parent), 
	m_socket(NULL), 
	m_linkId(linkId) ,
	m_processHost(processHost),
	m_processPort(processPort)
	
{
}

QtProbe::~QtProbe()
{
	if (m_socket)
    {
		this->stop();
	    delete m_socket;
    }
}

void QtProbe::init()
{
	//TODO add m_initialized member to know if it is initialized
	
	setupUi();
	
	// Delete on close
	this->setAttribute(Qt::WA_DeleteOnClose);
		
	m_socket = new QTcpSocket(this);
	m_socket->connectToHost(m_processHost,m_processPort);
	
	connect(m_socket,SIGNAL(connected()),this,SLOT(connected()));
	connect(m_socket,SIGNAL(readyRead()),this,SLOT(readyRead()));
	connect(m_socket,SIGNAL(error(QAbstractSocket::SocketError)),this,SLOT(error(QAbstractSocket::SocketError)));
}
   
void QtProbe::stop()
{
	if(m_socket->state() == QAbstractSocket::ConnectedState) {
   	    QString buf = QString("disconnect %1\n").arg(m_linkId);     	
	    m_socket->write(buf.toStdString().c_str(), buf.size());
	    m_socket->flush();
	}
	m_socket->close();
}

void QtProbe::readyRead ()
{
	//Try to read lines
	int size = m_socket->bytesAvailable();
	QByteArray data = m_socket->read(size);
	dataReceived(data);
}

void QtProbe::connected()
{
	QString buf = QString("connect %1\n").arg(m_linkId);     	
	m_socket->write(buf.toStdString().c_str(), buf.size());
}

void QtProbe::error(QAbstractSocket::SocketError socketError)
{	
	if(m_socket)
	{
    	m_socket->close();
	}
}

} //namespace FD