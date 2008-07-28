
#include "QtProbe.h"

#include <iostream>

namespace FD
{
	
	
int probe_id_count = 0;	
		
QtProbe::QtProbe(QWidget *parent, const QString &processHost, const int &processPort, const int &linkId) : 
	QMainWindow(parent), 
	m_processHost(processHost), 
	m_processPort(processPort), 
	m_socket(NULL), 
	m_linkId(linkId)
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