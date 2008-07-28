
#include "QtProbeConsole.h"

#include <QVBoxLayout>
#include <iostream>

namespace FD
{


DECLARE_PROBE(QtProbeConsole)
		
QtProbeConsole::QtProbeConsole(QWidget *parent, const QString &processHost, const int &processPort, const int &linkId)
	: QtProbe(parent, processHost, processPort, linkId)
{
}

QtProbeConsole::~QtProbeConsole()
{
}

void QtProbeConsole::setupUi()
{
	this->setWindowTitle(tr("Link %1").arg(m_linkId));
    this->setGeometry(QRect(0, 252, 251, 150));
    
    QWidget* widgetContents = new QWidget(this);
	QVBoxLayout* layout = new QVBoxLayout(widgetContents);
	m_textBrowser = new QTextBrowser(widgetContents);
	layout->addWidget(m_textBrowser);
	this->setCentralWidget(widgetContents);
}

void QtProbeConsole::stop()
{
    m_textBrowser->append(tr("Stopped."));
	QtProbe::stop();
}

void QtProbeConsole::dataReceived(const QByteArray &data)
{
	QString info(data);
	m_textBrowser->append(info);	
}

void QtProbeConsole::connected()
{
	m_textBrowser->append(tr("Connected"));
	QtProbe::connected();
}

void QtProbeConsole::error(QAbstractSocket::SocketError socketError)
{
	switch(socketError)
	{
    	case QAbstractSocket::ConnectionRefusedError:
    		m_textBrowser->append(tr("Host unavailable."));
    		break;
    	case QAbstractSocket::RemoteHostClosedError:
    		m_textBrowser->append(tr("Connection lost."));
    		break;
    	case QAbstractSocket::HostNotFoundError:
    		m_textBrowser->append(tr("Host not found."));
    		break;
    	default:
    		m_textBrowser->append(tr("QTcpSocket: Unknown error occured!"));
    		break;
	}
	
	QtProbe::error(socketError);
}

} //namespace FD