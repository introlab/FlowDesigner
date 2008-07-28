#ifndef QTPROBE_H_
#define QTPROBE_H_

#include <QVector>
#include <QString>
#include <QMainWindow>
#include <QAbstractSocket>
#include <QTcpSocket>

#include "QtProbeRegistry.h"

namespace FD
{
	class QtProbe : public QMainWindow
	{
		Q_OBJECT;
		
		public:
			QtProbe(QWidget *parent, const QString &processHost, const int &processPort, const int &linkId);
			~QtProbe();
			
			// inherited class must calls QtProbe::init() at the end of this method
			virtual void init();
			// inherited class must calls QtProbe::stop() at the end of this method
			virtual void stop();
		
		public slots:
			// inherited class must calls QtProbe::connected() at the end of this method
		 	virtual void connected();
		 	// inherited class must calls QtProbe::error(socketError) at the end of this method
		 	virtual void error(QAbstractSocket::SocketError socketError);
		 	void readyRead();
		
		protected:
			virtual void dataReceived(const QByteArray &data) = 0;
		
		private:
			// Redefine this to setup the GUI
			virtual void setupUi() = 0;
		 	
		protected:
			QTcpSocket *m_socket;
			int m_linkId;
		
		private:
			QString m_processHost;
			int m_processPort;
		
	};

} //namespace FD

#endif /*QTPROBE_H_*/
