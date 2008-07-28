#ifndef QTPROBECONSOLE_H_
#define QTPROBECONSOLE_H_

#include <QTextBrowser>

#include "QtProbe.h"

namespace FD
{
	class QtProbeConsole;
	DECLARE_PROBE(QtProbeConsole)

	class QtProbeConsole : public QtProbe
	{
		Q_OBJECT;
		
		public:
			QtProbeConsole(QWidget *parent, const QString &processHost, const int &processPort, const int &linkId);
			~QtProbeConsole();
			
			// inherited class must calls QtProbe::stop() at the end of this method
			virtual void stop();
			
		public slots:
			// inherited class must calls QtProbe::connected() at the end of this method
		 	virtual void connected(); 
		 	// inherited class must calls QtProbe::error(socketError) at the end of this method
		 	virtual void error(QAbstractSocket::SocketError socketError);
		 	
		protected:
			virtual void dataReceived(const QByteArray &data);
			
		private:
			virtual void setupUi();
			QTextBrowser *m_textBrowser;
	};
	
} //namespace FD

#endif /*QTPROBECONSOLE_H_*/
