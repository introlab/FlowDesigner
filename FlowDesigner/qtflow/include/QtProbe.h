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
	/**
	 * An abstract class representing a probe. It provides 
	 * basic features for the connection with a QtFlow process. 
	 * At least, the methods dataReceived(...) and setupUi must 
	 * be redefined. Some methods are virtual to provide an easy 
	 * way to add custom actions (like adding a message in the 
	 * GUI to say that the probe is connected or the process has 
	 * stopped) but inherited methods must call their parent's 
	 * method at the end.
	 * @author Mathieu Labbe
	 */
	class QtProbe : public QMainWindow
	{
		Q_OBJECT;
		
		public:
			/**
			 * The constructor.
			 * @param parent the QWidget parent
			 * @param processHost the host name of the running QtFlow process
			 * @param processPort the port of the process
			 * @param linkId the link ID used to probe it
			 */
			QtProbe(QWidget *parent, const QString &processHost, const int &processPort, const int &linkId);
			
			/**
			 * The destructor.
			 */
			~QtProbe();
			
			/**
			 * Initialize the probe. The base implementation 
			 * calls SetupUi(), creates a socket.
			 * IMPORTANT: Inherited class must calls QtProbe::init()
			 *            at the end of this method
			 */
			virtual void init();
			
			/**
			 * Stop probing. The base implementation 
			 * sends "disconnect" command to the process 
			 * and closes the socket.
			 * IMPORTANT: Inherited class must calls QtProbe::stop() 
			 *            at the end of this method.
			 */
			virtual void stop();
		
		public slots:
			/**
			 * Called when a connection with the 
			 * server is established. The base implementation 
			 * sends "connect" command with the link id.
			 * IMPORTANT: Inherited class must calls 
			 *            QtProbe::connected() at the end 
			 *            of this method.
			 */ 
		 	virtual void connected();
		 	
		 	/**
			 * Called when an error occurs with the socket. 
			 * The base implementation closes the socket.
			 * IMPORTANT: Inherited class must calls 
			 *            QtProbe::error(socketError) at the end 
			 *            of this method.
			 * @param socketError the socket error
			 */ 
		 	virtual void error(QAbstractSocket::SocketError socketError);
		 	
		 	/**
		 	 * Called when data is received from the process.
		 	 */
		 	void readyRead();
		
		protected:
			/**
			 * Called when data from the link is received.
			 * @param data the data
			 */
			virtual void dataReceived(const QByteArray &data) = 0;
		
		private:
			/**
			 * Setup the interface of the probe.
			 * Redefine this to setup the GUI.
			 */
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
