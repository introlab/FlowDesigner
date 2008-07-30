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
