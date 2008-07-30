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
#ifndef QTPROBECONSOLE_H_
#define QTPROBECONSOLE_H_

#include <QTextBrowser>

#include "QtProbe.h"

namespace FD
{

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
