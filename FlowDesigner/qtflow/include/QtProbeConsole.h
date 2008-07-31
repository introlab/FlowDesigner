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
	/**
	 * A probe console. It shows the raw 
	 * data received from a link. It works 
	 * with all link types.
	 */
	class QtProbeConsole : public QtProbe
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
			QtProbeConsole(QWidget *parent, const QString &processHost, const int &processPort, const int &linkId);
			
			/**
			 * The destructor.
			 */
			~QtProbeConsole();
			
			/**
			 * Add a message that the process is 
			 * stopped in the console.
			 */
			virtual void stop();
			
		public slots:
			/**
			 * Add a message that the probe is 
			 * connected in the console.
			 */
		 	virtual void connected(); 
		 	
		 	/**
			 * Add an error message to the console.
			 */
		 	virtual void error(QAbstractSocket::SocketError socketError);
		 	
		protected:
			/**
			 * Just append the data to the console.
			 */
			virtual void dataReceived(const QByteArray &data);
			
		private:
			/**
			 * Setup the interface of the probe.
			 */
			virtual void setupUi();
			
		private:
			QTextBrowser *m_textBrowser;
	};
	
} //namespace FD

#endif /*QTPROBECONSOLE_H_*/
