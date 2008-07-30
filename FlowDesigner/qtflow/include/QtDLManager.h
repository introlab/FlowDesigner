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
#ifndef _QTDLMANAGER_H_
#define _QTDLMANAGER_H_

#include <QLibrary>
#include <list>
#include <QObject>
#include <QString>

namespace FD {

	class QtDLManager : public QObject
	{
		
		Q_OBJECT;
		
	public:


		~QtDLManager();
		static QtDLManager* instance();
		static void destroy();
		static std::list<QLibrary*> &getLoadedLibraries();
		static std::list<QLibrary*> &getFailedLibraries();
		void scanDL(std::string path=std::string(INSTALL_PREFIX) + "/lib/flowdesigner/toolbox");
		
		signals:
		
		void newLoadedLibrary(QString path);		
		void newFailedLibrary(QString path);
		
		private:
		
		QtDLManager();
		static QtDLManager* m_instance;
				
	};

} //namespace FD
#endif
