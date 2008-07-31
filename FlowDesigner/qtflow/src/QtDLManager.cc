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
#include "QtDLManager.h"
#include <vector>
#include "path.h"
#include <iostream>
#include <QApplication>

namespace FD
{
	using namespace std;
	
	//Initialize static instance pointer (singleton) to NULL
	QtDLManager* QtDLManager::m_instance = NULL;
	
	QtDLManager* QtDLManager::instance()
	{
		if (!QtDLManager::m_instance)
		{
			QtDLManager::m_instance = new QtDLManager();
		}
		return QtDLManager::m_instance;
	}
	
	void QtDLManager::destroy()
	{
		if (QtDLManager::m_instance)
		{
			delete QtDLManager::m_instance;
			QtDLManager::m_instance = NULL;
		}
	}
	
	
	std::list<QLibrary*> & QtDLManager::getLoadedLibraries()
	{
		static std::list<QLibrary*> m_LoadedLibraries;
		return m_LoadedLibraries;
	}
	
	std::list<QLibrary*> & QtDLManager::getFailedLibraries()
	{
		static std::list<QLibrary*> m_FailedLibraries;
		return m_FailedLibraries;
	}
	
	
	QtDLManager::QtDLManager()
	{

	}
	
	QtDLManager::~QtDLManager()
	{
		//Memory cleanup
		while(!QtDLManager::getLoadedLibraries().empty())
		{
			delete QtDLManager::getLoadedLibraries().front();
			QtDLManager::getLoadedLibraries().pop_front();
		}
		
		while(!QtDLManager::getFailedLibraries().empty())
		{
			delete QtDLManager::getFailedLibraries().front();
			QtDLManager::getFailedLibraries().pop_front();
		}
	}
	
	void QtDLManager::scanDL(std::string path)
	{
		vector<string> libs;
		
		//looking recursively in path for toolboxes
		recursiveScanDL(path,libs,true);
		
		//loading toolboxes
		for (unsigned int i =0; i < libs.size(); i++)
		{
			cerr<<"Loading : "<<libs[i];
			try {
				QLibrary *mylib = new QLibrary(libs[i].c_str());
				
				mylib->load();
				
				if (mylib->isLoaded())
				{
					cerr<<" ... OK"<<endl;
					QtDLManager::getLoadedLibraries().push_back(mylib);
					emit newLoadedLibrary(QString(libs[i].c_str()));
					
					//Process events
					if (qApp)
					{
						qApp->processEvents();
					}
				}
				else
				{
					cerr<<"... FAILED. "<<"Error message : "<<mylib->errorString().toStdString()<<endl;
					QtDLManager::getFailedLibraries().push_back(mylib);
					emit newFailedLibrary(QString(libs[i].c_str()));
										
					//Process events
					if (qApp)
					{
						qApp->processEvents();
					}
				}
			}
			catch(...)
			{
				cerr<<"Exception caught while loading library  : "<<libs[i]<<endl;
			}
			
		}
		
	}

}//namespace FD

