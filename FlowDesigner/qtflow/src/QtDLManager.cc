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
		if (QtDLManager::m_instance)
		{
			return QtDLManager::m_instance;
		}
		else
		{
			return new QtDLManager();
		}
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

