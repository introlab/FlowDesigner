#include "QtDLManager.h"
#include <vector>
#include "path.h"
#include <iostream>

namespace FD
{
	using namespace std;
	
	std::list<QLibrary*> & QtDLManager::getLoadedLibraries()
	{
		static std::list<QLibrary*> m_libraries;
		return m_libraries;
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
				}
				else
				{
					cerr<<"... FAILED. "<<"Error message "<<mylib->errorString().toStdString()<<endl;
					delete mylib;
				}
			}
			catch(...)
			{
				cerr<<"Exception caught while loading library  : "<<libs[i]<<endl;
			}
			
		}
		
	}

}//namespace FD

