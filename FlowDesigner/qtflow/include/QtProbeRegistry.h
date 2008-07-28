#ifndef _QT_PROBE_REGISTRY_H_
#define _QT_PROBE_REGISTRY_H_

#include <QObject>
#include <QString>
#include <QStringList>
#include <QVector>
#include <map>
#include <sys/types.h>
#include <dirent.h>
#include <sys/stat.h>
#include <vector>
#include <string>
#include <iostream>
#include <QLibrary>

namespace FD
{

	//Forward delaration
	class QtProbe;
		
	class QtProbeBaseFactory
	{
		public:
			void addAllowedDataType(const QString &dataType)
			{
				if(!m_dataTypes.contains(dataType)) {
					m_dataTypes.append(dataType);
				}
			}
			
			bool canProbe(const QString &dataType)
			{
				// All data types allowed
				if(m_dataTypes.size() == 0) {
					return true;
				}
				
				for(int i=0; i<m_dataTypes.size(); i++) {
					if(m_dataTypes[i].compare(dataType) == 0) {
						return true;
					}
				}
				return false;
			}
			
			virtual QtProbe* create(QWidget *parent, const QString &processHost, const int &processPort, const int &linkId) = 0;
		
		private:
			QVector<QString> m_dataTypes;
	};
	
	template <class T>
	class QtProbeFactory : public QtProbeBaseFactory
	{
		public:
			QtProbeFactory() {}
			~QtProbeFactory() {}
			virtual QtProbe* create(QWidget *parent, const QString &processHost, const int &processPort, const int &linkId)
			{
				return dynamic_cast<QtProbe*>(new T(parent, processHost, processPort, linkId));
			}
	};
	
	
	class QtProbeRegistry : public QObject
	{
		Q_OBJECT;
		public:
		
		QtProbeRegistry()
		{
			QtProbeRegistry::Scan(QString(INSTALL_PREFIX) + QString("/lib/flowdesigner/toolbox"),true);
		}
		
		~QtProbeRegistry()
		{
			std::vector<QLibrary*> &failedLibraries = getFailedLibraries();
			std::vector<QLibrary*> &loadedLibraries = getLoadedLibraries();
			
			for (unsigned int i = 0; i < failedLibraries.size(); i++)
			{
				delete failedLibraries[i];
			}
			failedLibraries.resize(0);
			
			for (unsigned int i = 0; i < loadedLibraries.size(); i++)
			{
				delete loadedLibraries[i];
			}
			loadedLibraries.resize(0);
			
			//Delete factories
			std::map<QString, QtProbeBaseFactory*>::iterator iter = getFactoryMap().begin();
			while (iter != getFactoryMap().end()) {
				if(iter->second) {
	            	delete iter->second;
	         	}
	            iter++;
			}
			getFactoryMap().clear();
		}	
		
		static int registerFactory(const QString &probeName, QtProbeBaseFactory *factory)
		{
			getFactoryMap().insert(std::make_pair(probeName,factory));
			return 0;
		}
		
		static int addAllowedDataType(const QString &probeName, const QString &dataType)
		{
			if (getFactoryMap().find(probeName) != getFactoryMap().end())
			{
				getFactoryMap()[probeName]->addAllowedDataType(dataType);
			}
			return 0;
		}
		
		static QVector<QString> getAllowedProbe(const QString &dataType)
		{
			QVector<QString> probes;
			std::map<QString, QtProbeBaseFactory*>::iterator iter = getFactoryMap().begin();
			while (iter != getFactoryMap().end()) {
				if(iter->second->canProbe(dataType)) {
	            	probes.append(iter->first);
	         	}
	            iter++;
			}
			return probes;
		}
		
		static QtProbe* createProbe(const QString &probeName, QWidget *parent, const QString &processHost, const int &processPort, const int &linkId)
		{
			if (getFactoryMap().find(probeName) != getFactoryMap().end())
			{
				return getFactoryMap()[probeName]->create(parent, processHost, processPort, linkId);
			}
			else
			{
				return NULL;
			}
		}
		
	private:
		
		static std::map<QString,QtProbeBaseFactory*>& getFactoryMap()
		{
			static std::map<QString,QtProbeBaseFactory*> myProbeFactory;
			return myProbeFactory;
		}
		
		static void recursiveScan(const std::string &path, std::vector<std::string> &fileList, bool debug = false)
		{
			
			if (debug)
			{
				std::cerr<<"QtProbeRegistry::recursiveScan on path : "<<path<<std::endl;
			}
			
			DIR *my_directory = opendir (path.c_str());
			
			if (!my_directory) {
				perror((std::string("error opening directory ") + path).c_str());
				return;
			}
			
			struct dirent *current_entry;
			
			for (current_entry = readdir(my_directory); 
				 current_entry != NULL; current_entry = readdir(my_directory)) {
				
				struct stat my_stat;
				std::string name = current_entry->d_name;

				std::string fullpath = path + "/" + name;
				
				//is it a directory, if so let's scan it...
				if (stat(fullpath.c_str(), &my_stat) < 0) {	    
					perror(fullpath.c_str());
					continue;
				}
				
				if (S_ISDIR(my_stat.st_mode)) {
					//it is a directory, let's doing it recursively
					if (name != std::string("..") && name != std::string(".")) {
						recursiveScan(fullpath, fileList,debug);
					}
				}
				else {
					//this is a standard file, look for the .tlb extension
#ifndef WIN32	  
					if (name.find(".probe") != std::string::npos) {	    
#else
						if (name.find(".probe") != std::string::npos && name.find(".a") == std::string::npos){
#endif
							
							if (debug) {
								std::cerr << "Found " << fullpath << std::endl;
							}   
							fileList.push_back(fullpath);
						}
					}       
				}//for all entries
				
				closedir(my_directory);
				
		}
		
		static std::vector<QLibrary*>& getLoadedLibraries()
		{
			static std::vector<QLibrary*> myLoadedLibraries;
			return myLoadedLibraries;
		}
		
		static std::vector<QLibrary*>& getFailedLibraries()
		{
			static std::vector<QLibrary*> myFailedLibraries;
			return myFailedLibraries;
		}
			
		static void Scan(const QString & path, bool debug = false)
		{
			std::vector<std::string> libs;
			
			recursiveScan(path.toStdString(),libs,debug);
			
			//loading toolboxes
			for (unsigned int i =0; i < libs.size(); i++)
			{
				std::cerr<<"QtProbeRegistry::Scan Loading : "<<libs[i];
				try {
					
					QLibrary *mylib = new QLibrary(libs[i].c_str());
					
					mylib->load();
					
					if (mylib->isLoaded())
					{
						std::cerr<<" ... OK"<<std::endl;
						getLoadedLibraries().push_back(mylib);
					}
					else
					{
						std::cerr<<"... FAILED. "<<"Error message : "<<mylib->errorString().toStdString()<<std::endl;
						getFailedLibraries().push_back(mylib);
					}
				}
				catch(...)
				{
					std::cerr<<"Exception caught while loading library  : "<<libs[i]<<std::endl;
				}
			}
		}
	};
		
	#define DECLARE_PROBE(ProbeClass) static int dummy_probe_initializer_for_ ## ProbeClass = \
		QtProbeRegistry::registerFactory(# ProbeClass, new QtProbeFactory<ProbeClass>());
	
	static int count = 0; // Used to generate unique dummy initializer name
	#define DECLARE_PROBE_ALLOWED_DATA_TYPE(ProbeClass, DataType) static int dummy_probe_initializer_for_ ## ProbeClass ## count = \
		QtProbeRegistry::addAllowedDataType(# ProbeClass, DataType) + count++;
}
#endif




