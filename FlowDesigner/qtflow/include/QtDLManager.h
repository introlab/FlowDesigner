#ifndef _QTDLMANAGER_H_
#define _QTDLMANAGER_H_

#include <QLibrary>
#include <list>

namespace FD {

	class QtDLManager 
	{
		public:
		
		static std::list<QLibrary*> &getLoadedLibraries();
		
		static void scanDL(std::string path=std::string(INSTALL_PREFIX) + "/toolbox");
		
		private:
		
		QtDLManager();
				
	};

} //namespace FD
#endif
