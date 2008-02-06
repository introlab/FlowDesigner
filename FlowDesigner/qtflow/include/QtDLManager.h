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
