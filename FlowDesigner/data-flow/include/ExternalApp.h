// Copyright (C) 2000 Jean-Marc Valin
#ifndef _EXTERNALAPP_H_
#define _EXTERNALAPP_H_

#include <map>
#include <string>

namespace FD {
	
	class ExternalApp;
	
	class AppFactory {
	public:
		virtual ExternalApp *create()=0;
		virtual ~AppFactory(){;}
	};
	
	template <class T>
	class _AppFactory : public AppFactory{
	public:
		ExternalApp *create() {return new T();}
	};
	
	class ExternalApp {
		static std::map<std::string, AppFactory *> &factories();
	protected:
	public:
		static void addAppFactory(std::string name, AppFactory *f) {factories()[name] = f;}
		static ExternalApp *startApp(std::string name) {return factories()[name]->create();}
	};
	
}//namespace FD
#endif
