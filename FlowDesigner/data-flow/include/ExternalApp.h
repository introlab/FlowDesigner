// Copyright (C) 2000 Jean-Marc Valin

#include <map>
#include <string>

using namespace std;

class ExternalApp;

class AppFactory {
  public:
   virtual ExternalApp *create()=0;
};

template <class T>
class _AppFactory : public AppFactory{
  public:
   ExternalApp *create() {return new T();}
};

class ExternalApp {
   static map<string, AppFactory *> &factories();
  protected:
  public:
   static int addAppFactory(string name, AppFactory *f) {factories()[name] = f;}
   static ExternalApp *startApp(string name) {return factories()[name]->create();}
};
