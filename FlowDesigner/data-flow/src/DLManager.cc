// Copyright (C) 1999 Jean-Marc Valin

#include "DLManager.h"

map<string,LoadedLibrary* > DLManager::loaded;

LoadedLibrary *DLManager::getLib(const string &name)
{
   //cerr << "DLManager::get_lib(" << name << ")\n";
   if (loaded.find(name)==loaded.end())
   {
      loaded[name] = new LoadedLibrary (name);
   }
   return loaded[name];
}
