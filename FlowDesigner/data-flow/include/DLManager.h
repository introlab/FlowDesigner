// Copyright (C) 1999 Jean-Marc Valin

#ifndef DLMANAGER_H
#define DLMANAGER_H



#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "path.h"
#include <map>
#include <vector>
#include <string>
#include <stdio.h>

#include <errno.h>
#include "Exception.h"

//#include "rc_ptrs.h"

//#define HPUX

#ifdef HAVE_DLFCN_H
#include <dlfcn.h>

namespace FD {

/**The pointer to library type (OS dependent)*/
typedef void *DL_HANDLE_TYPE;

/**How to open a library*/
inline DL_HANDLE_TYPE _DL_OPEN(std::string path, int debug = 1) 
{
   //std::cerr << "opening lib " << path.c_str() << std::endl;
   DL_HANDLE_TYPE library = dlopen (path.c_str(), RTLD_LAZY|RTLD_GLOBAL);
   if (!library && debug) 
      std::cerr << "Toolbox load error: " << dlerror() << std::endl;

   return library;
}

/**How to search for a specific symbol */
inline void * _DL_GET_SYM(DL_HANDLE_TYPE lib, const std::string &symbol) 
{
   return dlsym (lib, symbol.c_str());
}

/**How to close a library*/
inline void _DL_CLOSE(DL_HANDLE_TYPE lib) 
{
   dlclose(lib);
}

#elif defined (HAVE_DL_H)

#include <dl.h>

typedef shl_t DL_HANDLE_TYPE;
inline DL_HANDLE_TYPE _DL_OPEN(string path, int debug = 0)
{
   //cerr << "_DL_OPEN(" << path.c_str() << ") \n";
   DL_HANDLE_TYPE library = shl_load (path.c_str(), BIND_IMMEDIATE, 0);
   //cerr << "library = " << library << endl;
   if (!library && debug) 
      perror ("Load error");
   return library;
}
inline void * _DL_GET_SYM(DL_HANDLE_TYPE lib, std::string symbol) 
{
   void *tmp;
   shl_findsym (&lib, symbol.c_str(), TYPE_PROCEDURE, &tmp);
   return tmp;
}
inline void _DL_CLOSE(DL_HANDLE_TYPE lib) 
{
   shl_unload (lib);
}


#endif


/**Class for a dynamically loaded library*/
class LoadedLibrary {

   /**The library pointer as defined by the OS*/
   DL_HANDLE_TYPE lib;

   /**How many times is the library used ("opened" by DLManager)*/
   int count;

public:
   /**Default constructor (takes the path to the shared library)*/
   LoadedLibrary(const std::string &path) 
      : lib(_DL_OPEN(path))
      , count(1)
   {if (!lib) throw new GeneralException(std::string("couldn't load library ")+path,__FILE__,__LINE__);}
   
   /**returns a pointer to the function named 'symbol'*/
   void *get_proc (std::string symbol) 
   {return _DL_GET_SYM(lib,symbol);}
   
   /**Destructor*/
   ~LoadedLibrary()
   {
      //perform some ref counting here
      //_DL_CLOSE (lib);
   }
};



/**Class that manages the loading of shared libraries so that they don't get loaded twice*/
class DLManager {

   /**a list (STL map) of loaded libraries indexed by name (path)*/
   static std::map<std::string,LoadedLibrary* > loaded;

public:

   /**Returns a pointer to a Library specified by 'name' 
      (loads it if it hasn't been done before)*/
   static LoadedLibrary *getLib(const std::string &name);


};

class ToolboxData {
   std::string fullname;
   DL_HANDLE_TYPE handle;
  public:
   ToolboxData() {}
   ToolboxData(std::string _fullname, DL_HANDLE_TYPE _handle)
      : fullname(_fullname)
      , handle(_handle)
      {}
};

class ToolboxList {
   static std::map<std::string, ToolboxData> loadedToolboxes;
  public:
   static std::vector<std::string> load(const std::vector<std::string> &list, int debug);
};

}//namespace FD

#endif
