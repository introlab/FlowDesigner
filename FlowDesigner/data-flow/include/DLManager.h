// Copyright (C) 1999 Jean-Marc Valin

#ifndef DLMANAGER_H
#define DLMANAGER_H

using namespace std;

#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#include "path.h"
#include <map>
#include <string>
#include <stdio.h>

#include <errno.h>
#include "Exception.h"

//#include "rc_ptrs.h"

//#define HPUX

#ifdef HPUX
#include <dl.h>

typedef shl_t DL_HANDLE_TYPE;
inline DL_HANDLE_TYPE _DL_OPEN(string path) 
{
   //cerr << "_DL_OPEN(" << path.c_str() << ") \n";
   DL_HANDLE_TYPE library = shl_load (path.c_str(), BIND_IMMEDIATE, 0);
   //cerr << "library = " << library << endl;
   if (!library) 
      perror ("Load error");
   return library;
}
inline void * _DL_GET_SYM(DL_HANDLE_TYPE lib, string symbol) 
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


#if defined (LINUX) || defined(SOLARIS) || defined(FREEBSD)
#include <dlfcn.h>

/**The pointer to library type (OS dependent)*/
typedef void *DL_HANDLE_TYPE;

/**How to open a library*/
inline DL_HANDLE_TYPE _DL_OPEN(string path) 
{
   //cerr << "opening lib " << path.c_str() << endl;
   DL_HANDLE_TYPE library = dlopen (path.c_str(), RTLD_LAZY);
   if (!library) 
      cerr << "Toolbox load error: " << dlerror() << endl;

   return library;
}

/**How to search for a specific symbol */
inline void * _DL_GET_SYM(DL_HANDLE_TYPE lib, const string &symbol) 
{
   return dlsym (lib, symbol.c_str());
}

/**How to close a library*/
inline void _DL_CLOSE(DL_HANDLE_TYPE lib) 
{
   dlclose(lib);
}

#endif /*LINUX or SOLARIS*/

/**Class for a dynamically loaded library*/
class LoadedLibrary {

   /**The library pointer as defined by the OS*/
   DL_HANDLE_TYPE lib;

   /**How many times is the library used ("opened" by DLManager)*/
   int count;

public:
   /**Default constructor (takes the path to the shared library)*/
   LoadedLibrary(const string &path) 
      : lib(_DL_OPEN(path))
      , count(1)
   {if (!lib) throw new GeneralException(string("couldn't load library ")+path,__FILE__,__LINE__);}
   
   /**returns a pointer to the function named 'symbol'*/
   void *get_proc (string symbol) 
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
   static map<string,LoadedLibrary* > loaded;

public:

   /**Returns a pointer to a Library specified by 'name' 
      (loads it if it hasn't been done before)*/
   static LoadedLibrary *getLib(const string &name);


};




#endif
