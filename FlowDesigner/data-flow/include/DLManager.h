// Copyright (C) 1998-1999 Jean-Marc Valin & Dominic Letourneau
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#ifndef DLMANAGER_H
#define DLMANAGER_H

#include <map>
#include <string>

#ifdef HPUX

#include <dl.h>
#include <errno.h>
#include "rc_ptrs.h"

class LoadedLibrary {
   shl_t lib;
   int count;
   LoadedLibrary(const string &path) 
      : lib(shl_load (path, BIND_IMMEDIATE, 0))
      , count(1)
   {if (!lib) throw string("couldn't load library");}
   void *get_proc (string symbol) 
   {
      void *tmp;
      shl_find_sym (lib, symbol, TYPE_PROCEDURE, &tmp);
      return tmp;
   }
   ~LoadedLibrary()
   {
      shl_unload (lib);
   }
};



#endif

class DLManager {
   map<string,LoadedLibrary* > loaded;
public:
   LoadedLibrary *get_lib(string name)
   {
      if (loaded.find(name)==loaded.end())
      {
         loaded[name] = new LoadedLibrary (name);
      }
      return loaded[name];
   }
};




#endif
