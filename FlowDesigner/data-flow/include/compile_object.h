// Copyright (C) 2001 Jean-Marc Valin

#ifndef COMPILE_OBJECT_H
#define COMPILE_OBJECT_H

#include <string>
#include "DLManager.h"

class CompileObject {
   string code;
   string symname;
   string filename;
   DL_HANDLE_TYPE lib;
   void *sym;
public:
   CompileObject(const string &_code);
   ~CompileObject();
   void compile();
   void *getFuncPtr() {return sym;}
};

#endif
