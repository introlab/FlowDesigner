// Copyright (C) 2001 Jean-Marc Valin

#ifndef COMPILE_OBJECT_H
#define COMPILE_OBJECT_H

#include <string>
#include "DLManager.h"

namespace FD {

class CompileObject {
   std::string code;
   std::string symname;
   std::string filename;
   DL_HANDLE_TYPE lib;
   void *sym;
public:
   CompileObject(const std::string &_code="");
   ~CompileObject();
   void setCode(const std::string &_code) {code = _code;}
   void compile();
   void *getFuncPtr() {return sym;}
};
}//namespace FD
#endif
