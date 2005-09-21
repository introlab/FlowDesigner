// Copyright (C) 2001 Jean-Marc Valin

#include "compile_object.h"
#include <fstream>

using namespace std;

namespace FD {

CompileObject::CompileObject(const string &_code)
   : code(_code)
{
}

CompileObject::~CompileObject()
{
   _DL_CLOSE(lib);
}

void CompileObject::compile()
{
   filename = "tata";
   string cppname(filename + ".cpp");
   string soname(filename + ".so");
   symname = "func";
   {
      ofstream of(cppname.c_str());
      of << code << endl;
   }
   string cmd = "g++ -shared -o " + soname + " " + cppname + " -lm";
   system(cmd.c_str());
   //lib = _DL_OPEN(soname);
   lib = _DL_OPEN("./tata.so");
   if (!lib)
      throw new GeneralException("Cannot open CompileObject library: dlopen failed", __FILE__, __LINE__);
   sym = _DL_GET_SYM(lib, symname);
   if (!sym)
      throw new GeneralException("Cannot get symbol in CompileObject library", __FILE__, __LINE__);
}

}//namespace FD
