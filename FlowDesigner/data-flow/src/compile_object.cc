// Copyright (C) 2001 Jean-Marc Valin

#include "compile_object.h"

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
   lib = _DL_OPEN(filename);
   sym = _DL_GET_SYM(lib, symname);
}
