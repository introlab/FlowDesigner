// Copyright (C) 2001 Jean-Marc Valin

#include "vmethod.h"

//SymbolSet *symbols = new SymbolSet;
using namespace std;

VirtualMethods* vmethod()
{
   static VirtualMethods *vt = new VirtualMethods;
   return vt;
}
