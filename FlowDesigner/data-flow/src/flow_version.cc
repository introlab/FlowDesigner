// Copyright (C) 2001 Jean-Marc Valin

#include "flow_version.h"
#include <string>
#include <iostream>

using namespace std;

int version_check(const char *vers, const char *abi_vers, const char *unused_vers)
{
   static string lib_version = FLOWDESIGNER_VERSION;
   static string abi_version = FLOWDESIGNER_ABI_VERSION;
   if (lib_version!=vers)
   {
      cerr << "Version mismatch: trying to link libflow version " << lib_version << " with code compiled for version " << vers << endl;
      exit(3);
   } else if (abi_version != abi_vers) 
   {
      cerr << "FlowDesigner ABI version mismatch: trying to link libflow with ABI version " << abi_version << " with code compiled for ABI version " << abi_vers << ". ";
      cerr << "This means that you're likely using a development version, so you should know what this means." << endl;
      exit(3);
   } else if (unused_vers)
   {
      cerr << "Strange, unused version string set. I guess this means your version of libflow is too old or something really evil happened" << endl;
      exit(3);
   }
   return 1;
}

