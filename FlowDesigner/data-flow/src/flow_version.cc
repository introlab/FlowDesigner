// Copyright (C) 2001 Jean-Marc Valin

#include "flow_version.h"
#include <string>
#include <iostream>

using namespace std;

int version_check(const char *vers)
{
   static string lib_version = OVERFLOW_VERSION;
   if (lib_version!=vers)
   {
      cerr << "Version mismatch: trying to link libflow version " << lib_version << " with code compiled for version " << vers << endl;
      exit(3);
   }
   return 1;
}

