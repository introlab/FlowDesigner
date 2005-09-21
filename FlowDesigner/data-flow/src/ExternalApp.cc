// Copyright (C) 2000 Jean-Marc Valin

#include "ExternalApp.h"

using namespace std;

namespace FD {

map<string, AppFactory *> &ExternalApp::factories()
{
   static map<string, AppFactory *> ret;
   return ret;
}

}//namespace FD
