// Copyright (C) 2000 Jean-Marc Valin

#include "ExternalApp.h"


map<string, AppFactory *> &ExternalApp::factories()
{
   static map<string, AppFactory *> ret;
   return ret;
}
