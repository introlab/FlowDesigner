// Copyright (C) 2000 Jean-Marc Valin
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

#include <map>
#include <string>

class ExternalApp;

class AppFactory {
  public:
   virtual ExternalApp *create()=0;
};

template <class T>
class _AppFactory : public AppFactory{
  public:
   ExternalApp *create() {return new T();}
};

class ExternalApp {
   static map<string, AppFactory *> &factories();
  protected:
  public:
   static int addAppFactory(string name, AppFactory *f) {factories()[name] = f;}
   
};
