// Copyright (C) 2001 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#ifndef WRAPPER_H
#define WRAPPER_H

using namespace std;

#include "UIDocument.h"
#include "Network.h"
#include "ParameterSet.h"

class OFWrapper {
   UIDocument *doc;
   Network *net;

public:
   OFWrapper();
   ~OFWrapper();
   OFWrapper(string name);
   void open(string name);
   void init(const ParameterSet &params);
   ObjectRef process(ObjectRef in);
};




#endif
