// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau
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

#ifndef _NODEFACTORY_H_
#define _NODEFACTORY_H_

/*
  author: Dominic Letourneau
  date  : 09/06/99 
 */

#include "Node.h"


//abstract factory class
class _NodeFactory {
   
public:
   
   virtual Node* Create(const string &name, const ParameterSet &parameters) = 0;
   virtual ~_NodeFactory() {;}

private:


};


//Template class used by all Nodes
template <class T>
class NodeFactory : public _NodeFactory {
public:
   virtual Node* Create(const string &name, const ParameterSet &parameters) {
      return ((Node*) new T(name,parameters));
   }
};

#endif
