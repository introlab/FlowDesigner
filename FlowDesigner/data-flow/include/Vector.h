// Copyright (C) 1999 Jean-Marc Valin
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

#ifndef GEN_TYPE_VECTOR_H
#define GEN_TYPE_VECTOR_H

#include "Object.h"
#include <vector>
#include "ObjectParser.h"

template<class T>
class Vector : public vector<T>, public Object
{
public:
   Vector() : vector<T>() {}
   Vector(int n, const T &x = T()) : vector<T>(n, x) {}
   void printOn(ostream &out) const
   {
      out << *static_cast<const vector<T> *> (this);
   }
   virtual void rawWrite(ostream &out) const
   {
      out.write ((const unsigned char*) begin(), size()*sizeof(T));
   }
};


#endif
