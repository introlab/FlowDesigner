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

#ifndef BUFFER_H
#define BUFFER_H


#include "Object.h"
//#include "Exception.h"
#include <typeinfo>
#include "ObjectRef.h"

class Buffer : public Object {
public:
   /**Default Constructor*/
   Buffer() : currentPos(-1) {}
   
   /**Indexing operator, also sets the indexed frame as being the current frame*/
   virtual ObjectRef & operator[] (int ind) = 0;
   
   /**Indexing operator, also sets the indexed frame as being the current frame*/
   virtual ObjectRef operator[] (int ind) const = 0;
   
   /**Print*/
   virtual void printOn(ostream &out = cout) const = 0;

   ///Returns the current position in the matrix (the last valid line)
   virtual int getCurrentPos() {return currentPos;}

protected:
   ///The current pos (the newest frame)
   mutable int currentPos;

};

#endif
