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

#ifndef CODEBOOKMAP_H
#define CODEBOOKMAP_H

#include <math.h>
#include <vector>
#include <iostream>
#include "Object.h"
#include "vq.h"
#include "Vector.h"

class CodebookMap;

ostream &operator << (ostream &out, const CodebookMap &cell);


class CodebookMap : public Object {
protected:
   RCPtr<VQ> mapIn;
   
   Vector<Vector<float> > mapOut;

public:
   //CodebookMap() 
   //{}

   CodebookMap(){}
   
   CodebookMap (const CodebookMap &) {cerr << "don't call the CodebookMap copy constructor\n"; exit(1);}

   CodebookMap(const RCPtr<VQ> &_mapIn, const vector<float *> dataIn, const vector<float *> dataOut, int length);

   ~CodebookMap()
   {
   }

   
   const float * calcOutput(const float *in) const;

   void printOn(ostream &out) const;

   void readFrom (istream &in);

   friend istream &operator >> (istream &in, CodebookMap &cell);
};

#endif
