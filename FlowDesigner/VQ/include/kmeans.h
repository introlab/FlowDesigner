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

#ifndef KMEANS_H
#define KMEANS_H

#include <vector>
#include <stream.h>
#include "Object.h"

class KMeans : public Object {
protected:
   int length;
   vector<vector<float> > means;
   //vector<int> accum;
   float (*dist)(const float *, const float*, int);

   static float euclidian (const float *x, const float *y, int len)
   {
      float val=0;
      for (int i=0;i<len;i++)
         val += (x[i]-y[i])*(x[i]-y[i]);
      //cerr << "returned " << val << endl;
      return val;
   }

public:
   KMeans (float (*_dist)(const float *, const float*, int) = euclidian)
      : dist(_dist)
   {}

   const vector<float> &operator[] (int i) const {return means[i];}

   int split (const vector<float *> &data, int len);

   int bsplit ();

   void update (const vector<float *> &data, int len);

   void train (int codeSize, const vector<float *> &data, int len, bool binary=false);

   int getClassID (const float *v, float *dist_return = NULL) const;

   void printOn(ostream &out=cout) const;
   void readFrom (istream &in=cin);
   friend istream &operator >> (istream &in, KMeans &mdl);
};

#endif
