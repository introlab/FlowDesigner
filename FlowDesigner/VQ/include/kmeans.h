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
#include <iostream>
#include "Object.h"
#include "vq.h"

class KMeans : public VQ {
protected:
   //int length;
   vector<vector<float> > means;
   //vector<int> accum;

public:
   KMeans (float (*_dist)(const float *, const float*, int) = euclidian)
      : VQ(_dist)
   {}

   int nbClasses() const {return means.size();}

   const vector<float> &operator[] (int i) const;

   int split (const vector<float *> &data, int len);

   int bsplit ();

   void update (const vector<float *> &data, int len);

   void train (int codeSize, const vector<float *> &data, int len, bool binary=false);

   int getClassID (const float *v, float *dist_return = NULL) const;
   virtual void calcDist (const float *v, float *dist_return) const;

   void weightMeans (const vector<float> &w, vector<float> &out) const;
   
   void printOn(ostream &out=cout) const;
   void readFrom (istream &in=cin);
   friend istream &operator >> (istream &in, KMeans &mdl);
};

#endif
