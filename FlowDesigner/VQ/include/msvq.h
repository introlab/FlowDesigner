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

#ifndef MSVQ_H
#define MSVQ_H

#include "kmeans.h"
#include "Object.h"
#include <vector>

/**This is a multi-stage vector quantization class, and has nothing to do with microsoft :-)*/
class MSVQ : public VQ {
  protected:
   vector<int> stagesSizes;
   vector<KMeans> stages;
  public:
   MSVQ(float (*_dist)(const float *, const float*, int) = KMeans::euclidian)
      : VQ(_dist)
      {}
   MSVQ(const vector<int> &_stagesSizes, float (*_dist)(const float *, const float*, int) = KMeans::euclidian);

   int ID2Vec(const vector<int> &vec) const;

   vector<int> Vec2ID(int ID) const;

   int nbClasses() const;

   int nbStages() const {return stagesSizes.size();}

   //const vector<float> &operator[] (int i) const;

   void train (int codeSize, const vector<float *> &data, int len, bool binary=false);

   int getClassID (const float *v, float *dist_return = NULL) const;

   //void calcDist (const float *v, float *dist_return) const;

   void printOn(ostream &out=cout) const;
   void readFrom (istream &in=cin);
   friend istream &operator >> (istream &in, MSVQ &mdl);
};
#endif
