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

#ifndef RBF_H
#define RBF_H

#include <vector>
#include <stream.h>
#include "Object.h"
#include "kmeans.h"

class RBF : public KMeans {
  public:
   static float mahalanobis (const float *x, const float *c, const float *y, int len)
   {
      float sum1=0,sum2=0,sum3=0,sum4=0;
      const float *end = x+len;
      while (x<end-3)
      {
	 sum1 += *c++ *(*x-*y)*(*x-*y);
	 x++; y++;
	 sum2 += *c++ *(*x-*y)*(*x-*y);
	 x++; y++;
	 sum3 += *c++ *(*x-*y)*(*x-*y);
	 x++; y++;
	 sum4 += *c++ *(*x-*y)*(*x-*y);
	 x++; y++;
      }
       while (x<end)
       {
	 sum1 += *c++ *(*x-*y)*(*x-*y);
	 x++; y++;
       }
      return (sum1+sum2)+(sum3+sum4);
   }

protected:
   vector<vector<float> > covar;
   
   
public:
   RBF (float (*_dist)(const float *, const float*, int) = euclidian)
      : KMeans(_dist)
   {}

   void train (int codeSize, const vector<float *> &data/*, const vector<float *> &data_out*/, int len, bool binary=false);
   
   void calcDist (const float *v, float *dist_return) const;

   void printOn(ostream &out=cout) const;
   void readFrom (istream &in=cin);
   friend istream &operator >> (istream &in, RBF &mdl);
};

#endif
