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

#include "vec.h"
#include "Vector.h"

class DiagGaussian {
   void *ptr;
   float *meanCov;
   int size;
   int accum;
public:
   DiagGauss(int _size)
      : size(_size)
      , ptr(malloc (2*_size*sizeof(float)+CACHE_LINE))
      , accum(0)
   {
      meanCov = (float *) (((unsigned long)(ptr) + (CACHE_LINE-1))&CACHE_MASK);
   }

   ~DiagGauss() {delete [] ptr;}

   float dist(const float *vec) const
   {
      
   }

   float mdist(const vector<float *> &in, vector<float> &out)
   {
   }

};

class DiagGMM {
   vector<DiagGaussian *> gauss;
public:
   train(vector<float *> in, int length, int nbGauss);
   
};
