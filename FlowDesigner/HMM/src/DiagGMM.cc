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

#include "DiagGMM.h"
#include "misc.h"
#include "vec.h"


float DiagGMM::score(const float *vec)
{
   float score;
   char mem[augDim*sizeof(float)+CACHE_LINES];
   float *data = (float *) (((unsigned long)(mem) + (CACHE_LINES-1))&CACHE_MASK);
   for (int i=0;i<dim;i++)
      data[i]=vec[i];
   data[dim]=1;
   for (int i=dim+1;i<augDim;i++)
      data[i]=0;

   int inc=2*augDim;
   float *mean=base;
   float *cov=base+augDim;
   float maxScore=0;
   for (int k=0;k<nbGauss;k++)
   {
      score = 0;
      for (int i=0;i<augDim;i++)
	 score += sqr(data[i]-mean[i])*cov[i];
      if (k==0 || score > maxScore)
	 maxScore = score;
      mean+=inc;
      cov += inc;
   }
   return maxScore;
}
