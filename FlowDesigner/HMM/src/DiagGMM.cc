// Copyright (C) 2001 Jean-Marc Valin

#include "DiagGMM.h"
#include "misc.h"
#include "vec.h"


float DiagGMM::score(const float *vec)
{
   float score;
   char mem[augDim*sizeof(float)+CACHE_LINES];
   //cerr << augDim << endl;
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
      //for (int i=0;i<augDim;i++)
      // score += sqr(data[i]-mean[i])*cov[i];
      score = vec_mahalanobis2(data, mean, cov, augDim);
      //cerr << score << endl;
      if (k==0 || score > maxScore)
	 maxScore = score;
      mean+=inc;
      cov += inc;
   }
   //cerr << "tata\n";
   return maxScore;
}
