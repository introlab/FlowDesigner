// Copyright (C) 2001 Jean-Marc Valin

#include "DiagGMM.h"
#include "misc.h"
#include "vec.h"

//template <class T>
#define T float
inline T vec_mahalanobis3(const T *a, const T *b, const T *c, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  const T *end = a+len;
  /*while (a<end-3)
    {
       //cerr << "tata\n";
      sum1+=c[0]*(a[0]-b[0])*(a[0]-b[0]);
      sum2+=c[1]*(a[1]-b[1])*(a[1]-b[1]);
      sum3+=c[2]*(a[2]-b[2])*(a[2]-b[2]);
      sum4+=c[3]*(a[3]-b[3])*(a[3]-b[3]);
      a+=4;
      b+=4;
      c+=4;
      }*/
  //while (a<end-3)
    {
       /*sum1+=c[0]*(a[0]-b[0])*(a[0]-b[0]);
      sum2+=c[1]*(a[1]-b[1])*(a[1]-b[1]);
      sum3+=c[2]*(a[2]-b[2])*(a[2]-b[2]);
      sum4+=c[3]*(a[3]-b[3])*(a[3]-b[3]);
      a+=4; b+=4; c+=4;
       */
      /*sum1+=c[0]*(a[0]-b[0])*(a[0]-b[0]);
      sum2+=c[1]*(a[1]-b[1])*(a[1]-b[1]);
      sum3+=c[2]*(a[2]-b[2])*(a[2]-b[2]);
      a+=3; b+=3; c+=3;*/

      /*sum1+=c[0]*(a[0]-b[0])*(a[0]-b[0]);
      sum2+=c[1]*(a[1]-b[1])*(a[1]-b[1]);
      a+=2; b+=2; c+=2;*/

      /*a++; b++; c++;
      sum2+=c[0]*(a[0]-b[0])*(a[0]-b[0]);
      a++; b++; c++;*/
    }
  while (a<end)
    {
      sum1+=c[0]*(a[0]-b[0])*(a[0]-b[0]);
      a++; b++; c++;
    }
  //cerr << "*********************\n";
  return (sum1+sum2)+(sum3+sum4);
}

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
      //score = vec_mahalanobis3(data, mean, cov, augDim);
      //cerr << score << endl;
      if (k==0 || score > maxScore)
	 maxScore = score;
      mean+=inc;
      cov += inc;
   }
   //cerr << "tata\n";
   return maxScore;
}
