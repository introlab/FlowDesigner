// Copyright (C) 1999 Jean-Marc Valin

#ifndef RBF_H
#define RBF_H

#include <vector>
#include <iostream>
#include "Object.h"
#include "kmeans.h"

namespace FD {

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
   std::vector<std::vector<float> > covar;
   
   
public:
   explicit RBF (float (*_dist)(const float *, const float*, int) = euclidian)
      : KMeans(_dist)
   {}

   void train (int codeSize, const std::vector<float *> &data/*, const std::vector<float *> &data_out*/, int len, bool binary=false);
   
   void calcDist (const float *v, float *dist_return) const;

   void printOn(std::ostream &out=std::cout) const;
   void readFrom (std::istream &in=std::cin);
   friend std::istream &operator >> (std::istream &in, RBF &mdl);
};
}//namespace FD
#endif
