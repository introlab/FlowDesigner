// Copyright (C) 1999 Jean-Marc Valin

#ifndef VQ_H
#define VQ_H

//#include "kmeans.h"
#include "Object.h"
#include <vector>

/**This is a vector quantization class*/
class VQ : public Object {
  protected:
   float (*dist)(const float *, const float*, int);
   int length;
  public:
   static float euclidian (const float *x, const float *y, int len)
   {
      float sum1=0,sum2=0,sum3=0,sum4=0;
      const float *end = x+len;
      while (x<end-3)
      {
	 sum1 += (*x-*y)*(*x-*y);
	 x++; y++;
	 sum2 += (*x-*y)*(*x-*y);
	 x++; y++;
	 sum3 += (*x-*y)*(*x-*y);
	 x++; y++;
	 sum4 += (*x-*y)*(*x-*y);
	 x++; y++;
      }
       while (x<end)
       {
	 sum1 += (*x-*y)*(*x-*y);
	 x++; y++;
       }
      return (sum1+sum2)+(sum3+sum4);
   }

  public:
   VQ(float (*_dist)(const float *, const float*, int) = euclidian)
      : dist(_dist)
      {}

   virtual ~VQ() {}

   virtual int nbClasses() const = 0;

   //virtual const vector<float> &operator[] (int i) const = 0;

   virtual int getClassID (const float *v, float *dist_return = NULL) const = 0;
   
   //virtual void calcDist (const float *v, float *dist_return) const = 0;

   virtual void printOn(ostream &out=cout) const = 0;
   virtual void readFrom (istream &in=cin) = 0;
};
#endif
