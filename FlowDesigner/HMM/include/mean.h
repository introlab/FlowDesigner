// Copyright (C) 1998-1999  Jean-Marc Valin
#ifndef MEAN_H
#define MEAN_H

#include <math.h>
#include <vector>
#include "Object.h"
#include "ObjectParser.h"
#include "misc.h"
#include "Vector.h"

class Mean : public Vector<double>
{
protected:
   int mode;
   int accum_count;
   int dimension;
public:
   enum Mode {accum=0, real, rotated, inverted};
   Mean() 
      : Vector<double>() 
      , mode(accum)
      , accum_count(0)
      , dimension(0)
   {}

   Mean(int n, const double &x = 0) 
      : Vector<double>(n, x)
      , mode(accum)
      , accum_count(0)
      , dimension(n)
   {}

   int getAccum() {return accum_count;}

   void accumFrame(const float *v)
   {
      for (int i=0;i<dimension;i++)
         this->operator[] (i) += v[i];
      accum_count++;
   }

   void accumFrame(const vector<float> &v)
   {
      for (int i=0;i<v.size();i++)
         this->operator[] (i) += v[i];
      accum_count++;
   }

   void toReal()
   {
      double accum_1 = 1.0/accum_count;
      for (int i=0;i<dimension;i++)
         this->operator[] (i) *= accum_1;
      mode = real;
   }

   void resetToAccum()
   {
      for (int i=0;i<dimension;i++)
         this->operator[] (i) = 0.0;
      mode = accum;
      accum_count = 0;
   }

   /** print function used for operator << */
   virtual void printOn(ostream &out=cout) const;

   /**Read function used for operator >> */
   void readFrom (istream &in=cin);

   /**extractor operator*/
   friend istream &operator >> (istream &in, Mean &cov);

};

#endif
