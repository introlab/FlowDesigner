// Copyright (C) 1998-1999  Jean-Marc Valin
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
#ifndef MEAN_H
#define MEAN_H

#include <math.h>
#include <vector>
#include "Object.h"
#include "ObjectParser.h"
#include "misc.h"
#include "Vector.h"

class Mean : public Vector<float>
{
protected:
   int mode;
   int accum_count;
   int dimension;
public:
   enum Mode {accum=0, real, rotated, inverted};
   Mean() 
      : Vector<float>() 
      , mode(accum)
      , accum_count(0)
      , dimension(0)
   {}

   Mean(int n, const float &x = 0) 
      : Vector<float>(n, x)
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
      float accum_1 = 1.0/accum_count;
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
