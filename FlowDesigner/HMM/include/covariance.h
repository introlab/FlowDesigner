// Copyright (C) 1998-1999  Jean-Marc Valin & Daniel Kiecza
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
#ifndef COVARIANCE_H
#define COVARIANCE_H

#include <math.h>
#include <vector>
#include "hmm.h"

//class Frame;
class ifstream;
class ofstream;

enum covariance_type {diagonal, radial, full};

class Covariance 
{
protected:
   unsigned int   dimension;     // *** dimension of the covariance matrix
   mutable float determinant;   // *** log of the determinant
   mutable bool  determinant_is_valid;
public:
   Covariance(int dim) : dimension(dim) , determinant(-10000) , determinant_is_valid(false)
   {
      
   }
   Covariance(const Covariance &cov) 
      : dimension(cov.dimension) 
      , determinant(0)
      , determinant_is_valid(false)
   {}
   virtual ~Covariance() {}
   
   
   unsigned int   size() const { return dimension; }
   float getDeterminant() const 
   { 
      if (!determinant_is_valid) compute_determinant(); 
      return determinant; 
   }
   virtual void compute_determinant() const =0;
   void print() const;
   
   virtual float&      operator[](int )=0;
   virtual float&      operator()(int,int)=0;
   virtual void reset()=0;
   virtual Covariance * copy()=0;
   virtual void to_real(const float accum_1, const vector<float> *mean)=0;
   //virtual Covariance& operator=(const Covariance&)=0;
};



class DiagonalCovariance : public Covariance  {
   vector<float> data;
public:
   DiagonalCovariance(int dim) 
      : Covariance(dim) 
      , data(vector<float>(dim,0.0)) 
   {}

   DiagonalCovariance (const DiagonalCovariance &cov) 
      : Covariance(cov)
      , data(cov.data)
   {}

   //Dangerous for performance: these are virtual functions and should not
   //be used in loops
   float&      operator[](int i) {return data[i];}
   float&      operator()(int i) {return data[i];}
   float&      operator()(int i,int) {return data[i];}
   void compute_determinant() const;
   void reset();
   DiagonalCovariance *copy () {  return new DiagonalCovariance (*this); }
   void to_real(const float accum_1, const vector<float> *mean);
};

inline Covariance *NewDiagonalCovariance(int dim) {return new DiagonalCovariance (dim);}



#endif
