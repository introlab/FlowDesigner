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


class ifstream;
class ofstream;

///Abstract covariance class
class Covariance 
{
protected:
   ///Size of the covariance matrix
   unsigned int   dimension;
   ///Log of the determinant
   mutable float determinant;
   //Whether or not the determinant has been computed
   mutable bool  determinant_is_valid;
public:
   ///Create a Covariance with dim dimensions
   Covariance(int dim) : dimension(dim) , determinant(-10000) , determinant_is_valid(false)
   {
      
   }
   ///Copy constructor
   Covariance(const Covariance &cov) 
      : dimension(cov.dimension) 
      , determinant(0)
      , determinant_is_valid(false)
   {}
   ///Virtual Destructor
   virtual ~Covariance() {}
   
   
   ///Returns the covariance size (dimension)
   unsigned int   size() const { return dimension; }
   ///Returns (and compute if necessary) the covariance log determinant
   float getDeterminant() const 
   { 
      if (!determinant_is_valid) compute_determinant(); 
      return determinant; 
   }
   ///Computes the determinant
   virtual void compute_determinant() const =0;
   ///Prints the covariance
   virtual void print(ostream &out) const = 0;
   
   ///Virtual indexing operator 1D (for diagonal covariance)
   virtual float&      operator[](int )=0;
   ///Virtual indexing operator 2D
   virtual float&      operator()(int,int)=0;
   ///Resets accumulation to zero
   virtual void reset()=0;
   ///Returns a copy of the covariance
   virtual Covariance * copy()=0;
   ///Converts from accumulate mode to real
   virtual void to_real(const float accum_1, const vector<float> *mean)=0;

   friend ostream &operator << (ostream &out, const Covariance &covar);
};

ostream &operator << (ostream &out, const Covariance &covar);

///Diagonal Covariance class
class DiagonalCovariance : public Covariance  {
   ///The covariance data as the diagonal vector
   vector<float> data;
public:
   DiagonalCovariance(istream &in)
      : Covariance(0)
   {in >> *this;}
   
   DiagonalCovariance() 
      : Covariance(0)
      , data(vector<float>(0,0.0))
   {}

   ///Constructs a Diagonal Covariance with dimension dim
   DiagonalCovariance(int dim) 
      : Covariance(dim) 
      , data(vector<float>(dim,0.0)) 
   {}

   ///Copy Constructor
   DiagonalCovariance (const DiagonalCovariance &cov) 
      : Covariance(cov)
      , data(cov.data)
   {}

   /**@name Indexing 
   *Warning: These virtual indexing function are dangerous for performance
    *and should not be used in loops*/
   //@{
   ///1D indexing
   float&      operator[](int i) {return data[i];}

   ///1D indexing
   float&      operator()(int i) {return data[i];}

   ///2D indexing (second operator is ignored)
   float&      operator()(int i,int) {return data[i];}
   //@}

   ///Computes the determinant
   void compute_determinant() const;

   ///Reset accumulation to zero
   void reset();

   ///Returns a copy of the covariance
   DiagonalCovariance *copy () {  return new DiagonalCovariance (*this); }

   ///Converts from accumulate mode to real
   void to_real(const float accum_1, const vector<float> *mean);

   virtual void print(ostream &out) const;

   friend istream &operator >> (istream &in, DiagonalCovariance &cov);
}
;

///Function that acts as a factory for diagonal covariance
inline Covariance *NewDiagonalCovariance(int dim) {return new DiagonalCovariance (dim);}

istream &operator >> (istream &in, DiagonalCovariance &cov);

#endif
