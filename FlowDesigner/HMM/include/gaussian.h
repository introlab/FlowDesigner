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
#ifndef GAUSSIAN_H
#define GAUSSIAN_H

#include <stream.h>
#include <string>
#include "covariance.h"
#include "ObjectParser.h"
#include "misc.h"

/**Gaussian class*/
class Gaussian : public Object
{
protected:
   /**The mean of the gaussian stored as an STL vector of float*/
   vector<float> *mean;

   /**The covariance of the gaussian is a pointer to abstract class Covariance*/
   Covariance    *covariance;

   /**number of frames aligned (accumulated) to the covariance*/
   int            accum_count;

   /**Dimension (same as the mean and covariance dimension)*/
   int            dimension;

public:
   /**Empty gaussian constructor*/
   Gaussian() : dimension(0) {}

   Gaussian(istream &in) {in >> *this;}

   /**Construct a Gaussian with dimension dim and a covariance pseudo-factory
    *(allows to create gaussians with either diagonal or full covariance*/
   Gaussian(int dim, Covariance *(*cov_new)(int)) 
      : mean(new vector<float> (dim,0.0))
      , covariance(cov_new (dim))
      , accum_count(0)
      , dimension(dim)
   {}

   /**Copy constructor*/
   Gaussian(const Gaussian &g) 
      : mean(new vector<float> (*g.mean))
      , covariance(g.covariance->copy())
      , accum_count (g.accum_count)
      , dimension (g.dimension)
   {}

   /**Destructor*/
   ~Gaussian();
      
   /**Returns the dimension of the gaussian*/
   int getDimension() const  { return dimension; }

   /**Returns the mean of the gaussian*/
   vector<float> &getMean() const  { return *mean; }

   /**Returns the covariance of the gaussian*/
   Covariance &getCovariance() const { return *covariance; }

   /**Convert from accumulate to real mode*/
   void to_real();

   /**Returns the number of frames aligned to the gaussian*/
   int get_accum_count() const {return accum_count;} 

   /**Returns the mahalanobis distance between the gaussian and a frame*/
   float mahalanobis(const float * fr) const
   {
      /*float dist=0;
      for (unsigned int i=0;i<dimension;i++)
      {
         dist+=sqr(fr[i]-(*mean)[i]);// /(*covariance)[i];
      }
      //cerr << "det: " << covariance->getDeterminant() << endl;
      return dist;// + covariance->getDeterminant();
      */
      return covariance->mahalanobisDistance(fr,mean->begin());
   }

   /**Returns the euclidian distance between the gaussian and a frame*/
   float euclidian(const float * fr) const
   {
      float dist=0;
      for (unsigned int i=0;i<dimension;i++)
      {
         dist+=sqr(fr[i]-(*mean)[i]);
      }
      //cerr << "det: " << covariance->getDeterminant() << endl;
      return dist;
   }

   /**Adds (accumulates) a frame to the gaussian*/
   void accum_frame(const float * fr)
   {
      accum_count++;
      for (unsigned int i=0;i<dimension;i++)
      {
         (*mean)[i]+=fr[i];
         (*covariance)[i]+=sqr(fr[i]);
      }
   }

   /**Set everything to zero and come back to accumulate mode*/
   void reset_to_accum_mode()
   {
      accum_count=0;
      //mean->reset();
      covariance->reset();
      for (unsigned int i=0;i<mean->size();i++)
         (*mean)[i]=0.0;
   }

   /**print function for operator <<*/
   void printOn(ostream &out = cout) const;

   friend istream &operator >> (istream &in, Gaussian &gauss);
};

istream &operator >> (istream &in, Gaussian &gauss);
#endif
