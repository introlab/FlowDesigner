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
#ifndef GAUSSIAN_H
#define GAUSSIAN_H

#include <iostream>
#include <string>
#include "covariance.h"
#include "ObjectParser.h"
#include "misc.h"
#include "Vector.h"
#include "covariance_set.h"
#include "mean.h"
#include "mean_set.h"

class GMM;

/**Gaussian class*/
class Gaussian : public Object
{
protected:
   /**The mean of the gaussian stored as an STL vector of float*/
   Ptr<Mean> mean;
   //Ptr<Vector<float> > mean;

   /**The covariance of the gaussian is a pointer to abstract class Covariance*/
   Ptr<Covariance> covariance;

   /**number of frames aligned (accumulated) to the covariance*/
   int            accum_count;

   /**Dimension (same as the mean and covariance dimension)*/
   int            dimension;

   /**Was the gaussian loaded using indexes for mean*/
   bool using_meanID;
   
   /**Was the gaussian loaded using indexes for covariance*/
   bool using_covarianceID;
   
   /**The ID of the gaussian's mean*/
   int meanID;

   /**The ID of the gaussian's covariance*/
   int covarianceID;

public:
   /**Empty gaussian constructor*/
   Gaussian() 
      : dimension(0) 
      , using_meanID(false)
      , using_covarianceID(false)
   {}

   Gaussian(istream &in) {in >> *this;}

   /**Construct a Gaussian with dimension dim and a covariance pseudo-factory
    *(allows to create gaussians with either diagonal or full covariance*/
   Gaussian(int dim, Covariance *(*cov_new)(int))
      : mean(new Mean (dim,0.0))
      //: mean(new Vector<float> (dim,0.0))
      , covariance(cov_new (dim))
      , accum_count(0)
      , dimension(dim)
      , using_meanID(false)
      , using_covarianceID(false)
   {}

   Gaussian(int dim, int _meanID, int _covarianceID)
      : accum_count(0)
      , dimension(dim)
      , using_meanID(true)
      , using_covarianceID(true)
      , meanID(_meanID)
      , covarianceID(_covarianceID)
   {}

   /**Copy constructor*/
   Gaussian(const Gaussian &g) 
      : mean(new Mean (*g.mean))
      //: mean(new Vector<float> (*g.mean))
      , covariance(g.covariance->copy())
      , accum_count (g.accum_count)
      , dimension (g.dimension)
      , using_meanID(false)
      , using_covarianceID(false)
   {}

   /**Destructor*/
   ~Gaussian();
      
   /**Returns the dimension of the gaussian*/
   int getDimension() const  { return dimension; }

   /**Returns the mean of the gaussian*/
   Mean &getMean() const  { return *mean; }

   /**Returns the covariance of the gaussian*/
   Covariance &getCovariance() const { return *covariance; }

   /**Convert from accumulate to real mode*/
   void to_real();

   /**Returns the number of frames aligned to the gaussian*/
   int get_accum_count() const {return accum_count;} 

   /**Returns the mahalanobis distance between the gaussian and a frame*/
   float mahalanobis(const float * fr) const
   {
      return covariance->mahalanobisDistance(fr,mean->begin());
   }

   /**Returns the mahalanobis distance between the gaussian and a frame*/
   float mahalanobis(const float * fr, Covariance *cov) const
   {
      return cov->mahalanobisDistance(fr,mean->begin());
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
      mean->accumFrame(fr);
      covariance->accumFrame(fr);
      accum_count++;
   }

   /**Set everything to zero and come back to accumulate mode*/
   void reset_to_accum_mode()
   {
      accum_count=0;
      covariance->reset();
      mean->resetToAccum();
   }

   void toIDsUsing (MeanSet &means, CovarianceSet & covariances);

   void toPtrsUsing (const MeanSet &means, const CovarianceSet & covariances);

   /**print function for operator <<*/
   void printOn(ostream &out = cout) const;

   /**Read function used for operator >> */
   void readFrom (istream &in=cin);

   friend istream &operator >> (istream &in, Gaussian &gauss);
   friend class GMM;
};

istream &operator >> (istream &in, Gaussian &gauss);
#endif
