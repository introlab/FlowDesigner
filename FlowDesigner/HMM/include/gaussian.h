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

#include "hmm.h"
#include <stream.h>
#include <string>
#include "covariance.h"

class Gaussian 
{
private:
   vector<float> *mean;
   Covariance    *covariance;
   int            accum_count;
   int            dimension;
public:
   Gaussian(int dim, Covariance *(*cov_new)(int)) 
      : mean(new vector<float> (dim))
      , covariance(cov_new (dim)) , dimension(dim)
   {}
   Gaussian(const Gaussian &g) 
      : mean(new vector<float> (*g.mean))
      , covariance(g.covariance->copy())
      , accum_count (g.accum_count)
      , dimension (g.dimension)
   {}
   ~Gaussian();
      
   int         getDimension() const  { return dimension; }
   vector<float>      &getMean()      const  { return *mean; }
   Covariance &getCovariance() const { return *covariance; }
   void to_real();
   int get_accum_count() const {return accum_count;} 

   float mahalanobis(const Frame *fr) const
   {
      float dist=0;
      for (unsigned int i=0;i<fr->size();i++)
      {
         dist+=sqr((*fr)[i]-(*mean)[i]);///(*covariance)[i];
      }
      return dist;//*covariance->getDeterminant();
   }

   void accum_frame(const Frame &fr)
   {
      accum_count++;
      for (unsigned int i=0;i<fr.size();i++)
      {
         (*mean)[i]+=fr[i];
         (*covariance)[i]+=sqr(fr[i]);
      }
   }

   void reset_to_accum_mode()
   {
      accum_count=0;
      //mean->reset();
      covariance->reset();
      for (unsigned int i=0;i<mean->size();i++)
         (*mean)[i]=0.0;
   }
   void print_mean(ostream &out = cout, string separ = " ") const;
   void print_covar(ostream &out = cout, string separ = " ") const;
   
   //void print() const;
};
#endif
