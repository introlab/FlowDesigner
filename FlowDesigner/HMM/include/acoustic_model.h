// Copyright (C) 1999 Jean-Marc Valin
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


#ifndef ACCOUSTIC_MODEL_H
#define ACCOUSTIC_MODEL_H

#include "gmm.h"
#include "gaussian.h"
#include "covariance.h"
#include "Object.h"
#include <stream.h>
#include <iostream.h>
#include "covariance_set.h"
#include "mean_set.h"
#include "gaussian_set.h"
#include "gmm_set.h"

//#include "state.h"
class AcousticModel : public Object{
protected:
   CovarianceSet covariances;
   MeanSet means;
   GaussianSet gaussians;
   GMMSet gmms;

public:
   AcousticModel() {}

   /**convert all elements to using IDs (before saving)*/
   void toIDs();
 
   /**convert all elements to using Ptrs (after loading)*/
   void toPtrs();

   /** print function used for operator << */
   virtual void printOn(ostream &out=cout) const;

   /**Read function used for operator >> */
   void readFrom (istream &in=cin);

   /**extractor operator*/
   friend istream &operator >> (istream &in, AcousticModel &mdl);
   
};

#endif
