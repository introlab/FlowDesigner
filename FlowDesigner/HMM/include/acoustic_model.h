// Copyright (C) 1999 Jean-Marc Valin


#ifndef ACCOUSTIC_MODEL_H
#define ACCOUSTIC_MODEL_H

#include "gmm.h"
#include "gaussian.h"
#include "covariance.h"
#include "Object.h"
#include <iostream>
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
   virtual void printOn(std::ostream &out=std::cout) const;

   /**Read function used for operator >> */
   void readFrom (std::istream &in=std::cin);

   /**extractor operator*/
   friend std::istream &operator >> (std::istream &in, AcousticModel &mdl);
   
};

#endif
