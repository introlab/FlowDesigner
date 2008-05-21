// Copyright (C) 1998-1999  Jean-Marc Valin

#ifndef GAUSSIAN_SET_H
#define GAUSSIAN_SET_H

#include "gaussian.h"
#include "mean_set.h"
#include "covariance_set.h"
#include "Object.h"

namespace FD {

class GaussianSet : public Object {
protected:
   int nb_gaussians;
   Vector<RCPtr<Gaussian> > gaussians;
public:
   /**Default Constructor*/
   GaussianSet() 
      : nb_gaussians(0)
   {};

   /**Returns the id corresponding to the pointer (or add if absent)*/
   int getIDFor(RCPtr<Gaussian>);

   /**Returns the pointer corresponding to the id*/
   RCPtr<Gaussian> getPtrFor(int id) const;

   /**convert all elements to using IDs (before saving)*/
   void toIDs(MeanSet & means, CovarianceSet & covariances);
 
   /**convert all elements to using Ptrs (after loading)*/
   void toPtrs(const MeanSet & means, const CovarianceSet & covariances) const;

   /** print function used for operator << */
   virtual void printOn(std::ostream &out=std::cout) const;

   /**Read function used for operator >> */
   void readFrom (std::istream &in=std::cin);

   /**extractor operator*/
   friend std::istream &operator >> (std::istream &in, GaussianSet &cov);

};

}//namespace FD

#endif
