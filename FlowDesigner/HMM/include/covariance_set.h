// Copyright (C) 1998-1999  Jean-Marc Valin

#ifndef COVARIANCE_SET_H
#define COVARIANCE_SET_H

#include "covariance.h"
#include "Object.h"
#include "mean_set.h"

class CovarianceSet : public Object {
protected:
   int nb_covariances;
   Vector<RCPtr<Covariance> > covariances;
public:
   /**Default Constructor*/
   CovarianceSet() 
   : nb_covariances (0)
   {};

   /**Converts the covariances from accum mode to invert mode*/
   void toInvert (const MeanSet &means);

   /**Returns the number of elements*/
   int size() {return nb_covariances;}

   /**Indexing operator, returns the Ptr to the specified ID*/
   RCPtr<Covariance> operator [] (int id) {return covariances[id];}

   /**Returns the id corresponding to the pointer (or add if absent)*/
   int getIDFor(RCPtr<Covariance>);

   /**Returns the pointer corresponding to the id*/
   RCPtr<Covariance> getPtrFor(int id) const;

   /** print function used for operator << */
   virtual void printOn(std::ostream &out=std::cout) const;

   /**Read function used for operator >> */
   void readFrom (std::istream &in=std::cin);

   /**extractor operator*/
   friend std::istream &operator >> (std::istream &in, CovarianceSet &cov);

};

#endif
