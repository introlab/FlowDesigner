// Copyright (C) 2001 Jean-Marc Valin

#ifndef DIAG_GMM_H
#define DIAG_GMM_H
#include "Object.h"

class GMM;

/**Gaussian Mixture Model (GMM) implementation using diagonal covariances and
optimized for speed*/
class DiagGMM : public Object {
   /**Allocated poinger for all data (only used in construction/destruction)*/
   char *ptr;

   /**Base pointer for data (aligned on cache line boundary)*/
   float *base;

   /**Number of dimensions*/
   int dim;

   /**Augmented number of dimensions (so we have an integer number of cache lines)*/
   int augDim;

   /**Number of gaussians*/
   int nbGauss;

public:
   //DiagGMM(const GMM &gmm);
   DiagGMM() {ptr=NULL;}

   /**Destructor*/
   ~DiagGMM() {if (ptr) delete [] ptr;}

   /**Score a frame*/
   float score(const float *vec);

   /**Number of dimensions*/
   int getDim() {return dim;}

   friend class GMM;
};


#endif /* DIAG_GMM_H */
