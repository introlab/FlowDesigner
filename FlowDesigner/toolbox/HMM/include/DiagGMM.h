// Copyright (C) 2001 Jean-Marc Valin

#ifndef DIAG_GMM_H
#define DIAG_GMM_H
#include "Object.h"
#include <vector>

namespace FD {

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

   void train (const std::vector<float *> &frames, int nb_dim, int nb_gaussians, int nb_splits);

   /**Score a frame*/
   float score(const float *vec);

   /**Number of dimensions*/
   size_t getDim() {return dim;}

   /** print function used for operator << */
   virtual void printOn(std::ostream &out=std::cout) const;

   /**Read function used for operator >> */
   void readFrom (std::istream &in=std::cin);

   /**extractor for DiagGMM*/
   friend std::istream &operator >> (std::istream &in, DiagGMM &gmm);

   virtual void serialize(std::ostream &out) const;

   virtual void unserialize(std::istream &in);

   friend class GMM;
};

std::istream &operator >> (std::istream &in, DiagGMM &gmm);

}//namespace FD

#endif /* DIAG_GMM_H */
