// Copyright (C) 2001 Jean-Marc Valin

#ifndef DIAG_GMM_H
#define DIAG_GMM_H
#include "Object.h"
#include <vector>

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

   void train (const vector<float *> &frames, int nb_dim, int nb_gaussians, int nb_splits);

   /**Score a frame*/
   float score(const float *vec);

   /**Number of dimensions*/
   int getDim() {return dim;}

   /** print function used for operator << */
   virtual void printOn(ostream &out=cout) const;

   /**Read function used for operator >> */
   void readFrom (istream &in=cin);

   /**extractor for DiagGMM*/
   friend istream &operator >> (istream &in, DiagGMM &gmm);

   virtual void serialize(ostream &out) const;

   virtual void unserialize(istream &in);

   friend class GMM;
};

//ostream &operator << (ostream &out, const GMM &gmm);
istream &operator >> (istream &in, DiagGMM &gmm);


#endif /* DIAG_GMM_H */
