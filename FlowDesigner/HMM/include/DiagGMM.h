// Copyright (C) 2001 Jean-Marc Valin

#ifndef DIAG_GMM_H
#define DIAG_GMM_H
#include "Object.h"

class GMM;
class DiagGMM : public Object {
   char *ptr;
   float *base;
   int dim;
   int augDim;
   int nbGauss;
public:
   //DiagGMM(const GMM &gmm);
   DiagGMM() {ptr=NULL;}

   ~DiagGMM() {if (ptr) delete [] ptr;}

   float score(const float *vec);

   int getDim() {return dim;}

   friend class GMM;
};


#endif /* DIAG_GMM_H */
