// Copyright (C) 1999 Jean-Marc Valin

#ifndef KMEANS_H
#define KMEANS_H

#include "Vector.h"
#include <iostream>
#include "Object.h"
#include "vq.h"

class KMeans : public VQ {
protected:
   //int length;
   std::vector<std::vector<float> > means;
   //std::vector<int> accum;

public:
   explicit KMeans (float (*_dist)(const float *, const float*, int) = euclidian)
      : VQ(_dist)
   {}

   int nbClasses() const {return means.size();}

   const std::vector<float> &operator[] (int i) const;

   void split (const std::vector<float *> &data, int len);

   void bsplit ();

   void update (const std::vector<float *> &data, int len);

   void train (int codeSize, const std::vector<float *> &data, int len, bool binary=false);

   int getClassID (const float *v, float *dist_return = NULL) const;
   virtual void calcDist (const float *v, float *dist_return) const;

   void weightMeans (const Vector<float> &w, Vector<float> &out) const;
   
   void printOn(std::ostream &out=std::cout) const;
   void readFrom (std::istream &in=std::cin);
   friend std::istream &operator >> (std::istream &in, KMeans &mdl);
};

#endif
