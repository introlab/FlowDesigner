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
   vector<vector<float> > means;
   //vector<int> accum;

public:
   explicit KMeans (float (*_dist)(const float *, const float*, int) = euclidian)
      : VQ(_dist)
   {}

   int nbClasses() const {return means.size();}

   const vector<float> &operator[] (int i) const;

   void split (const vector<float *> &data, int len);

   void bsplit ();

   void update (const vector<float *> &data, int len);

   void train (int codeSize, const vector<float *> &data, int len, bool binary=false);

   int getClassID (const float *v, float *dist_return = NULL) const;
   virtual void calcDist (const float *v, float *dist_return) const;

   void weightMeans (const Vector<float> &w, Vector<float> &out) const;
   
   void printOn(ostream &out=cout) const;
   void readFrom (istream &in=cin);
   friend istream &operator >> (istream &in, KMeans &mdl);
};

#endif
