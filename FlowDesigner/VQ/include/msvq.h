// Copyright (C) 1999 Jean-Marc Valin

#ifndef MSVQ_H
#define MSVQ_H

#include "kmeans.h"
#include "Object.h"
#include <vector>

namespace FD {

/**This is a multi-stage vector quantization class, and has nothing to do with microsoft :-)*/
class MSVQ : public VQ {
  protected:
   std::vector<int> stagesSizes;
   std::vector<KMeans> stages;
  public:
   MSVQ(float (*_dist)(const float *, const float*, int) = KMeans::euclidian)
      : VQ(_dist)
      {}
   MSVQ(const std::vector<int> &_stagesSizes, float (*_dist)(const float *, const float*, int) = KMeans::euclidian);

   int ID2Vec(const std::vector<int> &vec) const;

   std::vector<int> Vec2ID(int ID) const;

   int nbClasses() const;

   int nbStages() const {return stagesSizes.size();}

   //const std::vector<float> &operator[] (int i) const;

   void train (const std::vector<float *> &data, int len, bool binary=false);

   int getClassID (const float *v, float *dist_return = NULL) const;

   //void calcDist (const float *v, float *dist_return) const;

   void printOn(std::ostream &out=std::cout) const;
   void readFrom (std::istream &in=std::cin);
   friend std::istream &operator >> (std::istream &in, MSVQ &mdl);
};

}//namespace FD
#endif
