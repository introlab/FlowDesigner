#ifndef FFNET_H
#define FFNET_H

#include <vector>
#include <stream.h>

#include "FFLayer.h"
#include "Object.h"
#include "Vector.h"

//#define alpha .0000005

class FFNet : public Object {
  protected:
   Vector<int> topo;
   Vector<FFLayer *> layers;
  public:
   FFNet(const Vector<int> &_topo);
   FFNet() {}
   double *calc(const double *input);

   void learn(double *input, double *output, double alpha);

   void train(vector<float *> in, vector<float *> out, int iter, double learnRate=.00001, double mom=.9, 
	      double increase=1.05, double decrease=.7, double errRatio=1.04);

   void printOn(ostream &out) const;
   void readFrom (istream &in);
};

istream &operator >> (istream &in, FFNet &net);

#endif
