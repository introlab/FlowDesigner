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
   double error;
   double last_error;
   double alpha;
   double momentum;
  public:
   FFNet(const Vector<int> &_topo);
   FFNet() {}
   double *calc(const double *input);

   void learn(double *input, double *output);

   void train(vector<float *> in, vector<float *> out, int iter);

   void printOn(ostream &out) const;
   void readFrom (istream &in);
};

istream &operator >> (istream &in, FFNet &net);

#endif
