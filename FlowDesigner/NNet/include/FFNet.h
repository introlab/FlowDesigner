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
   float alpha;
   float momentum;
  public:
   FFNet(const Vector<int> &_topo);
   FFNet() {}
   float *calc(const float *input);

   void learn(float *input, float *output);

   void train(vector<float *> in, vector<float *> out, int iter);

   void printOn(ostream &out) const;
   void readFrom (istream &in);
};

istream &operator >> (istream &in, FFNet &net);

#endif
