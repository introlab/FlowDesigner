#ifndef FFNET_H
#define FFNET_H

#include <vector>
#include <iostream>

#include "FFLayer.h"
#include "Object.h"
#include "Array.h"

//#define alpha .0000005

class FFNet : public Object {
  protected:
   Vector<int> topo;
   Vector<FFLayer *> layers;
  public:
   FFNet(const Vector<int> &_topo, const vector<string> &functions);
   FFNet(const Vector<int> &_topo);
   FFNet() {}
   double *calc(const double *input)
   {
      layers[0]->update(input);
      for (int i=1;i<layers.size();i++)
	 layers[i]->update(layers[i-1]->getValue());
      return layers[layers.size()-1]->getValue();
   }

   FFNet(FFNet &net);

   FFNet(const Vector<int> &_topo, const vector<string> &functions, vector<float *> &tin, vector<float *> &tout);
   double totalError(vector<float *> tin, vector<float *> tout);
   double calcError(const vector<float *> &tin, const vector<float *> &tout);
   void getGradient(double *ptr);
   void getWeights(double *ptr);
   void setWeights(double *ptr);
   //void calcGradient(vector<float *> &tin, vector<float *> &tout, double *weights, double *gradient, double &err);
   void calcGradient(vector<float *> &tin, vector<float *> &tout, Array<double> weights, Array<double> &gradient, double &err);
   void calcGradientRecur(vector<float *> &tin, vector<float *> &tout, Array<double> weights, Array<double> &gradient, double &err);
   void calcGradientBounds(vector<float *> &tin, vector<float *> &tout, vector<float *> &tbounds, Array<double> weights, Array<double> &gradient, double &err);

   void learn(double *input, double *output, double *err=NULL, double *calc_output=NULL);
   void learn_bounds(double *input, double *output, double *low_bound, double *err=NULL, double *calc_output=NULL);
   //void learnlm(double *input, double *output, double **jacob, double *err, double &sse);

   void train(vector<float *> in, vector<float *> out, int iter, double learnRate=.00001, double mom=.9, 
	      double increase=1.05, double decrease=.7, double errRatio=1.04, int nbSets=1);

   void traincg(vector<float *> in, vector<float *> out, int iter);

   void trainCGB(vector<float *> tin, vector<float *> tout, int iter, double sigma=.03, double lambda=.2);

   void trainSCGBounds(vector<float *> tin, vector<float *> tout, vector<float *> tbounds, int iter, double sigma=.03, double lambda=.2);

   void trainDeltaBar(vector<float *> tin, vector<float *> tout, int iter, double learnRate, 
		      double mom, double increase, double decrease, int nbSets);
   void trainRecurDeltaBar(vector<float *> tin, vector<float *> tout, int iter, double learnRate, 
		      double mom, double increase, double decrease, int nbSets);
   void trainRecurrent(vector<float *> tin, vector<float *> tout, int iter, double learnRate, 
		       double mom, double increase, double decrease);
   //void trainlm(vector<float *> in, vector<float *> out, int maxIter);
   
   void FFNet::trainSA(vector<float *> tin, vector<float *> tout, int iter, double Ti, 
		       double Tf, double increase, double decrease);

   void printOn(ostream &out) const;
   void readFrom (istream &in);
};

istream &operator >> (istream &in, FFNet &net);

#endif
