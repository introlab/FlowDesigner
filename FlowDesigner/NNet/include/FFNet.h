#ifndef FFNET_H
#define FFNET_H

#include <vector>

#include "FFLayer.h"

#define alpha .01

class FFNet {
  protected:
   vector<int> topo;
   vector<FFLayer *> layers;
  public:
   FFNet(const vector<int> &_topo);

   float *calc(float *input)
      {
	 layers[0]->update(input);
	 for (int i=1;i<layers.size();i++)
	    layers[i]->update(layers[i-1]->getValue());
	 return layers[layers.size()-1]->getValue();
      }

   void learn(float *input, float *output)
      {
	 int outputLayer = topo.size()-2;
	 float *current = calc(input);
	 int outSize = topo[topo.size()-1];
	 int hSize = topo[topo.size()-2];
	 //float err[outSize];
	 
	 float *previous;
	 if (topo.size() >= 3)
	    previous = layers[topo.size()-3]->getValue();
	 else 
	    previous = input;
	 for (int i=0;i<outSize;i++)
	 {
	    float *w=layers[topo.size()-2]->getWeights(i);
	    float err = output[i]-current[i];
	    for (int j=0;j<hSize;j++)
	    {
	       w[i] += alpha * previous[j] * err ; // * g'(x)
	    }
	    w[i] += alpha * err; // * g'(x)
	 }

	 for (int k=outputLayer-1;k>=0;k++)
	 {
	    if (k==0)
	       previous = input;
	    else 
	       previous = layers[k-1]->getValue();
	    current = layers[k]->getValue();
	    int layerSize = topo[k+1];
	    int layerInputs = topo[k];
	    for (int i=0;i<layerSize;i++)
	    {
	       float *w = layers[k]->getWeights(i);
	    }
	 }

      }

};

#endif
