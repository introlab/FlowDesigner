#ifndef FFNET_H
#define FFNET_H

#include <vector>
#include <stream.h>

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
	 calc(input);


	 for (int k=outputLayer;k>=0;k--)
	 {
	    float *previous, *current;
	    if (k==0)
	       previous = input;
	    else 
	       previous = layers[k-1]->getValue();
	    current = layers[k]->getValue();
	    int layerSize = topo[k+1];
	    int layerInputs = topo[k];
	    float *err = layers[k]->getError();
	    for (int i=0;i<layerSize;i++)
	    {
	       float *w = layers[k]->getWeights(i);
	       if (k==outputLayer)
	       {
		  //cerr << "output layer\n";
		  err[i]=output[i]-current[i];
		  cout << "error = " << err[i] << endl;
	       }
	       else
	       {
		  err[i] = 0;
		  float *prevErr = layers[k+1]->getError();
		  cerr << "topo: " << topo[k+2] << endl;
		  for (int j=0;j<topo[k+2];j++)
		  {
		     float *prevW = layers[k+1]->getWeights(j);
		     err[i]+= prevErr[j]*prevW[i];
		  }
	       }
	       for (int j=0;j<layerInputs;j++)
	       {
		  //cout << w[j] << " -> ";
		  w[j] += alpha * previous[j] * err[i] ; // * g'(x)
	       }
	       w[layerInputs] += alpha * err[i];  // * g'(x)
	       //cout << w[i] << endl;
	    }
	 }

      }

};

#endif
