#ifndef FFNET_H
#define FFNET_H

#include <vector>
#include <stream.h>

#include "FFLayer.h"
#include "Object.h"
#include "Vector.h"

#define alpha .001

class FFNet : public Object {
  protected:
   Vector<int> topo;
   Vector<FFLayer *> layers;
  public:
   FFNet(const Vector<int> &_topo);
   FFNet() {}
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

	 //start with the output layer, towards the input
	 for (int k=outputLayer;k>=0;k--)
	 {
	    FFLayer *currentLayer = layers[k];
	    float *previousValue, *currentValue;
	    if (k==0)
	       previousValue = input;
	    else 
	       previousValue = layers[k-1]->getValue();

	    currentValue = currentLayer->getValue();

	    int layerSize = topo[k+1];
	    int layerInputs = topo[k];
	    float *delta = currentLayer->getError();
	    for (int i=0;i<layerSize;i++)
	    {
	       float *w = currentLayer->getTmpWeights(i);
	       //cerr << k << endl;
	       if (k==outputLayer)
	       {
		  //cerr << "output layer\n";
		  delta[i]=output[i]-currentValue[i];
		  cout << "error = " << delta[i] << endl;
		  delta[i] = currentLayer->deriv[i]*delta[i];
	       }
	       else
	       {
		  delta[i] = 0;
		  float *outErr = layers[k+1]->getError();
		  for (int j=0;j<topo[k+2];j++)
		  {
		     float *outW = layers[k+1]->getWeights(j);
		     delta[i]+= outErr[j]*outW[i];
		  }
		  delta[i] = currentLayer->deriv[i]*delta[i];
	       }
	       for (int j=0;j<layerInputs;j++)
	       {
		  w[j] += alpha * previousValue[j] * delta[i];
	       }
	       w[layerInputs] += alpha * delta[i];
	    }
	 }	 
      }

   void train(vector<float *> in, vector<float *> out, int iter)
      {
	 while (iter)
	 {
	    int i;
	    //cerr << "iter...\n";
	    for (i=0;i<layers.size();i++)
	    {
	       layers[i]->copyToTmp();
	    }
	    for (i=0;i<in.size();i++)
	       learn (in[i], out[i]);
	    for (i=0;i<layers.size();i++)
	    {
	       layers[i]->copyFromTmp();
	    }
 	    iter--;
	    
	 }
      }

   void printOn(ostream &out) const;
   void readFrom (istream &in);
};

istream &operator >> (istream &in, FFNet &net);

#endif
