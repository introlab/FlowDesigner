#ifndef FFNET_H
#define FFNET_H

#include <vector>
#include <iostream>

#include "FFLayer.h"
#include "Object.h"
#include "Array.h"

/**Feed-forward neural network (MLP) class
   @author: Jean-Marc Valin
 */
class FFNet : public Object {
  protected:
   Vector<int> topo;
   Vector<FFLayer *> layers;
   float *weights;
   int nbNeurons, nbWeights;
  public:
   FFNet(const Vector<int> &_topo, const vector<string> &functions);
   //FFNet(const Vector<int> &_topo);
   FFNet() {}

   void init(const vector<string> &functions);

   void setupLayersAfterRead();

   float *calc(const float *input, float *value, float *deriv=NULL)
   {
      layers[0]->update(input, value, deriv);
      for (int i=1;i<layers.size();i++)
      {
	 if (deriv)
	 {
	    layers[i]->update(value+layers[i-1]->getNeuronOffset(), 
			      value+layers[i]->getNeuronOffset(), 
			      deriv+layers[i]->getNeuronOffset());
	 } else {
	    layers[i]->update(value+layers[i-1]->getNeuronOffset(), 
			      value+layers[i]->getNeuronOffset());
	 }
      }
      return value+layers[layers.size()-1]->getNeuronOffset();
   }

   FFNet(FFNet &net);

   FFNet(const Vector<int> &_topo, const vector<string> &functions, vector<float *> &tin, vector<float *> &tout);
   
   float totalError(vector<float *> tin, vector<float *> tout);

   float calcError(const vector<float *> &tin, const vector<float *> &tout);

   //void getGradient(float *ptr);

   //void getWeights(float *ptr);

   //void setWeights(float *ptr);
   
   int getNbWeights () {return nbWeights;}

   int getNbNeurons () {return nbNeurons;}

   void calcGradient(vector<float *> &tin, vector<float *> &tout, Array<float> weights, Array<double> &gradient, double &err);
  
   inline void learn(float *input, float *output, double *gradient, double *err=NULL, float *calc_output=NULL);
   
     //void trainCGB(vector<float *> tin, vector<float *> tout, int iter, float sigma=.03, float lambda=.2);

   void trainDeltaBar(vector<float *> tin, vector<float *> tout, int iter, float learnRate, 
		      float increase, float decrease);

   void trainQProp(vector<float *> tin, vector<float *> tout, int iter, float learnRate);
   
   //void FFNet::trainSA(vector<float *> tin, vector<float *> tout, int iter, float Ti, 
   //	       float Tf, float increase, float decrease);

   void printOn(ostream &out) const;

   void readFrom (istream &in);

   void setDerivOffset(float d);

};

istream &operator >> (istream &in, FFNet &net);

#endif
