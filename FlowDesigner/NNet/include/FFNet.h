// Copyright (C) 2001 Jean-Marc Valin

#ifndef FFNET_H
#define FFNET_H

#include <vector>
#include <iostream>

#include "FFLayer.h"
#include "Object.h"
#include "Array.h"

class TrainingAlgo;


/**Feed-forward neural network (MLP) class
   @author: Jean-Marc Valin
 */
class FFNet : public Object {
  protected:

   /**Network topology*/
   Vector<int> topo;

   /**Pointers to all MLP layers*/
   Vector<RCPtr<FFLayer> > layers;

   /**Complete weight vector*/
   float *weights;
   
   /**Total number of neurons*/
   int nbNeurons;

   /**Total number of weights*/
   int nbWeights;

  public:
   FFNet(const Vector<int> &_topo, const Vector<string> &functions);
   //FFNet(const Vector<int> &_topo);
   FFNet() {}

   FFNet(FFNet &net);

   FFNet(const Vector<int> &_topo, const Vector<string> &functions, vector<float *> &tin, vector<float *> &tout);

   void init(const Vector<string> &functions);

   void setupLayersAfterRead();

   /**Calculates the network result for a certain input*/
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
  
   /**Calculates the gradient for a single sample*/
   void learn(float *input, float *output, double *gradient, double *err=NULL, float *calc_output=NULL);

   /**Calculates the gradient for a set of sample*/
   void calcGradient(vector<float *> &tin, vector<float *> &tout, Array<float> weights, 
		     Array<double> &gradient, double &err);

   /**Calculates the gradient for a single sample using variable weight*/
   void weightedLearn(float *input, float *output, float *learnWeights, double *gradient, 
		      double *err=NULL, float *calc_output=NULL);

   /**Calculates the gradient for a set of sample using variable weight*/
   void weightedCalcGradient(vector<float *> &tin, vector<float *> &tout, vector<float *> &learnWeights, 
			     Array<float> weights, Array<double> &gradient, double &err);

   /**Mean square error for a complete set*/ 
   float totalError(vector<float *> tin, vector<float *> tout);

   int getNbWeights () {return nbWeights;}

   int getNbNeurons () {return nbNeurons;}

   const Vector<int> &getTopo() {return topo;}

   const Vector<RCPtr<FFLayer> > &getLayers() {return layers;}

   const float *getWeights() {return weights;}

   void setWeights(float *ptr) {vec_copy(ptr, weights, nbWeights);}

   void setDerivOffset(float d);
   
   /**Writes an MLP to a stream*/
   void printOn(ostream &out) const;

   /**Reads an MLP from a stream*/
   void readFrom (istream &in);

};

istream &operator >> (istream &in, FFNet &net);

#endif
