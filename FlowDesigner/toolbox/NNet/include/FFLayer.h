// Copyright (C) 2001 Jean-Marc Valin
#ifndef FFLAYER_H
#define FFLAYER_H

#include "Object.h"
#include <math.h>
#include <iostream>
#include <stdlib.h>
#include "functions.h"

namespace FD {

/**Represents one (fully-connected) layer from a multi-layer percetron (FFNet)
   @author: Jean-Marc Valin
*/
class FFLayer : public Object {
  public:
   /**Activation function pointer*/
   void (*func) (float *, float *, int);

   /**Activation function derivative pointer*/
   void (*deriv_func) (float *, float *, int);

  protected:
   /**Number of neurons in the layer*/
   int nbNeurons;

   /**Number of input neurons*/
   int nbInputs;

   /**Pointer to the weight vector*/
   float *weights;

   /*type of activation function (tansig, sigmoid, lin)*/
   std::string funcType;

   /**Offset of the layer weight vector in the network weight vector*/
   int weightOffset;

   /**Offset of the layer's first neuron in the list of all network neurons 
      (not too sure about that one, though)*/
   int neuronOffset;

   /**Offset of the layer's first neuron derivative in the list of all network neurons*/
   float derivOffset;

  public:
   /**This (empty) constructor is used for parsing a layer from a file*/
   FFLayer() 
      : derivOffset(0) 
   {}

   //FFLayer(float *_weights) : weights(_weights) {};

   /**Standard constructor*/
   FFLayer(int _nbNeurons, int _nbInputs, float *_weights, int _weightOffset, int _neuronOffset, std::string type = "tansig");

   /**Unimplemented yet (not sure if we *should* implement it)*/
   FFLayer(const FFLayer &layer) {std::cerr << "I wouldn't do that if I were you\n";}
   
   /**Called after reading a layer to setup the weight vector correctly*/
   void setupAfterRead(float *_weights, int _weightOffset, int _neuronOffset);

   ~FFLayer() 
   {
   }

   /**Calculates all the activation functions (and derivatives) for a given input*/
   void update(const float *previous, float *value, float *deriv=NULL)
   {
      for (int i=0;i<nbNeurons;i++)
      {
	 float *w=weights + i*(nbInputs+1);	    	 
	 value[i] = vec_inner_prod(w, previous, nbInputs) + w[nbInputs];

      }

      if (func == tansig)
      {
	 tansig(value, value, nbNeurons);
	 if (deriv)
	    deriv_tansig(value, deriv, nbNeurons);
      } else if (func == lin)
      {
	 //lin(value, value, nbNeurons);
	 if (deriv)
	    deriv_lin(value, deriv, nbNeurons);
      } else if (func == sigmoid)
      {
	 sigmoid(value, value, nbNeurons);
	 if (deriv)
	    deriv_sigmoid(value, deriv, nbNeurons);
      } else {
	 std::cerr << "unknown\n";
	 func(value, value, nbNeurons);
	 if (deriv)
	    deriv_func(value, deriv, nbNeurons);
      }
      if (deriv)
	 vec_add_scal(derivOffset, deriv, deriv, nbNeurons);
   }

   /**Layer size*/
   int size() {return nbNeurons;}

   /**Number of weights*/
   int getNbWeights() {return nbNeurons*(nbInputs+1);}

   /**Offset of a certain neuron's weight vector relative to the whole network's
      weight vector*/
   int getNeuronWeightOffset(int i) {return weightOffset+i*(nbInputs+1);}

   int getWeightOffset() {return weightOffset;}

   int getNeuronOffset() {return neuronOffset;}

   void init(float minmax);

   /**Initializes the layer values given the means and standard deviations of each 
      of the inputs*/
   void init(double *mean, double *std);

   /**Sets the bias vector*/
   void setBias(double *minmax);

   float *getWeights(int i) {return weights + i*(nbInputs+1);}

   /**Writes the layer to a stream*/
   void printOn(std::ostream &out) const;

   /**Reads the layer from a stream (remember to call setupAfterRead after that*/
   void readFrom (std::istream &in);

   void setDerivOffset(float d) {derivOffset=d;}
};

/**Extraction operator for FFLayer*/
std::istream &operator >> (std::istream &in, FFLayer &layer);

}//namespace FD


#endif
