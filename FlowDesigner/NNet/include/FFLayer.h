#ifndef FFLAYER_H
#define FFLAYER_H

#include <math.h>
#include <iostream>
#include "Object.h"
#include <stdlib.h>
#include "functions.h"


class FFLayer : public Object {
  public:
   void (*func) (float *, float *, int);
   void (*deriv_func) (float *, float *, int);
  protected:
   int nbNeurons;
   int nbInputs;
   float *weights;
   string funcType;
   int weightOffset, neuronOffset;
   float derivOffset;

  public:
   FFLayer() 
      : derivOffset(0) 
   {}

   //FFLayer(float *_weights) : weights(_weights) {};

   FFLayer(int _nbNeurons, int _nbInputs, float *_weights, int _weightOffset, int _neuronOffset, string type = "tansig");

   FFLayer(const FFLayer &layer) {cerr << "I wouldn't do that if I were you\n";}
   
   void setupAfterRead(float *_weights, int _weightOffset, int _neuronOffset);

   ~FFLayer() 
   {
   }

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
	 cerr << "unknown\n";
	 func(value, value, nbNeurons);
	 if (deriv)
	    deriv_func(value, deriv, nbNeurons);
      }
      if (deriv)
	 vec_add_scal(derivOffset, deriv, deriv, nbNeurons);
   }

   int size() {return nbNeurons;}

   int getNbWeights() {return nbNeurons*(nbInputs+1);}

   int getNeuronWeightOffset(int i) {return weightOffset+i*(nbInputs+1);}

   int getWeightOffset() {return weightOffset;}

   int getNeuronOffset() {return neuronOffset;}

   void init(float minmax);

   void init(float *mean, float *std);

   void setBias(float *minmax);

   float *getWeights(int i) {return weights + i*(nbInputs+1);}

   void printOn(ostream &out) const;

   void readFrom (istream &in);

   void setDerivOffset(float d) {derivOffset=d;}
};

istream &operator >> (istream &in, FFLayer &layer);

#endif
