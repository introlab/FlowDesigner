#ifndef FFLAYER_H
#define FFLAYER_H

#include <math.h>

class FFLayer {
  protected:
   int nbNeurons;
   int nbInputs;
   float *weights;
   float *value;
   float *error;
  public:
   FFLayer(int _nbNeurons, int _nbInputs);
   void update(float *previous)
      {
	 for (int i=0;i<nbNeurons;i++)
	 {
	    float *w=weights + i*(nbInputs+1);
	    value[i]=w[nbInputs];
	    for (int j=0;j<nbInputs;j++)
	       value[i] += w[j]*previous[j];
	    //value[i]=tanh(value[i]);
	 }
      }
   float *getValue() {return value;}
   float *getWeights(int i) {return weights + i*(nbInputs+1);}
   float *getError() {return error;}

};

#endif;
