#ifndef FFLAYER_H
#define FFLAYER_H

#include <math.h>
#include <stream.h>
#include "Object.h"

inline double sigmoid(double x)
{
   return 1/(1+exp(-x));
}

inline double deriv_sigmoid(double x)
{
   //return 20*(.05*x+.5)*(1 - (.05*x+.5));
   return x*(1-x);
}

inline double deriv_tanh(double x)
{
   //float c=cosh(x);
   //return 1/(c*c);

   //return 1;
   
   float c = cosh(atanh(x));
   return 1/(c*c);
   //return 1/(x*x);
}

inline double lin(double x)
{
   return x;
}

inline double deriv_lin(double x)
{
   return 1;
}

//#define func(x) sigmoid(x)
//#define deriv(x) deriv_sigmoid(x)

//#define func(x) (x)
//#define deriv(x) (1)

class FFLayer : public Object {
  public:
   double (*func) (double);
   double (*deriv_func) (double);
   float *tmp_weights;
   float *deriv;
  protected:
   int nbNeurons;
   int nbInputs;
   float *weights;
   float *value;
   float *error;
   string funcType;
   float *momentum;
  public:
   FFLayer() {};
   FFLayer(int _nbNeurons, int _nbInputs, string type = "tanh");
   ~FFLayer() {delete tmp_weights; delete value; delete weights; delete error; delete deriv;}
   void update(float *previous)
      {
	 for (int i=0;i<nbNeurons;i++)
	 {
	    float *w=weights + i*(nbInputs+1);
	    value[i]=w[nbInputs];
	    for (int j=0;j<nbInputs;j++)
	       value[i] += w[j]*previous[j];
	    //value[i]=tanh(value[i]);
	    value[i]=func(value[i]);
	    deriv[i]=deriv_func(value[i]);
	 }
      }
   void copyToTmp()
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    //tmp_weights[i]=weights[i];
	    tmp_weights[i]=0;
	 }
      }
   void copyFromTmp()
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    //weights[i]=tmp_weights[i];
	    momentum[i] = .90*momentum[i] + tmp_weights[i];
	    weights[i]+=momentum[i];
	    //weights[i]+=tmp_weights[i];
	 }
      }
   float *getValue() {return value;}
   float *getWeights(int i) {return weights + i*(nbInputs+1);}
   float *getTmpWeights(int i) {return tmp_weights + i*(nbInputs+1);}
   float *getError() {return error;}
   void printOn(ostream &out) const;
   void readFrom (istream &in);
};

istream &operator >> (istream &in, FFLayer &layer);

#endif;
