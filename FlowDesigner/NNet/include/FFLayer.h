#ifndef FFLAYER_H
#define FFLAYER_H

#include <math.h>
#include <stream.h>
#include "Object.h"
#include <stdlib.h>

inline double sigmoid(double x)
{
   if (x>10)
      return 1.0;
   else if (x<-10)
      return 0;
   return 1/(1+exp(-x));
}

inline double deriv_sigmoid(double x)
{
   //return 20*(.05*x+.5)*(1 - (.05*x+.5));
   return x*(1-x);
}

inline double tansig(double x)
{
   if (x>10)
      return 1.0;
   else if (x<-10)
      return 0;
   return 2/(1+exp(-2*x)) - 1;
}

inline double deriv_tansig(double x)
{
   //return 20*(.05*x+.5)*(1 - (.05*x+.5));
   return 1-x*x;
}

inline double deriv_tanh(double x)
{
   //float c=cosh(x);
   //return 1/(c*c);

   //return 1;
   
   double c = cosh(atanh(x));
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
   double *tmp_weights;
   double *deriv;
  protected:
   int nbNeurons;
   int nbInputs;
   double *weights;
   double *value;
   double *error;
   string funcType;
   double *momentum;
  public:
   FFLayer() {};
   FFLayer(int _nbNeurons, int _nbInputs, string type = "tansig");
   ~FFLayer() {delete tmp_weights; delete value; delete weights; delete error; delete deriv;}
   void update(const double *previous)
      {
	 for (int i=0;i<nbNeurons;i++)
	 {
	    double *w=weights + i*(nbInputs+1);
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
	    tmp_weights[i]=0;
	 }
      }
   void copyFromTmp(double mom)
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    momentum[i] = mom*momentum[i] + (1-mom)*tmp_weights[i];
	    //momentum[i] += weights[i]*.00001*(rand()%100-50);
	    weights[i]+=momentum[i];
	 }
      }
   void undo()
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    weights[i]-=momentum[i];
	 }
      }
   void init(double minmax);
   double *getValue() {return value;}
   double *getWeights(int i) {return weights + i*(nbInputs+1);}
   double *getTmpWeights(int i) {return tmp_weights + i*(nbInputs+1);}
   double *getError() {return error;}
   void printOn(ostream &out) const;
   void readFrom (istream &in);
};

istream &operator >> (istream &in, FFLayer &layer);

#endif;
