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
   double *gradient;
   double *saved_weights;
   double *deriv;
  protected:
   int nbNeurons;
   int nbInputs;
   double *weights;
   double *value;
   double *error;
   string funcType;
   double *momentum;

   bool alloc;
  public:
   FFLayer() : alloc(false) {};
   FFLayer(int _nbNeurons, int _nbInputs, string type = "tansig");
   ~FFLayer() 
      {
	 if (alloc)
	 {
	    delete [] gradient; 
	    delete [] saved_weights; 
	    delete [] value; 
	    delete [] weights; 
	    delete [] error; 
	    delete [] deriv;
	 }
      }
   void update(const double *previous)
      {
	 for (int i=0;i<nbNeurons;i++)
	 {
	    double *w=weights + i*(nbInputs+1);
	    
	    const double *p=previous;
	    const double *end=p+nbInputs;
	    value[i]=0;
	    while (p<end)
	       value[i] += *w++ * *p++;
	    value[i] += *w;
	    /*
	    value[i]=w[nbInputs];
	    for (int j=0;j<nbInputs;j++)
	    value[i] += w[j]*previous[j];
	    */
	    value[i]=func(value[i]);
	    deriv[i]=deriv_func(value[i]);
	 }
      }
   void saveWeights()
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    saved_weights[i]=weights[i];
	 }
      }
   void loadWeights()
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    weights[i]=saved_weights[i];
	 }
      }
   void resetGradient()
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    gradient[i]=0;
	 }
      }
   void updateGradient(double alpha, double mom)
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    //momentum[i] = mom*momentum[i] + (1-mom)*gradient[i];
	    momentum[i] = mom*momentum[i] + alpha*gradient[i];
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
   double *getGradient(int i) {return gradient + i*(nbInputs+1);}
   double *getError() {return error;}
   void printOn(ostream &out) const;
   void readFrom (istream &in);
};

istream &operator >> (istream &in, FFLayer &layer);

#endif;
