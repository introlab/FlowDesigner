#ifndef FFLAYER_H
#define FFLAYER_H

#include <math.h>
#include <stream.h>
#include "Object.h"
#include <stdlib.h>


static double *calc_tansig_table()
{
   double *table = new double [2001];
   for (int i=0;i<2001;i++)
   {
      double xx = .01*i - 10;
      table[i] = 2/(1+exp(-2*xx)) - 1;
   }
   
   return table;
}

static double *tansig_table = calc_tansig_table();


inline void sigmoid(double *x, double *y, int len)
{
   for (int i=0;i<len;i++)
   {
      double xx=*x++;

      if (xx>10)
	 xx=10;
      else if (xx<-10)
	 xx=-10;
      *y++ = 1/(1+exp(-xx));
   }
}

inline void deriv_sigmoid(double *x, double *y, int len)
{
   for (int i=0;i<len;i++)
   {
      *y++ = *x * (1-*x);
      x++;
   }
}

inline void tansig(double *x, double *y, int len)
{
   for (int i=0;i<len;i++)
   {
      double xx=*x++;

      if (xx>9.9)
	 xx=9.9;
      else if (xx<-9.9)
	 xx=-9.9;
      
      double n = xx*100.0+1000.0;
      //int n1 = int(floor(n));
      int n1 = int(n);
      double f = n - n1;
      *y++ = (1-f)*tansig_table[n1] + f*tansig_table[n1+1];
      //*y++ = 2/(1+exp(-2*xx)) - 1;
   }
}

inline void deriv_tansig(double *x, double *y, int len)
{
   /*for (int i=0;i<len;i++)
   {
      *y++ = (1- (*x)*(*x));
      x++;
      }*/

   //This is just an unrolled version of the previous section
   double *end = x+len;
   while (x<end-3)
   {
      *y++ = (1- (*x)*(*x));
      x++;
      *y++ = (1- (*x)*(*x));
      x++;
      *y++ = (1- (*x)*(*x));
      x++;
      *y++ = (1- (*x)*(*x));
      x++;
   }
   while (x<end)
   {
      *y++ = (1- (*x)*(*x));
      x++;
   }

}

/*inline double deriv_tanh(double *x, double *y, int len)
{
   //float c=cosh(x);
   //return 1/(c*c);

   //return 1;
   
   double c = cosh(atanh(x));
   return 1/(c*c);
   //return 1/(x*x);
}
*/

inline void lin(double *x, double *y, int len)
{
   for (int i=0;i<len;i++)
      *y++ = *x++;
}

inline void deriv_lin(double *x, double *y, int len)
{
   for (int i=0;i<len;i++)
      *y++ = 1;
}

//#define func(x) sigmoid(x)
//#define deriv(x) deriv_sigmoid(x)

//#define func(x) (x)
//#define deriv(x) (1)

class FFLayer : public Object {
  public:
   void (*func) (double *, double *, int);
   void (*deriv_func) (double *, double *, int);
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

	    double sum1=0,sum2=0,sum3=0,sum4=0;
	    while (p < end-3)
	    {
	       sum1 += *w++ * *p++;
	       sum2 += *w++ * *p++;
	       sum3 += *w++ * *p++;
	       sum4 += *w++ * *p++;
	    }
	    value[i] = sum1+sum2+sum3+sum4;
	    while (p<end)
	       value[i] += *w++ * *p++;
	    value[i] += *w;

	    /*while (p<end)
	       value[i] += *w++ * *p++;
	       value[i] += *w;*/

	    /*
	    value[i]=w[nbInputs];
	    for (int j=0;j<nbInputs;j++)
	    value[i] += w[j]*previous[j];
	    */
	    //value[i]=func(value[i]);
	    //deriv[i]=deriv_func(value[i]);
	 }
	 if (func == tansig)
	 {
	    tansig(value, value, nbNeurons);
	    deriv_tansig(value, deriv, nbNeurons);
	 } else if (func == lin)
	 {
	    lin(value, value, nbNeurons);
	    deriv_lin(value, deriv, nbNeurons);
	 } else if (func == sigmoid)
	 {
	    sigmoid(value, value, nbNeurons);
	    deriv_sigmoid(value, deriv, nbNeurons);
	 } else {
	    cerr << "unknown\n";
	    func(value, value, nbNeurons);
	    deriv_func(value, deriv, nbNeurons);
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

   void setDeltaRate(double rate)
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	    saved_weights[i] = rate;
      }
   void deltaBar(double mom, double inc, double dec)
      {
	 for (int i=0;i<nbNeurons*(nbInputs+1);i++)
	 {
	    double x = gradient[i]/momentum[i];
	    if (fabs(momentum[i]) < 1e-12)
	       x=0;
	    if (x<-10)
	       x=-10;
	    if (x>10)
	       x=10;
	    saved_weights[i] *= .02+1.03/(1+exp((-2*x-2)));
	    /*if (saved_weights[i] > 10) 
	       saved_weights[i] = 10;*/
	       /*if (momentum[i]*gradient[i] > 0)
	       saved_weights[i] *= inc;
	    else
	    saved_weights[i] *= dec;*/
	    momentum[i] = mom*momentum[i] + gradient[i];
	    weights[i] += saved_weights[i]*gradient[i];
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
