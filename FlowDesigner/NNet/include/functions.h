#ifndef FUNCTIONS_H
#define FUNCTIONS_H


#include "vec.h"

static float *calc_tansig_table()
{
   float *table = new float [2001];
   for (int i=0;i<2001;i++)
   {
      float xx = .01*i - 10;
      table[i] = 2/(1+exp(-2*xx)) - 1;
   }
   
   return table;
}

static float *tansig_table = calc_tansig_table();

static float *calc_sigmoid_table()
{
   float *table = new float [2001];
   for (int i=0;i<2001;i++)
   {
      float xx = .01*i - 10;
      table[i] = 1/(1+exp(-xx));
   }
   
   return table;
}

static float *sigmoid_table = calc_sigmoid_table();

inline void sigmoid(float *x, float *y, int len)
{
   for (int i=0;i<len;i++)
   {
      float xx=*x++;

      if (xx>9.9)
	 xx=9.9;
      else if (xx<-9.9)
	 xx=-9.9;
      
      float n = xx*100.0+1000.0;
      int n1 = int(n);
      float f = n - n1;
      *y++ = (1-f)*sigmoid_table[n1] + f*sigmoid_table[n1+1];
      
   }
}

inline void deriv_sigmoid(float *x, float *y, int len)
{
   /*for (int i=0;i<len;i++)
     {
     *y++ = *x * (1-*x);
     x++;
     }
   */ 
   //This is just an unrolled version of the previous section
   float *end = x+len;
   while (x<end-3)
   {
      *y++ = *x * (1-*x);
      x++;
      *y++ = *x * (1-*x);
      x++;
      *y++ = *x * (1-*x);
      x++;
      *y++ = *x * (1-*x);
      x++;
   }
   while (x<end)
   {
      *y++ = *x * (1-*x);
      x++;
   }


}

inline void tansig(float *x, float *y, int len)
{
   for (int i=0;i<len;i++)
   {
      float xx=*x++;

      if (xx>9.9)
	 xx=9.9;
      else if (xx<-9.9)
	 xx=-9.9;
      
      float n = xx*100.0+1000.0;
      int n1 = int(n);
      float f = n - n1;
      *y++ = (1-f)*tansig_table[n1] + f*tansig_table[n1+1];
   }
}

inline void deriv_tansig(float *x, float *y, int len)
{
   /*for (int i=0;i<len;i++)
   {
      *y++ = (1- (*x)*(*x));
      x++;
      }*/

   //This is just an unrolled version of the previous section
   float *end = x+len;
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

inline void lin(float *x, float *y, int len)
{
   for (int i=0;i<len;i++)
      *y++ = *x++;
}

inline void deriv_lin(float *x, float *y, int len)
{
   for (int i=0;i<len;i++)
      *y++ = 1;
}

#endif
