// Copyright (C) 1999 Jean-Marc Valin

#ifndef FFTWRAP_H
#define FFTWRAP_H

using namespace std;

#ifndef STUPID_COMPLEX_KLUDGE
#include <complex>
#endif


#ifdef HAVE_FFTW
#include <fftw.h>
#include <rfftw.h>


//typedef fftw_complex fft_complex;


#ifdef NO_HASH_MAP
#include <map>
#else
#ifdef HAVE_HASH_MAP
#include <hash_map>
#elif defined (HAVE_EXT_HASH_MAP)
#include <ext/hash_map>
#endif
#endif

class _FFTWrap {
#ifdef NO_HASH_MAP
   typedef map<int, rfftw_plan> FFTPlanMap;
   typedef map<int, rfftw_plan> RFFTPlanMap;
#else
   typedef hash_map<int, rfftw_plan, hash<int> > FFTPlanMap;
   typedef hash_map<int, rfftw_plan, hash<int> > RFFTPlanMap;
#endif
   
   FFTPlanMap FFTPlans[2];
   RFFTPlanMap RFFTPlans[2];
  public:

   ~_FFTWrap() 
   {
      for (int i=0;i<2;i++)
	 for (RFFTPlanMap::iterator plan_pair = RFFTPlans[i].begin(); plan_pair != RFFTPlans[i].end(); plan_pair++)
	    rfftw_destroy_plan(plan_pair->second);
      for (int i=0;i<2;i++)
	 for (FFTPlanMap::iterator plan_pair = FFTPlans[i].begin(); plan_pair != FFTPlans[i].end(); plan_pair++)
	    fftw_destroy_plan(plan_pair->second);
   }

#ifndef STUPID_COMPLEX_KLUDGE
   void fft (const complex<float> *fin, complex<float> *fout, int size)
   {
      FFTW_COMPLEX in[size];
      FFTW_COMPLEX out[size];
      for (int i=0;i<size;i++)
      {
	 in[i].re = fin[i].real();
	 in[i].im = fin[i].imag();
      }
      FFTPlanMap::iterator plan_pair = FFTPlans[0].find(size);
      fftw_plan *plan;
      if (plan_pair == FFTPlans[0].end())
      {
	 FFTPlans[0][size] = fftw_create_plan (size, FFTW_FORWARD, FFTW_ESTIMATE);
	 plan = &FFTPlans[0][size];
      } else {
	 plan = &plan_pair->second;
      }
      
      fftw_one (*plan, const_cast <FFTW_COMPLEX *> (in), out);

      for (int i=0;i<size;i++)
	 fout[i] = complex<float> (out[i].re, out[i].im);
   }

   void ifft (const complex<float> *fin, complex<float> *fout, int size)
   {
      FFTW_COMPLEX in[size];
      FFTW_COMPLEX out[size];
      for (int i=0;i<size;i++)
      {
	 in[i].re = fin[i].real();
	 in[i].im = fin[i].imag();
      }
      FFTPlanMap::iterator plan_pair = FFTPlans[1].find(size);
      fftw_plan *plan;
      if (plan_pair == FFTPlans[1].end())
      {
	 FFTPlans[1][size] = fftw_create_plan (size, FFTW_BACKWARD, FFTW_ESTIMATE);
	 plan = &FFTPlans[1][size];
      } else {
	 plan = &plan_pair->second;
      }
      
      fftw_one (*plan, const_cast <FFTW_COMPLEX *> (in), out);

      for (int i=0;i<size;i++)
	 fout[i] = complex<float> (out[i].re, out[i].im);
   }
#endif

   void rfft (const float *fin, float *fout, int size)
   {
      FFTW_REAL in[size];
      FFTW_REAL out[size];
      for (int i=0;i<size;i++)
	 in[i]=fin[i];
      RFFTPlanMap::iterator plan_pair = RFFTPlans[0].find(size);
      rfftw_plan *plan;
      if (plan_pair == RFFTPlans[0].end())
      {
	 RFFTPlans[0][size] = rfftw_create_plan (size, FFTW_FORWARD, FFTW_ESTIMATE);
	 plan = &RFFTPlans[0][size];
      } else {
	 plan = &plan_pair->second;
      }
      
      rfftw_one (*plan, const_cast <FFTW_REAL *> (in), out);
      for (int i=0;i<size;i++)
	 fout[i]=out[i];
   }

   void irfft (const float *fin, float *fout, int size)
   {
      FFTW_REAL in[size];
      FFTW_REAL out[size];
      for (int i=0;i<size;i++)
	 in[i]=fin[i];
      RFFTPlanMap::iterator plan_pair = RFFTPlans[1].find(size);
      rfftw_plan *plan;
      if (plan_pair == RFFTPlans[1].end())
      {
	 RFFTPlans[1][size] = rfftw_create_plan (size, FFTW_BACKWARD, FFTW_ESTIMATE);
	 plan = &RFFTPlans[1][size];
      } else {
	 plan = &plan_pair->second;
      }
      
      rfftw_one (*plan, const_cast <FFTW_REAL *> (in), out);
      for (int i=0;i<size;i++)
	 fout[i]=out[i];

   }

   
};




#else /* ifdef HAVE_FFTW */

#warning FFTW is not present. Overflow will use a (very, very) slow DFT implementation.
#include <iostream>
#include <math.h>


class _FFTWrap {
  public:
#ifndef STUPID_COMPLEX_KLUDGE
   void fft (const complex<float> *fin, complex<float> *fout, int size)
   {
      float fact = 2*M_PI/size;
      for (int i=0;i<size;i++)
      {
	 fout[i] = complex<float> (0,0);
	 for (int j=0;j<size;j++)
	 {
	    complex<float> c(cos(fact*j*i),-sin(fact*j*i));
	    fout[i] += c*fin[j];
	 }
      }
   }

   void ifft (const complex<float> *fin, complex<float> *fout, int size)
   {
      float fact = 2*M_PI/size;
      for (int i=0;i<size;i++)
      {
	 fout[i] = complex<float> (0,0);
	 for (int j=0;j<size;j++)
	 {
	    complex<float> c(cos(fact*j*i),sin(fact*j*i));
	    fout[i] += c*fin[j];
	 }
      }
   }
#endif

   void rfft (const float *fin, float *fout, int size)
   {
      float fact = 2*M_PI/size;
      for (int i=0;i<size;i++)
	 fout[i] = 0;
      for (int i=1;i<(size+1)>>1;i++)
      {
	 for (int j=0;j<size;j++)
	 {
	    fout[i] += fin[j]*cos(fact*j*i);
	    fout[size-i] -= fin[j]*sin(fact*j*i);
	 }
      }
      for (int j=0;j<size;j++)
      {
	 fout[0] += fin[j];
      }
      if (size&1)
	 for (int j=0;j<size;j++)
	 {
	    if (j&1)
	       fout[size>>1] -= fin[j];
	    else
	       fout[size>>1] += fin[j];
	 }
   }

   void irfft (const float *fin, float *fout, int size)
   {
      float fact = 2*M_PI/size;
      for (int i=0;i<size;i++)
	 fout[i] = 0;
      for (int i=1;i<(size+1)>>1;i++)
      {
	 for (int j=0;j<size;j++)
	 {
	    fout[i] += fin[j]*cos(fact*j*i);
	    fout[size-i] += fin[j]*sin(fact*j*i);
	 }
      }
      for (int j=0;j<size;j++)
      {
	 fout[0] += fin[j];
      }
      if (size&1)
	 for (int j=0;j<size;j++)
	 {
	    if (j&1)
	       fout[size>>1] -= fin[j];
	    else
	       fout[size>>1] += fin[j];
	 }
   }


};

#endif /* ifdef HAVE_FFTW */

extern _FFTWrap FFTWrap;

#endif
