// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#ifndef FFTWRAP_H
#define FFTWRAP_H

using namespace std;


#ifdef HAVE_FFTW
#include <fftw.h>
#include <rfftw.h>
typedef fftw_complex fft_complex;


#ifdef NO_HASH_MAP
#include <map>
#else
#include <hash_map>
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

   void fft (const FFTW_COMPLEX *in, FFTW_COMPLEX *out, int size)
   {
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
   }

   void ifft (const FFTW_COMPLEX *in, FFTW_COMPLEX *out, int size)
   {
      FFTPlanMap::iterator plan_pair = FFTPlans[0].find(size);
      fftw_plan *plan;
      if (plan_pair == FFTPlans[0].end())
      {
	 FFTPlans[0][size] = fftw_create_plan (size, FFTW_BACKWARD, FFTW_ESTIMATE);
	 plan = &FFTPlans[0][size];
      } else {
	 plan = &plan_pair->second;
      }
      
      fftw_one (*plan, const_cast <FFTW_COMPLEX *> (in), out);
   }

   void rfft (const FFTW_REAL *in, FFTW_REAL *out, int size)
   {
      RFFTPlanMap::iterator plan_pair = RFFTPlans[0].find(size);
      rfftw_plan *plan;
      if (plan_pair == RFFTPlans[0].end())
      {
	 RFFTPlans[0][size] = rfftw_create_plan (size, FFTW_FORWARD, FFTW_ESTIMATE);
	 plan = &RFFTPlans[0][size];
      } else {
	 plan = &plan_pair->second;
      }
      
      rfftw_one (*plan, const_cast <float *> (in), out);
   }

   void irfft (const FFTW_REAL *in, FFTW_REAL *out, int size)
   {
      RFFTPlanMap::iterator plan_pair = RFFTPlans[0].find(size);
      rfftw_plan *plan;
      if (plan_pair == RFFTPlans[0].end())
      {
	 RFFTPlans[0][size] = rfftw_create_plan (size, FFTW_BACKWARD, FFTW_ESTIMATE);
	 plan = &RFFTPlans[0][size];
      } else {
	 plan = &plan_pair->second;
      }
      
      rfftw_one (*plan, const_cast <float *> (in), out);

   }

   
};
#else /* ifdef HAVE_FFTW */

#include <iostream>

class fft_complex {
  public:
   float re;
   float im;
};

class _FFTWrap {
  public:
   void fft (const fft_complex *in, fft_complex *out, int size)
   {
      cerr << "error: FFTW is not present" << endl;
      exit(1);
   }

   void ifft (const fft_complex *in, fft_complex *out, int size)
   {
      cerr << "error: FFTW is not present" << endl;
      exit(1);
   }

   void rfft (const float *in, float *out, int size)
   {
      cerr << "error: FFTW is not present" << endl;
      exit(1);
   }

   void irfft (const float *in, float *out, int size)
   {
      cerr << "error: FFTW is not present" << endl;
      exit(1);
   }


};

#endif /* ifdef HAVE_FFTW */

extern _FFTWrap FFTWrap;

#endif
