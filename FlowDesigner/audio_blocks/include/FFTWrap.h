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

#if (FFTW_ENABLE_FLOAT == 1)

typedef fftw_complex fft_complex;


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
      RFFTPlanMap::iterator plan_pair = RFFTPlans[1].find(size);
      rfftw_plan *plan;
      if (plan_pair == RFFTPlans[1].end())
      {
	 RFFTPlans[1][size] = rfftw_create_plan (size, FFTW_BACKWARD, FFTW_ESTIMATE);
	 plan = &RFFTPlans[1][size];
      } else {
	 plan = &plan_pair->second;
      }
      
      rfftw_one (*plan, const_cast <float *> (in), out);

   }

   
};
#else /* (FFTW_ENABLE_FLOAT == 1) */

#error You do not have FFTW compiled with the --enable-float option. Either compile it with --enable-float or do not use it (you won't be able to compute FFT's)
/*
typedef fftw_complex fft_complex;


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

   void fft (const complex<float> *fin, complex<float> *fout, int size)
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

   void ifft (const float *fin, float *fout, int size)
   {
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
   }

   void rfft (const float *fin, float *fout, int size)
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

   void irfft (const float *fin, float *fout, int size)
   {
      RFFTPlanMap::iterator plan_pair = RFFTPlans[1].find(size);
      rfftw_plan *plan;
      if (plan_pair == RFFTPlans[1].end())
      {
	 RFFTPlans[1][size] = rfftw_create_plan (size, FFTW_BACKWARD, FFTW_ESTIMATE);
	 plan = &RFFTPlans[1][size];
      } else {
	 plan = &plan_pair->second;
      }
      
      rfftw_one (*plan, const_cast <float *> (in), out);

   }

   
};
*/

#endif /* (FFTW_ENABLE_FLOAT == 1) */


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
