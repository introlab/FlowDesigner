// Copyright (C) 1999 Jean-Marc Valin

#ifndef FFTWRAP_H
#define FFTWRAP_H

#ifdef WIN32 /*Work around bug in MSVC++ (for) variable scope*/
#define for if(0);else for
#endif

#ifndef STUPID_COMPLEX_KLUDGE
#include <complex>
#endif

#include "misc.h"
#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

#if (__GNUC__ == 3 && __GNUC_MINOR__ >= 1 && HAVE_EXT_HASH_MAP)
using namespace __gnu_cxx;
#endif

#ifdef HAVE_FFTW

#include <fftw.h>
#include <rfftw.h>

//typedef fftw_complex fft_complex;

#define NO_HASH_MAP

#ifdef NO_HASH_MAP
#include <map>
#else
#ifdef HAVE_HASH_MAP
#include <hash_map>
#elif defined (HAVE_EXT_HASH_MAP)
#include <ext/hash_map>
#endif

#endif /*ifdef NO_HASH_MAP*/

namespace FD {
	
	class _FFTWrap {
#ifdef NO_HASH_MAP
		typedef std::map<int, rfftw_plan> FFTPlanMap;
		typedef std::map<int, rfftw_plan> RFFTPlanMap;
#else
		typedef std::hash_map<int, rfftw_plan, hash<int> > FFTPlanMap;
		typedef std::hash_map<int, rfftw_plan, hash<int> > RFFTPlanMap;
		//typedef std::map<int, rfftw_plan> FFTPlanMap;
		// typedef std::map<int, rfftw_plan> RFFTPlanMap;
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
		void fft (const std::complex<float> *fin, std::complex<float> *fout, int size)
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
				fout[i] = std::complex<float> (out[i].re, out[i].im);
		}
		
		void ifft (const std::complex<float> *fin, std::complex<float> *fout, int size)
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
				fout[i] = std::complex<float> (out[i].re, out[i].im);
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
	
}//namespace FD


#else /* ifdef HAVE_FFTW */

#if 0
#warning FFTW is not present. FlowDesigner will use a (very, very) slow DFT implementation.
#endif

#include <iostream>
#include <math.h>

namespace FD {
	
	class _FFTWrap {
	public:
#ifndef STUPID_COMPLEX_KLUDGE
		void fft (const std::complex<float> *fin, std::complex<float> *fout, int size)
		{
			float fact = 2*M_PI/size;
			for (int i=0;i<size;i++)
			{
				fout[i] = std::complex<float> (0,0);
				for (int j=0;j<size;j++)
				{
					std::complex<float> c(cos(fact*j*i),-sin(fact*j*i));
					fout[i] += c*fin[j];
				}
			}
		}
		
		void ifft (const std::complex<float> *fin, std::complex<float> *fout, int size)
		{
			float fact = 2*M_PI/size;
			for (int i=0;i<size;i++)
			{
				fout[i] = std::complex<float> (0,0);
				for (int j=0;j<size;j++)
				{
					std::complex<float> c(cos(fact*j*i),sin(fact*j*i));
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
	
}//namespace FD

#endif /* ifdef HAVE_FFTW */


namespace FD {
	
	extern _FFTWrap FFTWrap;
	
}//namespace FD

#endif
