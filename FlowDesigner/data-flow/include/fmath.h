// Copyright (C) 2001 Jean-Marc Valin

/* WARNING: These routines are mostly untested, and I don't know
   for sure how fast and how accurate they are. Use at your own risks
*/

#ifndef FMATH_H
#define FMATH_H

#include <math.h>

#ifdef HAVE_FLOAT_H
#include <float.h>
#endif

namespace FD {
#ifdef WIN32 /*Work around bug in MSVC++ (for) variable scope*/
#define for if(0);else for
#endif

union FloatManip {
      float f;
      unsigned int i;
};

#ifndef M_LN2
#define M_LN2 0.69314718055994530942
#endif

/*#define FLOGLOOKUP2SIZE 8192
#define FLOGLOOKUP2SHIFT 10
*/

#define FLOGLOOKUP2SIZE 256
#define FLOGLOOKUP2SHIFT 15


/*
#define FLOGLOOKUP2SIZE 8388608
#define FLOGLOOKUP2SHIFT 0
*/

extern float logtable2[FLOGLOOKUP2SIZE];

inline void build_flog_table()
{
   static bool init=false;
   if (!init)
   {
      FloatManip m;
      
      for (int i=0;i<FLOGLOOKUP2SIZE;i++)
      {
	 m.i = (i<<FLOGLOOKUP2SHIFT) | 0x3f800000;
	 logtable2[i]=log(m.f);
      }
      init=true;
   }
}


//Log (base e) approximation
inline float flog(float f)
{
   build_flog_table();
   FloatManip m;
   m.f = f;
   //The exponent in id1
   unsigned int id1 = m.i>>23;
   //The first bits of the mantissa in id2
   unsigned int id2 = (m.i & 0x007fffff)>>FLOGLOOKUP2SHIFT;
   float f2=m.f;
   //Refining step: first order taylor
   m.i &= 0xffff8000;
   return (int(id1)-127)*M_LN2 + logtable2[id2] + (f2-m.f)/f2;
}

//Log (base e) rough approximation
inline void fflogv(const float *fin, float *fout, int len)
{
   build_flog_table();
   FloatManip m;
   for (int i=0;i<len;i++)
   {
      m.f = fin[i];
      //Extract the mantissa and perform lookup for log(mantissa)
      float tb = logtable2[(m.i & 0x007fffff)>>FLOGLOOKUP2SHIFT];
      //Extract the exponent
      int id = (m.i>>23)-127;
      //log(mantissa*2^exponent) = exponent*log(2) + log(mantissa)
      fout[i] = id*M_LN2 + tb;
   }
}

/*#define FEXPSHIFT 19
#define FEXPSIZE 8192
#define FEXPMASK 0xfff80000
*/

#define FEXPSHIFT 22
#define FEXPSIZE 1024
#define FEXPMASK 0xffc00000

extern float exptable[FEXPSIZE];

inline void build_fexp_table()
{
   static bool init=false;
   if (!init)
   {
      FloatManip m;
      
      for (int i=0;i<FEXPSIZE;i++)
      {
	 m.i = i<<FEXPSHIFT;
	 exptable[i]=exp(m.f);
      }
      init=true;
   }
}



//Exponential approximation
inline float fexp(float f)
{
   build_fexp_table();
   FloatManip m;
   m.f = f;
   //Exponent plus part of the mantissa for first approximation
   unsigned int id = m.i>>FEXPSHIFT;
   float val = exptable[id];
   float f2;
   
   //Refining step... we subtract the approximated x from the real x 
   //and lookup again
   f2=m.f;
   m.i &= FEXPMASK;
   m.f=f2-m.f;
   id = m.i>>FEXPSHIFT;
   val *= exptable[id];

   //Second refining step: second order Taylor approximation
   f2=m.f;
   m.i &= FEXPMASK;
   m.f=f2-m.f;
   
   //return val + val*m.f + .5*val*m.f*m.f;
   return val + ( val*m.f * (1+.5*m.f) );
}

}//namespace FD
#endif
