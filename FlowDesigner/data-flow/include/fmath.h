/* WARNING: These routines are mostly untested, and I don't know
   for sure how fast and how accurate they are. Use at your own risks
*/

#ifndef FMATH_H
#define FMATH_H

#include <math.h>

union FloatManip {
      float f;
      unsigned int i;
};


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

//Log (base e) approximation
inline float flog(float f)
{
   FloatManip m;
   m.f = f;
   //The exponent in id1
   unsigned int id1 = m.i>>23;
   //The first bits of the mantissa in id2
   unsigned int id2 = (m.i & 0x007fffff)>>FLOGLOOKUP2SHIFT;
   float f2=m.f;
   //Refining step: first order taylor
   m.i &= 0xffff8000;
   return (id1-127)*M_LN2 + logtable2[id2] + (f2-m.f)/f2;
}



/*#define FEXPSHIFT 19
#define FEXPSIZE 8192
#define FEXPMASK 0xfff80000
*/

#define FEXPSHIFT 22
#define FEXPSIZE 1024
#define FEXPMASK 0xffc00000

extern float exptable[FEXPSIZE];

//Exponential approximation
inline float fexp(float f)
{
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


#endif
