#ifndef FMATH_H
#define FMATH_H

#include <math.h>

#define FLOGLOOKUP2SIZE 8192
#define FLOGLOOKUP2SHIFT 10

/*
#define FLOGLOOKUP2SIZE 8388608
#define FLOGLOOKUP2SHIFT 0
*/

extern float logtable2[FLOGLOOKUP2SIZE];

   
inline float flog(float f)
{
   unsigned int *cas=(unsigned int *)(&f);
   unsigned int id1 = (*cas)>>23;
   unsigned int id2 = ((*cas) & 0x007fffff)>>FLOGLOOKUP2SHIFT;
   return (id1-127)*M_LN2 + logtable2[id2];
}





#define FEXPSHIFT 19
#define FEXPSIZE 8192
#define FEXPMASK 0xfff80000

extern float exptable[FEXPSIZE];


float fexp(float f)
{
   unsigned int id = (*reinterpret_cast<unsigned int *> (&f))>>FEXPSHIFT;
   float val = exptable[id];
   float f2;

   f2=f;
   (*reinterpret_cast<unsigned int *> (&f)) &= FEXPMASK;
   f=f2-f;
   id = (*reinterpret_cast<unsigned int *> (&f))>>FEXPSHIFT;
   val *= exptable[id];

   f2=f;
   (*reinterpret_cast<unsigned int *> (&f)) &= FEXPMASK;
   f=f2-f;
   
   return val + val*f;
}


#endif
