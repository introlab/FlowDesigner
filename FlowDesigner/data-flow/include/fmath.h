#ifndef FMATH_H
#define FMATH_H

#include <math.h>

#define FLOGLOOKUP2SIZE 8192
#define FLOGLOOKUP2SHIFT 10

/*
#define FLOGLOOKUP2SIZE 8388608
#define FLOGLOOKUP2SHIFT 0
*/

extern float logtable1[256];
extern float logtable2[FLOGLOOKUP2SIZE];

   
inline float flog(float f)
{
   unsigned int *cas=(unsigned int *)(&f);
   unsigned int id1 = (*cas)>>23;
   unsigned int id2 = ((*cas) & 0x007fffff)>>FLOGLOOKUP2SHIFT;
   return logtable1[id1] + logtable2[id2];
}




#endif
