#include "fmath.h"

int build_table()
{
   float f;
   int *cas=(int *)(&f);
   for (int i=0;i<256;i++)
   {
      int tmp=i<<23;
      (*cas)=tmp;
      logtable1[i]=log(f);
   }
   
   for (int i=0;i<FLOGLOOKUP2SIZE;i++)
   {
      int tmp = (i<<FLOGLOOKUP2SHIFT) | 0x3f800000;
      (*cas)=tmp;
      logtable2[i]=log(f);
   }
   return 0;
}

static int tmp_dummt_flog = build_table();

float logtable1[256];
float logtable2[FLOGLOOKUP2SIZE];
