#include "fmath.h"

int build_flog_table()
{
   float f;
   int *cas=(int *)(&f);
   
   for (int i=0;i<FLOGLOOKUP2SIZE;i++)
   {
      int tmp = (i<<FLOGLOOKUP2SHIFT) | 0x3f800000;
      (*cas)=tmp;
      logtable2[i]=log(f);
   }
   return 0;
}

static int tmp_dummy_flog = build_flog_table();

float logtable2[FLOGLOOKUP2SIZE];




int build_fexp_table()
{
   float f;
   int *cas=(int *)(&f);
   
   for (int i=0;i<FEXPSIZE;i++)
   {
      int tmp = i<<FEXPSHIFT;
      (*cas)=tmp;
      exptable[i]=exp(f);
   }
   return 0;
}

static int tmp_dummy_fexp = build_fexp_table();

float exptable[FEXPSIZE];

