#include "fmath.h"



int build_flog_table()
{
   FloatManip m;
   
   for (int i=0;i<FLOGLOOKUP2SIZE;i++)
   {
      m.i = (i<<FLOGLOOKUP2SHIFT) | 0x3f800000;
      logtable2[i]=log(m.f);
   }
   return 0;
}

static int tmp_dummy_flog = build_flog_table();

float logtable2[FLOGLOOKUP2SIZE];




int build_fexp_table()
{
   FloatManip m;
   
   for (int i=0;i<FEXPSIZE;i++)
   {
      m.i = i<<FEXPSHIFT;
      if (m.f<70)
	 exptable[i]=exp(m.f);
      else
	 exptable[i]=1e34;
   }
   return 0;
}

static int tmp_dummy_fexp = build_fexp_table();

float exptable[FEXPSIZE];

