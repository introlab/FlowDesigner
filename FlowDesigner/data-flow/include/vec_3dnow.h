// Copyright (C) 2001 Jean-Marc Valin
//This file contains 3DNow!-optimized vector primitives


#ifndef VEC_3DNOW_H
#define VEC_3DNOW_H


#ifdef _ENABLE_3DNOW


#define CLOBBER_3DNOW   : "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)", "memory"

inline float vec_inner_prod_3dnow(const float *a, const float *b, int len)
{
  float sum[2]={0,0};
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  pxor %%mm4, %%mm4
  pxor %%mm5, %%mm5

  sub $4, %%ecx
  jb mul4_skip%=

mul4_loop%=:
  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%edi), %%mm3
  add $16, %%eax
  add $16, %%edi
  pfmul %%mm0, %%mm1
  pfmul %%mm2, %%mm3
  pfadd %%mm1, %%mm4
  pfadd %%mm3, %%mm5

  sub $4,  %%ecx

  jae mul4_loop%=

  pfadd %%mm5,%%mm4

mul4_skip%=:

  add $2, %%ecx
  jae mul2_skip%=

  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  add $8, %%eax
  add $8, %%edi
  pfmul %%mm0, %%mm1
  pfadd %%mm1, %%mm4
mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  pfadd %%mm1, %%mm4
even%=:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%edi
  pop %%eax
  femms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (sum)
CLOBBER_3DNOW
  );
    
  return sum[0];
}



inline void vec_add_vec_3dnow(const float *a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  push %%edx

  sub $4, %%ecx
  jb mul4_skip%=

mul4_loop%=:
  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%edi), %%mm3
  pfadd %%mm2, %%mm3

  movq %%mm1, (%%edx)
  movq %%mm3, 8(%%edx)
  add $16, %%eax
  add $16, %%edi
  add $16, %%edx
  sub $4,  %%ecx

  jae mul4_loop%=

mul4_skip%=:

  add $2, %%ecx
  jae mul2_skip%=
  
  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%eax
  add $8, %%edi
  add $8, %%edx

mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movd %%mm1, (%%edx)
even%=:

  pop %%edx
  pop %%ecx
  pop %%edi
  pop %%eax
  femms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline void vec_sub_vec_3dnow(const float *a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  push %%edx

  sub $4, %%ecx
  jb mul4_skip%=

mul4_loop%=:
  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  pfsubr %%mm0, %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%edi), %%mm3
  pfsubr %%mm2, %%mm3

  movq %%mm1, (%%edx)
  movq %%mm3, 8(%%edx)
  add $16, %%eax
  add $16, %%edi
  add $16, %%edx
  sub $4,  %%ecx

  jae mul4_loop%=

mul4_skip%=:

  add $2, %%ecx
  jae mul2_skip%=

  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  pfsubr %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%eax
  add $8, %%edi
  add $8, %%edx

mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfsubr %%mm0, %%mm1
  movd %%mm1, (%%edx)
even%=:

  pop %%edx
  pop %%ecx
  pop %%edi
  pop %%eax
  femms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline void vec_mul_vec_3dnow(const float *a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  push %%edx

  sub $4, %%ecx
  jb mul4_skip%=

mul4_loop%=:
  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%edi), %%mm3
  pfmul %%mm2, %%mm3

  movq %%mm1, (%%edx)
  movq %%mm3, 8(%%edx)
  add $16, %%eax
  add $16, %%edi
  add $16, %%edx
  sub $4,  %%ecx

  jae mul4_loop%=

mul4_skip%=:

  add $2, %%ecx
  jae mul2_skip%=

  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%eax
  add $8, %%edi
  add $8, %%edx

mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movd %%mm1, (%%edx)
even%=:

  pop %%edx
  pop %%ecx
  pop %%edi
  pop %%eax
  femms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline void vec_add_scal_3dnow(const float a, const float *b, float *c, int len)
{
  float tmp[2];
  tmp[0]=tmp[1]=a;
  __asm__ __volatile__ (
  "
  push %%edi
  push %%ecx
  push %%edx
  movq (%%eax), %%mm0

  sub $4, %%ecx
  jb mul4_skip%=

mul4_loop%=:
  movq (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movq 8(%%edi), %%mm2
  pfadd %%mm0, %%mm2

  movq %%mm1, (%%edx)
  movq %%mm2, 8(%%edx)
  add $16, %%edi
  add $16, %%edx
  sub $4,  %%ecx

  jae mul4_loop%=

mul4_skip%=:

  add $2, %%ecx
  jae mul2_skip%=
  
  movq (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%edi
  add $8, %%edx

mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movd %%mm1, (%%edx)
even%=:

  pop %%edx
  pop %%ecx
  pop %%edi
  femms
  "
  : : "a" (tmp), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline void vec_mul_scal_3dnow(const float a, const float *b, float *c, int len)
{
  float tmp[2];
  tmp[0]=tmp[1]=a;
  __asm__ __volatile__ (
  "
  push %%edi
  push %%ecx
  push %%edx
  movq (%%eax), %%mm0

  sub $4, %%ecx
  jb mul4_skip%=

mul4_loop%=:
  movq (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movq 8(%%edi), %%mm2
  pfmul %%mm0, %%mm2

  movq %%mm1, (%%edx)
  movq %%mm2, 8(%%edx)
  add $16, %%edi
  add $16, %%edx
  sub $4,  %%ecx

  jae mul4_loop%=

mul4_skip%=:

  add $2, %%ecx
  jae mul2_skip%=
  
  movq (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%edi
  add $8, %%edx

mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movd %%mm1, (%%edx)
even%=:

  pop %%edx
  pop %%ecx
  pop %%edi
  femms
  "
  : : "a" (tmp), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline float vec_dist_3dnow(const float *a, const float *b, int len)
{
  //float sum=0;
  float sum[2]={0,0};
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  pxor %%mm4, %%mm4
  pxor %%mm5, %%mm5

  sub $4, %%ecx
  jb mul4_skip%=

mul4_loop%=:
  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%edi), %%mm3
  add $16, %%eax
  add $16, %%edi
  pfsub %%mm0, %%mm1
  pfsub %%mm2, %%mm3
  pfmul %%mm1, %%mm1
  pfmul %%mm3, %%mm3
  pfadd %%mm1, %%mm4
  pfadd %%mm3, %%mm5

  sub $4,  %%ecx

  jae mul4_loop%=

  pfadd %%mm5,%%mm4

mul4_skip%=:

  add $2, %%ecx
  jae mul2_skip%=

  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  add $8, %%eax
  add $8, %%edi
  pfsub %%mm0, %%mm1
  pfmul %%mm1, %%mm1
  pfadd %%mm1, %%mm4
mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfsub %%mm0, %%mm1
  pfmul %%mm1, %%mm1
  pfadd %%mm1, %%mm4
even%=:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%edi
  pop %%eax
  femms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (sum)
CLOBBER_3DNOW
  );
    
  return sum[0];
}


inline float vec_mahalanobis_3dnow(const float *a, const float *b, const float *c, int len)
{
   float sum[2]={0,0};
   __asm__ __volatile__ (
   "
   push %%eax
   push %%esi
   push %%edi
   push %%ecx
   pxor %%mm4, %%mm4
   pxor %%mm5, %%mm5

   sub $4, %%ecx
   jb mul4_skip%=

mul4_loop%=:
   movq (%%eax), %%mm0
   movq (%%edi), %%mm1
   movq 8(%%eax), %%mm2
   movq 8(%%edi), %%mm3
   movq (%%esi), %%mm6
   movq 8(%%esi), %%mm7
   add $16, %%eax
   add $16, %%edi
   add $16, %%esi
   pfsub %%mm0, %%mm1
   pfsub %%mm2, %%mm3
   pfmul %%mm1, %%mm1
   pfmul %%mm3, %%mm3
   pfmul %%mm6, %%mm1
   pfmul %%mm7, %%mm3
   pfadd %%mm1, %%mm4
   pfadd %%mm3, %%mm5

   sub $4,  %%ecx
   jae mul4_loop%=

mul4_skip%=:

   pfadd %%mm5, %%mm4




  add $2, %%ecx
  jae mul2_skip%=

  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  movq (%%esi), %%mm6
  add $8, %%eax
  add $8, %%edi
  add $8, %%esi
  pfsub %%mm0, %%mm1
  pfmul %%mm1, %%mm1
  pfmul %%mm6, %%mm1
  pfadd %%mm1, %%mm4
mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  pxor %%mm6, %%mm6
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  movd (%%esi), %%mm6
  pfsub %%mm0, %%mm1
  pfmul %%mm1, %%mm1
  pfmul %%mm6, %%mm1
  pfadd %%mm1, %%mm4

even%=:




   pxor %%mm5, %%mm5
   pfacc %%mm5, %%mm4
   movq %%mm4, (%%edx)

   pop %%ecx
   pop %%edi
   pop %%esi
   pop %%eax
   femms
   "
   : : "a" (a), "S" (c), "D" (b), "c" (len), "d" (sum)
   CLOBBER_3DNOW
   );
    
   return sum[0];
}


inline float vec_sum_3dnow(const float *a, int len)
{
  //float sum=0;
  float sum[2]={0,0};
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ecx
  pxor %%mm4, %%mm4
  pxor %%mm5, %%mm5

  sub $4, %%ecx
  jb mul4_skip%=

mul4_loop%=:
  movq (%%eax), %%mm0
  pfadd %%mm0, %%mm4
  movq 8(%%eax), %%mm1
  pfadd %%mm1, %%mm5

  add $16, %%eax
  sub $4,  %%ecx

  jae mul4_loop%=

  pfadd %%mm5,%%mm4

mul4_skip%=:

  add $2, %%ecx
  jae mul2_skip%=

  movq (%%eax), %%mm0
  add $8, %%eax
  pfadd %%mm0, %%mm4
mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  movd (%%eax), %%mm0
  pfadd %%mm0, %%mm4
even%=:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%eax
  femms
  "
  : : "a" (a), "c" (len), "d" (sum)
CLOBBER_3DNOW
  );
    
  return sum[0];
}


inline float vec_norm2_3dnow(const float *a, int len)
{
  //float sum=0;
  float sum[2]={0,0};
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ecx
  pxor %%mm4, %%mm4
  pxor %%mm5, %%mm5

  sub $4, %%ecx
  jb mul4_skip%=

mul4_loop%=:
  movq (%%eax), %%mm0
  pfmul %%mm0, %%mm0
  pfadd %%mm0, %%mm4
  movq 8(%%eax), %%mm1
  pfmul %%mm1, %%mm1
  pfadd %%mm1, %%mm5

  add $16, %%eax
  sub $4,  %%ecx

  jae mul4_loop%=

  pfadd %%mm5,%%mm4

mul4_skip%=:

  add $2, %%ecx
  jae mul2_skip%=

  movq (%%eax), %%mm0
  pfmul %%mm0, %%mm0
  add $8, %%eax
  pfadd %%mm0, %%mm4
mul2_skip%=:

  and $1, %%ecx
  jz even%=

  pxor %%mm0, %%mm0
  movd (%%eax), %%mm0
  pfmul %%mm0, %%mm0
  pfadd %%mm0, %%mm4
even%=:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%eax
  femms
  "
  : : "a" (a), "c" (len), "d" (sum)
CLOBBER_3DNOW
  );
    
  return sum[0];
}



inline void vec_inv_3dnow(const float *a, float *b, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ecx
  push %%edx

  sub $2, %%ecx
  jb mul2_skip%=

mul2_loop%=:
  movd (%%eax), %%mm0
  pfrcp %%mm0, %%mm2
  movd 4(%%eax), %%mm1
  pfrcp %%mm1, %%mm3
  //punpckldq %%mm0, %%mm0
  //punpckldq %%mm1, %%mm1
  pfrcpit1 %%mm2, %%mm0
  pfrcpit1 %%mm3, %%mm1
  pfrcpit2 %%mm2, %%mm0
  pfrcpit2 %%mm3, %%mm1

  movd %%mm0, (%%edx)
  movd %%mm1, 4(%%edx)
  add $8, %%eax
  add $8, %%edx
  sub $2,  %%ecx

  jae mul2_loop%=

mul2_skip%=:

  and $1, %%ecx
  jz even%=

  movd (%%eax), %%mm0
  pfrcp %%mm0, %%mm2
  //punpckldq %%mm0, %%mm0
  pfrcpit1 %%mm2, %%mm0
  pfrcpit2 %%mm2, %%mm0
  movd %%mm0, (%%edx)
even%=:

  pop %%edx
  pop %%ecx
  pop %%eax
  femms
  "
  : : "a" (a), "c" (len), "d" (b)
CLOBBER_3DNOW
  );
}



inline void vec_sqrt_3dnow(const float *a, float *b, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ecx
  push %%edx

  sub $2, %%ecx
  jb mul2_skip%=

mul2_loop%=:
  movd (%%eax), %%mm0
  movd 4(%%eax), %%mm1
  movq %%mm0, %%mm6
  movq %%mm1, %%mm7
  pfrsqrt %%mm0, %%mm2
  pfrsqrt %%mm1, %%mm3
  movq %%mm2, %%mm4
  movq %%mm3, %%mm5
  pfmul %%mm2, %%mm2
  pfmul %%mm3, %%mm3
  pfrsqit1 %%mm2, %%mm0
  pfrsqit1 %%mm3, %%mm1
  pfrcpit2 %%mm4, %%mm0
  pfrcpit2 %%mm5, %%mm1
  pfmul %%mm6, %%mm0
  pfmul %%mm7, %%mm1

  movd %%mm0, (%%edx)
  movd %%mm1, 4(%%edx)
  add $8, %%eax
  add $8, %%edx
  sub $2,  %%ecx

  jae mul2_loop%=

mul2_skip%=:

  and $1, %%ecx
  jz even%=

  movd (%%eax), %%mm0
  movq %%mm0, %%mm6
  pfrsqrt %%mm0, %%mm2
  movq %%mm2, %%mm4
  pfmul %%mm2, %%mm2
  pfrsqit1 %%mm2, %%mm0
  pfrcpit2 %%mm4, %%mm0
  pfmul %%mm6, %%mm0

  movd %%mm0, (%%edx)
even%=:

  pop %%edx
  pop %%ecx
  pop %%eax
  femms
  "
  : : "a" (a), "c" (len), "d" (b)
CLOBBER_3DNOW
  );
}

#else /* _ENABLE_3DNOW */


#include "BaseException.h"

#define ERROR_3DNOW_NI {throw new GeneralException("Trying to use 3DNow!, but Overflow not compiled with _ENABLE_3DNOW. Bad, bad, this should never happen", __FILE__, __LINE__);}

inline float vec_inner_prod_3dnow(const float *a, const float *b, int len)
ERROR_3DNOW_NI

inline void vec_add_vec_3dnow(const float *a, const float *b, float *c, int len)
ERROR_3DNOW_NI

inline void vec_sub_vec_3dnow(const float *a, const float *b, float *c, int len)
ERROR_3DNOW_NI

inline void vec_mul_vec_3dnow(const float *a, const float *b, float *c, int len)
ERROR_3DNOW_NI

inline void vec_add_scal_3dnow(const float a, const float *b, float *c, int len)
ERROR_3DNOW_NI

inline void vec_mul_scal_3dnow(const float a, const float *b, float *c, int len)
ERROR_3DNOW_NI

inline float vec_dist_3dnow(const float *a, const float *b, int len)
ERROR_3DNOW_NI

inline float vec_mahalanobis_3dnow(const float *a, const float *b, const float *c, int len)
ERROR_3DNOW_NI

inline float vec_sum_3dnow(const float *a, int len)
ERROR_3DNOW_NI

inline float vec_norm2_3dnow(const float *a, int len)
ERROR_3DNOW_NI

inline void vec_inv_3dnow(const float *a, float *b, int len)
ERROR_3DNOW_NI

inline void vec_sqrt_3dnow(const float *a, float *b, int len)
ERROR_3DNOW_NI

#endif /* !_ENABLE_3DNOW */



#endif
