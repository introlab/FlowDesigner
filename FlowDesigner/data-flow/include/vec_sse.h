// Copyright (C) 2001 Jean-Marc Valin
//This file contains SSE-optimized vector primitives


#ifndef VEC_SSE_H
#define VEC_SSE_H


#ifdef _ENABLE_SSE


#define CLOBBER_SSE : "memory"


//template <>
inline void vec_mul_and_add_sse(const float a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %0
  push %1
  push %2
  push %3

  movss (%0), %%xmm0
  shufps $0, %%xmm0, %%xmm0

  sub $8, %2
  jb mul8_skip%=

mul8_loop%=:
  movups (%1), %%xmm1
  movups 16(%1), %%xmm2
  mulps %%xmm0, %%xmm1
  mulps %%xmm0, %%xmm2
  
  movups (%3), %%xmm3
  movups 16(%3), %%xmm4
  addps %%xmm1, %%xmm3
  addps %%xmm2, %%xmm4
  movups %%xmm3, (%3)
  movups %%xmm4, 16(%3)

  add $32, %1
  add $32, %3
  sub $8,  %2

  jae mul8_loop%=

mul8_skip%=:

  add $4, %2
  jl mul4_skip%=

  movups (%1), %%xmm1
  mulps %%xmm0, %%xmm1
  movups (%3), %%xmm3
  addps %%xmm1, %%xmm3
  movups %%xmm3, (%3)

  add $16, %1
  add $16, %3

  sub $4,  %2

mul4_skip%=:


  add $4, %2

  jmp cond1%=

mul1_loop%=:

  movss (%1), %%xmm1
  mulss %%xmm0, %%xmm1
  movss (%3), %%xmm3
  addss %%xmm1, %%xmm3
  movss %%xmm3, (%3)
  add $4, %1
  add $4, %3

cond1%=:
  sub $1, %2
  jae mul1_loop%=

  pop %3
  pop %2
  pop %1
  pop %0
  "
  : : "r" (&a), "r" (b), "q" (len), "r" (c)
  CLOBBER_SSE
  );
}

inline float vec_inner_prod_sse(const float *a, const float *b, int len)
{
  float sum;
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  xorps %%xmm3, %%xmm3
  xorps %%xmm4, %%xmm4

  sub $8, %%ecx
  jb mul8_skip%=

mul8_loop%=:
  movups (%%eax), %%xmm0
  movups (%%edi), %%xmm1
  movups 16(%%eax), %%xmm5
  movups 16(%%edi), %%xmm6
  add $32, %%eax
  add $32, %%edi
  mulps %%xmm0, %%xmm1
  mulps %%xmm5, %%xmm6
  addps %%xmm1, %%xmm3
  addps %%xmm6, %%xmm4

  sub $8,  %%ecx

  jae mul8_loop%=

mul8_skip%=:

  addps %%xmm4, %%xmm3

  add $4, %%ecx
  jl mul4_skip%=

  movups (%%eax), %%xmm0
  movups (%%edi), %%xmm1
  add $16, %%eax
  add $16, %%edi
  mulps %%xmm0, %%xmm1
  addps %%xmm1, %%xmm3

  sub $4,  %%ecx

mul4_skip%=:


  add $4, %%ecx

  jmp cond1%=

mul1_loop%=:
  movss (%%eax), %%xmm0
  movss (%%edi), %%xmm1
  add $4, %%eax
  add $4, %%edi
  mulss %%xmm0, %%xmm1
  addss %%xmm1, %%xmm3

cond1%=:
  sub $1, %%ecx
  jae mul1_loop%=

  movhlps %%xmm3, %%xmm4
  addps %%xmm4, %%xmm3
  movaps %%xmm3, %%xmm4
  //FIXME: which one?
  shufps $0x55, %%xmm4, %%xmm4
  //shufps $33, %%xmm4, %%xmm4
  addss %%xmm4, %%xmm3
  movss %%xmm3, (%%edx)
  
  pop %%ecx
  pop %%edi
  pop %%eax
  emms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (&sum)
CLOBBER_SSE
  );
  return sum;
}

//WARNING:
//FIXME: Does not work yet with lengths that are not a multiple of 4
inline float vec_mahalanobis2_mul4_sse(const float *a, const float *b, const float *c, int len)
{
   float sum=0;
   __asm__ __volatile__ (
   "
   push %%eax
   push %%esi
   push %%edi
   push %%ecx
   xorps %%xmm4, %%xmm4
   xorps %%xmm5, %%xmm5

   sub $8, %%ecx
   jb mul8_skip%=

mul8_loop%=:
   movups (%%eax), %%xmm0
   movups (%%edi), %%xmm1
   movups 16(%%eax), %%xmm2
   movups 16(%%edi), %%xmm3
   movups (%%esi), %%xmm6
   movups 16(%%esi), %%xmm7
   add $32, %%eax
   add $32, %%edi
   add $32, %%esi
   subps %%xmm0, %%xmm1
   subps %%xmm2, %%xmm3
   mulps %%xmm1, %%xmm1
   mulps %%xmm3, %%xmm3
   mulps %%xmm6, %%xmm1
   mulps %%xmm7, %%xmm3
   addps %%xmm1, %%xmm4
   addps %%xmm3, %%xmm5

   sub $8,  %%ecx
   jae mul8_loop%=

mul8_skip%=:
   addps %%xmm5, %%xmm4


   add $4, %%ecx
   jl mul4_skip%=

   movups (%%eax), %%xmm0
   movups (%%edi), %%xmm1
   movups (%%esi), %%xmm6
   add $16, %%eax
   add $16, %%edi
   add $16, %%esi

   subps %%xmm0, %%xmm1
   mulps %%xmm1, %%xmm1
   mulps %%xmm6, %%xmm1
   addps %%xmm1, %%xmm4

   sub $4,  %%ecx

mul4_skip%=:



   movaps %%xmm4, %%xmm3


   movhlps %%xmm3, %%xmm4
   addps %%xmm4, %%xmm3
   movaps %%xmm3, %%xmm4
   shufps $33, %%xmm4, %%xmm4
   addss %%xmm4, %%xmm3
   movss %%xmm3, (%%edx)


   pop %%ecx
   pop %%edi
   pop %%esi
   pop %%eax
   emms
   "
   : : "a" (a), "S" (c), "D" (b), "c" (len), "d" (&sum)
   CLOBBER_SSE
   );
    
   return sum;
}


//WARNING:
//FIXME: Does not work yet with lengths that are not a multiple of 4
inline float vec_dist2_mul4_sse(const float *a, const float *b, int len)
{
   float sum=0;
   __asm__ __volatile__ (
   "
   push %%eax
   push %%edi
   push %%ecx
   xorps %%xmm4, %%xmm4
   xorps %%xmm5, %%xmm5

   sub $8, %%ecx
   jb mul8_skip%=

mul8_loop%=:
   movups (%%eax), %%xmm0
   movups (%%edi), %%xmm1
   movups 16(%%eax), %%xmm2
   movups 16(%%edi), %%xmm3
   add $32, %%eax
   add $32, %%edi
   subps %%xmm0, %%xmm1
   subps %%xmm2, %%xmm3
   mulps %%xmm1, %%xmm1
   mulps %%xmm3, %%xmm3
   addps %%xmm1, %%xmm4
   addps %%xmm3, %%xmm5

   sub $8,  %%ecx
   jae mul8_loop%=

mul8_skip%=:
   addps %%xmm5, %%xmm4


   add $4, %%ecx
   jl mul4_skip%=

   movups (%%eax), %%xmm0
   movups (%%edi), %%xmm1
   add $16, %%eax
   add $16, %%edi

   subps %%xmm0, %%xmm1
   mulps %%xmm1, %%xmm1
   addps %%xmm1, %%xmm4

   sub $4,  %%ecx

mul4_skip%=:



   movaps %%xmm4, %%xmm3


   movhlps %%xmm3, %%xmm4
   addps %%xmm4, %%xmm3
   movaps %%xmm3, %%xmm4
   shufps $33, %%xmm4, %%xmm4
   addss %%xmm4, %%xmm3
   movss %%xmm3, (%%edx)


   pop %%ecx
   pop %%edi
   pop %%eax
   emms
   "
   : : "a" (a), "D" (b), "c" (len), "d" (&sum)
   CLOBBER_SSE
   );
    
   return sum;
}


#else /* _ENABLE_SSE */


#include "BaseException.h"

#define ERROR_SSE_NI {throw new GeneralException("Trying to use SSE, but Overflow not compiled with _ENABLE_SSE. Bad, bad, this should never happen", __FILE__, __LINE__);}

inline float vec_inner_prod_sse(const float *a, const float *b, int len)
ERROR_SSE_NI

inline float vec_mahalanobis2_mul4_sse(const float *a, const float *b, const float *c, int len)
ERROR_SSE_NI

inline float vec_dist2_mul4_sse(const float *a, const float *b, int len)
ERROR_SSE_NI

#endif /* !_ENABLE_SSE */


#endif
