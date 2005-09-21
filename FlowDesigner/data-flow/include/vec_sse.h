// Copyright (C) 2001 Jean-Marc Valin
//This file contains SSE-optimized vector primitives


#ifndef VEC_SSE_H
#define VEC_SSE_H

#include "BaseException.h"

namespace FD {

#ifdef _ENABLE_SSE

#define CLOBBER_SSE : "memory"


//template <>
inline void vec_mul_and_add_sse(const float a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (
  
  "\tpush %0\n"
  "\tpush %1\n"
  "\tpush %2\n"
  "\tpush %3\n"

  "\tmovss (%0), %%xmm0\n"
  "\tshufps $0, %%xmm0, %%xmm0\n"

  "\tsub $8, %2\n"
  "\tjb mul8_skip%=\n"

"mul8_loop%=:\n"
  "\tmovups (%1), %%xmm1\n"
  "\tmovups 16(%1), %%xmm2\n"
  "\tmulps %%xmm0, %%xmm1\n"
  "\tmulps %%xmm0, %%xmm2\n"
  
  "\tmovups (%3), %%xmm3\n"
  "\tmovups 16(%3), %%xmm4\n"
  "\taddps %%xmm1, %%xmm3\n"
  "\taddps %%xmm2, %%xmm4\n"
  "\tmovups %%xmm3, (%3)\n"
  "\tmovups %%xmm4, 16(%3)\n"

  "\tadd $32, %1\n"
  "\tadd $32, %3\n"
  "\tsub $8,  %2\n"

  "\tjae mul8_loop%=\n"

"mul8_skip%=:\n"

  "\tadd $4, %2\n"
  "\tjl mul4_skip%=\n"

  "\tmovups (%1), %%xmm1\n"
  "\tmulps %%xmm0, %%xmm1\n"
  "\tmovups (%3), %%xmm3\n"
  "\taddps %%xmm1, %%xmm3\n"
  "\tmovups %%xmm3, (%3)\n"

  "\tadd $16, %1\n"
  "\tadd $16, %3\n"

  "\tsub $4,  %2\n"

"mul4_skip%=:\n"


  "\tadd $4, %2\n"

  "\tjmp cond1%=\n"

"mul1_loop%=:\n"

  "\tmovss (%1), %%xmm1\n"
  "\tmulss %%xmm0, %%xmm1\n"
  "\tmovss (%3), %%xmm3\n"
  "\taddss %%xmm1, %%xmm3\n"
  "\tmovss %%xmm3, (%3)\n"
  "\tadd $4, %1\n"
  "\tadd $4, %3\n"

"cond1%=:\n"
  "\tsub $1, %2\n"
  "\tjae mul1_loop%=\n"

  "\tpop %3\n"
  "\tpop %2\n"
  "\tpop %1\n"
  "\tpop %0\n"
  
  : : "r" (&a), "r" (b), "q" (len), "r" (c)
  CLOBBER_SSE
  );
}

inline float vec_inner_prod_sse(const float *a, const float *b, int len)
{
  float sum;
  __asm__ __volatile__ (
  
  "\tpush %%eax\n"
  "\tpush %%edi\n"
  "\tpush %%ecx\n"
  "\txorps %%xmm3, %%xmm3\n"
  "\txorps %%xmm4, %%xmm4\n"

  "\tsub $8, %%ecx\n"
  "\tjb mul8_skip%=\n"

"mul8_loop%=:\n"
  "\tmovups (%%eax), %%xmm0\n"
  "\tmovups (%%edi), %%xmm1\n"
  "\tmovups 16(%%eax), %%xmm5\n"
  "\tmovups 16(%%edi), %%xmm6\n"
  "\tadd $32, %%eax\n"
  "\tadd $32, %%edi\n"
  "\tmulps %%xmm0, %%xmm1\n"
  "\tmulps %%xmm5, %%xmm6\n"
  "\taddps %%xmm1, %%xmm3\n"
  "\taddps %%xmm6, %%xmm4\n"

  "\tsub $8,  %%ecx\n"

  "\tjae mul8_loop%=\n"

"mul8_skip%=:\n"

  "\taddps %%xmm4, %%xmm3\n"

  "\tadd $4, %%ecx\n"
  "\tjl mul4_skip%=\n"

  "\tmovups (%%eax), %%xmm0\n"
  "\tmovups (%%edi), %%xmm1\n"
  "\tadd $16, %%eax\n"
  "\tadd $16, %%edi\n"
  "\tmulps %%xmm0, %%xmm1\n"
  "\taddps %%xmm1, %%xmm3\n"

  "\tsub $4,  %%ecx\n"

"mul4_skip%=:\n"


  "\tadd $4, %%ecx\n"

  "\tjmp cond1%=\n"

"mul1_loop%=:\n"
  "\tmovss (%%eax), %%xmm0\n"
  "\tmovss (%%edi), %%xmm1\n"
  "\tadd $4, %%eax\n"
  "\tadd $4, %%edi\n"
  "\tmulss %%xmm0, %%xmm1\n"
  "\taddss %%xmm1, %%xmm3\n"

"cond1%=:\n"
  "\tsub $1, %%ecx\n"
  "\tjae mul1_loop%=\n"

  "\tmovhlps %%xmm3, %%xmm4\n"
  "\taddps %%xmm4, %%xmm3\n"
  "\tmovaps %%xmm3, %%xmm4\n"
  //FIXME: which one?
  "\tshufps $0x55, %%xmm4, %%xmm4\n"
  //shufps $33, %%xmm4, %%xmm4
  "\taddss %%xmm4, %%xmm3\n"
  "\tmovss %%xmm3, (%%edx)\n"
  
  "\tpop %%ecx\n"
  "\tpop %%edi\n"
  "\tpop %%eax\n"
  "\temms\n"
  
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
   
   "\tpush %%eax\n"
   "\tpush %%esi\n"
   "\tpush %%edi\n"
   "\tpush %%ecx\n"
   "\txorps %%xmm4, %%xmm4\n"
   "\txorps %%xmm5, %%xmm5\n"

   "\tsub $8, %%ecx\n"
   "\tjb mul8_skip%=\n"

"mul8_loop%=:\n"
   "\tmovups (%%eax), %%xmm0\n"
   "\tmovups (%%edi), %%xmm1\n"
   "m\tovups 16(%%eax), %%xmm2\n"
   "\tmovups 16(%%edi), %%xmm3\n"
   "\tmovups (%%esi), %%xmm6\n"
   "\tmovups 16(%%esi), %%xmm7\n"
   "\tadd $32, %%eax\n"
   "\tadd $32, %%edi\n"
   "\tadd $32, %%esi\n"
   "\tsubps %%xmm0, %%xmm1\n"
   "\tsubps %%xmm2, %%xmm3\n"
   "\tmulps %%xmm1, %%xmm1\n"
   "\tmulps %%xmm3, %%xmm3\n"
   "\tmulps %%xmm6, %%xmm1\n"
   "\tmulps %%xmm7, %%xmm3\n"
   "\taddps %%xmm1, %%xmm4\n"
   "\taddps %%xmm3, %%xmm5\n"

   "\tsub $8,  %%ecx\n"
   "\tjae mul8_loop%=\n"

"mul8_skip%=:\n"
   "\taddps %%xmm5, %%xmm4\n"


   "\tadd $4, %%ecx\n"
   "\tjl mul4_skip%=\n"

   "\tmovups (%%eax), %%xmm0\n"
   "\tmovups (%%edi), %%xmm1\n"
   "\tmovups (%%esi), %%xmm6\n"
   "\tadd $16, %%eax\n"
   "\tadd $16, %%edi\n"
   "\tadd $16, %%esi\n"

   "\tsubps %%xmm0, %%xmm1\n"
   "\tmulps %%xmm1, %%xmm1\n"
   "\tmulps %%xmm6, %%xmm1\n"
   "\taddps %%xmm1, %%xmm4\n"

   "\tsub $4,  %%ecx\n"

"mul4_skip%=:\n"



   "\tmovaps %%xmm4, %%xmm3\n"


   "\tmovhlps %%xmm3, %%xmm4\n"
   "\taddps %%xmm4, %%xmm3\n"
   "\tmovaps %%xmm3, %%xmm4\n"
   "\tshufps $33, %%xmm4, %%xmm4\n"
   "\taddss %%xmm4, %%xmm3\n"
   "\tmovss %%xmm3, (%%edx)\n"


   "\tpop %%ecx\n"
   "\tpop %%edi\n"
   "\tpop %%esi\n"
   "\tpop %%eax\n"
   "\temms\n"
   
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
   
   "\tpush %%eax\n"
   "\tpush %%edi\n"
   "\tpush %%ecx\n"
   "\txorps %%xmm4, %%xmm4\n"
   "\txorps %%xmm5, %%xmm5\n"

   "\tsub $8, %%ecx\n"
   "j\tb mul8_skip%=\n"

"mul8_loop%=:\n"
   "\tmovups (%%eax), %%xmm0\n"
   "\tmovups (%%edi), %%xmm1\n"
   "\tmovups 16(%%eax), %%xmm2\n"
   "\tmovups 16(%%edi), %%xmm3\n"
   "\tadd $32, %%eax\n"
   "\tadd $32, %%edi\n"
   "\tsubps %%xmm0, %%xmm1\n"
   "\tsubps %%xmm2, %%xmm3\n"
   "\tmulps %%xmm1, %%xmm1\n"
   "\tmulps %%xmm3, %%xmm3\n"
   "\taddps %%xmm1, %%xmm4\n"
   "\taddps %%xmm3, %%xmm5\n"

   "\tsub $8,  %%ecx\n"
   "\tjae mul8_loop%=\n"

"mul8_skip%=:\n"
   "\taddps %%xmm5, %%xmm4\n"


   "\tadd $4, %%ecx\n"
   "\tjl mul4_skip%=\n"

   "\tmovups (%%eax), %%xmm0\n"
   "\tmovups (%%edi), %%xmm1\n"
   "\tadd $16, %%eax\n"
   "\tadd $16, %%edi\n"

   "\tsubps %%xmm0, %%xmm1\n"
   "\tmulps %%xmm1, %%xmm1\n"
   "\taddps %%xmm1, %%xmm4\n"

   "\tsub $4,  %%ecx\n"

"mul4_skip%=:\n"



   "\tmovaps %%xmm4, %%xmm3\n"


   "\tmovhlps %%xmm3, %%xmm4\n"
   "\taddps %%xmm4, %%xmm3\n"
   "\tmovaps %%xmm3, %%xmm4\n"
   "\tshufps $33, %%xmm4, %%xmm4\n"
   "\taddss %%xmm4, %%xmm3\n"
   "\tmovss %%xmm3, (%%edx)\n"


   "\tpop %%ecx\n"
   "\tpop %%edi\n"
   "\tpop %%eax\n"
   "\temms\n"
   
   : : "a" (a), "D" (b), "c" (len), "d" (&sum)
   CLOBBER_SSE
   );
    
   return sum;
}


#else /* _ENABLE_SSE */


#define ERROR_SSE_NI {throw new GeneralException("Trying to use SSE, but Overflow not compiled with _ENABLE_SSE. Bad, bad, this should never happen", __FILE__, __LINE__);}

inline float vec_inner_prod_sse(const float *a, const float *b, int len)
  ERROR_SSE_NI

inline float vec_mahalanobis2_mul4_sse(const float *a, const float *b, const float *c, int len)
  ERROR_SSE_NI

inline float vec_dist2_mul4_sse(const float *a, const float *b, int len)
  ERROR_SSE_NI

#endif /* !_ENABLE_SSE */

}//namespace FD

#endif
