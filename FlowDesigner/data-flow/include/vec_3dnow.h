// Copyright (C) 2001 Jean-Marc Valin
//This file contains 3DNow!-optimized vector primitives


#ifndef VEC_3DNOW_H
#define VEC_3DNOW_H

#include "BaseException.h"

namespace FD {

#ifdef _ENABLE_3DNOW



#define CLOBBER_3DNOW   : "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)", "memory"

inline float vec_inner_prod_3dnow(const float *a, const float *b, int len)
{
  float sum[2]={0,0};
  __asm__ __volatile__ (

  "\tpush %%eax \n"
  "\tpush %%edi \n"
  "\tpush %%ecx \n"
  "\tpxor %%mm4, %%mm4 \n"
  "\tpxor %%mm5, %%mm5 \n"
  "\tsub $4, %%ecx \n"
  "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tmovq 8(%%eax), %%mm2 \n"
  "\tmovq 8(%%edi), %%mm3 \n"
  "\tadd $16, %%eax \n"
  "\tadd $16, %%edi \n"
  "\tpfmul %%mm0, %%mm1 \n"
  "\tpfmul %%mm2, %%mm3 \n"
  "\tpfadd %%mm1, %%mm4 \n"
  "\tpfadd %%mm3, %%mm5 \n"
  "\tsub $4,  %%ecx \n"
  "\tjae mul4_loop%= \n"
  "\tpfadd %%mm5,%%mm4 \n"

"mul4_skip%=: \n"
  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"
  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tadd $8, %%eax \n"
  "\tadd $8, %%edi \n"
  "\tpfmul %%mm0, %%mm1 \n"
  "\tpfadd %%mm1, %%mm4 \n"

"mul2_skip%=: \n"
  "\tand $1, %%ecx \n"
  "\tjz even%= \n"
  "\tpxor %%mm0, %%mm0 \n"
  "\tpxor %%mm1, %%mm1 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tmovd (%%edi), %%mm1 \n"
  "\tpfmul %%mm0, %%mm1 \n"
  "\tpfadd %%mm1, %%mm4 \n"

"even%=: \n"
  "\tpxor %%mm5, %%mm5 \n"
  "\tpfacc %%mm5, %%mm4 \n"
  "\tmovq %%mm4, (%%edx) \n"
  "\tpop %%ecx \n"
  "\tpop %%edi \n"
  "\tpop %%eax \n"
  "\tfemms \n"
  : : "a" (a), "D" (b), "c" (len), "d" (sum)
CLOBBER_3DNOW
  );
    
  return sum[0];
}



inline void vec_add_vec_3dnow(const float *a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (

  "\tpush %%eax \n"
  "\tpush %%edi \n"
  "\tpush %%ecx \n"
  "\tpush %%edx \n"

  "\tsub $4, %%ecx \n"
  "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfadd %%mm0, %%mm1 \n"
  "\tmovq 8(%%eax), %%mm2 \n"
  "\tmovq 8(%%edi), %%mm3 \n"
  "\tpfadd %%mm2, %%mm3 \n"

  "\tmovq %%mm1, (%%edx) \n"
  "\tmovq %%mm3, 8(%%edx) \n"
  "\tadd $16, %%eax \n"
  "\tadd $16, %%edi \n"
  "\tadd $16, %%edx \n"
  "\tsub $4,  %%ecx \n"

  "\tjae mul4_loop%= \n"

"mul4_skip%=: \n"

  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"
  
  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfadd %%mm0, %%mm1 \n"
  "\tmovq %%mm1, (%%edx) \n"
  "\tadd $8, %%eax \n"
  "\tadd $8, %%edi \n"
  "\tadd $8, %%edx \n"

"mul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"
 
  "\tpxor %%mm0, %%mm0 \n"
  "\tpxor %%mm1, %%mm1 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tmovd (%%edi), %%mm1 \n"
  "\tpfadd %%mm0, %%mm1 \n"
  "\tmovd %%mm1, (%%edx) \n" 
"even%=: \n"

  "\tpop %%edx \n"
  "\tpop %%ecx \n"
  "\tpop %%edi \n"
  "\tpop %%eax \n"
  "\tfemms \n"
  : : "a" (a), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline void vec_sub_vec_3dnow(const float *a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (
  
  "\tpush %%eax \n"
  "\tpush %%edi \n"
  "\tpush %%ecx \n"
  "\tpush %%edx \n"

  "\tsub $4, %%ecx \n"
  "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfsubr %%mm0, %%mm1 \n" 
  "\tmovq 8(%%eax), %%mm2 \n"
  "\tmovq 8(%%edi), %%mm3 \n"
  "\tpfsubr %%mm2, %%mm3 \n"

  "\tmovq %%mm1, (%%edx) \n"
  "\tmovq %%mm3, 8(%%edx) \n"
  "\tadd $16, %%eax \n"
  "\tadd $16, %%edi \n"
  "\tadd $16, %%edx \n"
  "\tsub $4,  %%ecx \n"

  "\tjae mul4_loop%= \n"

"mul4_skip%=: \n"

  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"

  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfsubr %%mm0, %%mm1 \n"
  "\tmovq %%mm1, (%%edx) \n"
  "\tadd $8, %%eax \n"
  "\tadd $8, %%edi \n"
  "\tadd $8, %%edx \n"

"\tmul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tpxor %%mm0, %%mm0 \n"
  "\tpxor %%mm1, %%mm1 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tmovd (%%edi), %%mm1 \n"
  "\tpfsubr %%mm0, %%mm1 \n"
  "\tmovd %%mm1, (%%edx) \n"
"even%=: \n"

  "\tpop %%edx \n"
  "\tpop %%ecx \n"
  "\tpop %%edi \n"
  "\tpop %%eax \n"
  "\tfemms \n"
  : : "a" (a), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline void vec_mul_vec_3dnow(const float *a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (

  "\tpush %%eax \n"
  "\tpush %%edi \n"
  "\tpush %%ecx \n"
  "\tpush %%edx \n"

  "\tsub $4, %%ecx \n"
  "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfmul %%mm0, %%mm1 \n"
  "\tmovq 8(%%eax), %%mm2 \n"
  "\tmovq 8(%%edi), %%mm3 \n"
  "\tpfmul %%mm2, %%mm3 \n"

  "\tmovq %%mm1, (%%edx) \n"
  "\tmovq %%mm3, 8(%%edx) \n"
  "\tadd $16, %%eax \n"
  "\tadd $16, %%edi \n"
  "\tadd $16, %%edx \n"
  "\tsub $4,  %%ecx \n"

  "\tjae mul4_loop%= \n"

"mul4_skip%=: \n"

  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"

  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfmul %%mm0, %%mm1 \n"
  "\tmovq %%mm1, (%%edx) \n"
  "\tadd $8, %%eax \n"
  "\tadd $8, %%edi \n"
  "\tadd $8, %%edx \n"

"\tmul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tpxor %%mm0, %%mm0 \n"
  "\tpxor %%mm1, %%mm1 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tmovd (%%edi), %%mm1 \n"
  "\tpfmul %%mm0, %%mm1 \n"
  "\tmovd %%mm1, (%%edx) \n"
"even%=: \n"

  "\tpop %%edx \n"
  "\tpop %%ecx \n"
  "\tpop %%edi \n"
  "\tpop %%eax \n"
  "\tfemms \n"
  : : "a" (a), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline void vec_add_scal_3dnow(const float a, const float *b, float *c, int len)
{
  float tmp[2];
  tmp[0]=tmp[1]=a;
  __asm__ __volatile__ (
  
  "\tpush %%edi \n"
  "\tpush %%ecx \n"
  "\tpush %%edx \n"
  "\tmovq (%%eax), %%mm0 \n"

  "\tsub $4, %%ecx \n"
  "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfadd %%mm0, %%mm1 \n"
  "\tmovq 8(%%edi), %%mm2 \n"
  "\tpfadd %%mm0, %%mm2 \n"

  "\tmovq %%mm1, (%%edx) \n"
  "\tmovq %%mm2, 8(%%edx) \n"
  "\tadd $16, %%edi \n"
  "\tadd $16, %%edx \n"
  "\tsub $4,  %%ecx \n"

  "\tjae mul4_loop%= \n"

"mul4_skip%=: \n"

  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"
  
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfadd %%mm0, %%mm1 \n"
  "\tmovq %%mm1, (%%edx) \n"
  "\tadd $8, %%edi \n"
  "\tadd $8, %%edx \n"

"mul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tpxor %%mm0, %%mm0 \n"
  "\tpxor %%mm1, %%mm1 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tmovd (%%edi), %%mm1 \n"
  "\tpfadd %%mm0, %%mm1 \n"
  "\tmovd %%mm1, (%%edx) \n"
"even%=: \n"

  "\tpop %%edx \n"
  "\tpop %%ecx \n"
  "\tpop %%edi \n"
  "\tfemms \n"
  
  : : "a" (tmp), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline void vec_mul_scal_3dnow(const float a, const float *b, float *c, int len)
{
  float tmp[2];
  tmp[0]=tmp[1]=a;
  __asm__ __volatile__ (
  
  "\tpush %%edi \n"
  "\tpush %%ecx \n"
  "\tpush %%edx \n"
  "\tmovq (%%eax), %%mm0 \n"

  "\tsub $4, %%ecx \n"
  "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfmul %%mm0, %%mm1 \n"
  "\tmovq 8(%%edi), %%mm2 \n"
  "\tpfmul %%mm0, %%mm2 \n"

  "\tmovq %%mm1, (%%edx) \n"
  "\tmovq %%mm2, 8(%%edx) \n"
  "\tadd $16, %%edi \n"
  "\tadd $16, %%edx \n"
  "\tsub $4,  %%ecx \n"

  "\tjae mul4_loop%= \n"

"mul4_skip%=: \n"

  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"
  
  "\tmovq (%%edi), %%mm1 \n"
  "\tpfmul %%mm0, %%mm1 \n"
  "\tmovq %%mm1, (%%edx) \n"
  "\tadd $8, %%edi \n"
  "\tadd $8, %%edx \n"

"mul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tpxor %%mm0, %%mm0 \n"
  "\tpxor %%mm1, %%mm1 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tmovd (%%edi), %%mm1 \n"
  "\tpfmul %%mm0, %%mm1 \n"
  "\tmovd %%mm1, (%%edx) \n"
"even%=: \n"

  "\tpop %%edx \n"
  "\tpop %%ecx \n"
  "\tpop %%edi \n"
  "\tfemms \n"
 
  : : "a" (tmp), "D" (b), "c" (len), "d" (c)
CLOBBER_3DNOW
  );
}



inline float vec_dist_3dnow(const float *a, const float *b, int len)
{
  //float sum=0;
  float sum[2]={0,0};
  __asm__ __volatile__ (
  
  "\tpush %%eax \n"
  "\tpush %%edi \n"
  "\tpush %%ecx \n"
  "\tpxor %%mm4, %%mm4 \n"
  "\tpxor %%mm5, %%mm5 \n"

  "\tsub $4, %%ecx \n"
  "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tmovq 8(%%eax), %%mm2 \n"
  "\tmovq 8(%%edi), %%mm3 \n"
  "\tadd $16, %%eax \n"
  "\tadd $16, %%edi \n"
  "\tpfsub %%mm0, %%mm1 \n"
  "\tpfsub %%mm2, %%mm3 \n"
  "\tpfmul %%mm1, %%mm1 \n"
  "\tpfmul %%mm3, %%mm3 \n"
  "\tpfadd %%mm1, %%mm4 \n"
  "\tpfadd %%mm3, %%mm5 \n"

  "\tsub $4,  %%ecx \n"

  "\tjae mul4_loop%= \n"

  "\tpfadd %%mm5,%%mm4 \n"

"mul4_skip%=: \n"

  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"

  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tadd $8, %%eax \n"
  "\tadd $8, %%edi \n"
  "\tpfsub %%mm0, %%mm1 \n"
  "\tpfmul %%mm1, %%mm1 \n"
  "\tpfadd %%mm1, %%mm4 \n"
"mul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tpxor %%mm0, %%mm0 \n"
  "\tpxor %%mm1, %%mm1 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tmovd (%%edi), %%mm1 \n"
  "\tpfsub %%mm0, %%mm1 \n"
  "\tpfmul %%mm1, %%mm1 \n"
  "\tpfadd %%mm1, %%mm4 \n"
"even%=: \n"

  "\tpxor %%mm5, %%mm5 \n"
  "\tpfacc %%mm5, %%mm4 \n"
  "\tmovq %%mm4, (%%edx) \n"

  "\tpop %%ecx \n"
  "\tpop %%edi \n"
  "\tpop %%eax \n"
  "\tfemms \n"
  
  : : "a" (a), "D" (b), "c" (len), "d" (sum)
CLOBBER_3DNOW
  );
    
  return sum[0];
}


inline float vec_mahalanobis_3dnow(const float *a, const float *b, const float *c, int len)
{
   float sum[2]={0,0};
   __asm__ __volatile__ (
   
   "\tpush %%eax \n"
   "\tpush %%esi \n"
   "\tpush %%edi \n"
   "\tpush %%ecx \n"
   "\tpxor %%mm4, %%mm4 \n"
   "\tpxor %%mm5, %%mm5 \n"

   "\tsub $4, %%ecx \n"
   "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
   "\tmovq (%%eax), %%mm0 \n"
   "\tmovq (%%edi), %%mm1 \n"
   "\tmovq 8(%%eax), %%mm2 \n"
   "\tmovq 8(%%edi), %%mm3 \n"
   "\tmovq (%%esi), %%mm6 \n"
   "\tmovq 8(%%esi), %%mm7 \n"
   "\tadd $16, %%eax \n"
   "\tadd $16, %%edi \n"
   "\tadd $16, %%esi \n"
   "\tpfsub %%mm0, %%mm1 \n"
   "\tpfsub %%mm2, %%mm3 \n"
   "\tpfmul %%mm1, %%mm1 \n"
   "\tpfmul %%mm3, %%mm3 \n"
   "\tpfmul %%mm6, %%mm1 \n"
   "\tpfmul %%mm7, %%mm3 \n"
   "\tpfadd %%mm1, %%mm4 \n"
   "\tpfadd %%mm3, %%mm5 \n"

   "\tsub $4,  %%ecx \n"
   "\tjae mul4_loop%= \n"

"mul4_skip%=: \n"

   "\tpfadd %%mm5, %%mm4 \n"




  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"

  "\tmovq (%%eax), %%mm0 \n"
  "\tmovq (%%edi), %%mm1 \n"
  "\tmovq (%%esi), %%mm6 \n"
  "\tadd $8, %%eax \n"
  "\tadd $8, %%edi \n"
  "\tadd $8, %%esi \n"
  "\tpfsub %%mm0, %%mm1 \n"
  "\tpfmul %%mm1, %%mm1 \n"
  "\tpfmul %%mm6, %%mm1 \n"
  "\tpfadd %%mm1, %%mm4 \n"
"mul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tpxor %%mm0, %%mm0 \n"
  "\tpxor %%mm1, %%mm1 \n"
  "\tpxor %%mm6, %%mm6 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tmovd (%%edi), %%mm1 \n"
  "\tmovd (%%esi), %%mm6 \n"
  "\tpfsub %%mm0, %%mm1 \n"
  "\tpfmul %%mm1, %%mm1 \n"
  "\tpfmul %%mm6, %%mm1 \n"
  "\tpfadd %%mm1, %%mm4 \n"

"even%=: \n"




   "\tpxor %%mm5, %%mm5 \n"
   "\tpfacc %%mm5, %%mm4 \n"
   "\tmovq %%mm4, (%%edx) \n"

   "\t pop %%ecx \n"
   "\tpop %%edi \n"
   "\tpop %%esi \n"
   "\tpop %%eax \n"
   "\tfemms \n"
   
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
  
  "\tpush %%eax \n"
  "\tpush %%ecx \n"
  "\tpxor %%mm4, %%mm4 \n"
  "\tpxor %%mm5, %%mm5 \n"

  "\tsub $4, %%ecx \n"
  "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
  "\tmovq (%%eax), %%mm0 \n"
  "\tpfadd %%mm0, %%mm4 \n"
  "\tmovq 8(%%eax), %%mm1 \n"
  "\tpfadd %%mm1, %%mm5 \n"

  "\tadd $16, %%eax \n"
  "\tsub $4,  %%ecx \n"

  "\tjae mul4_loop%= \n"

  "\tpfadd %%mm5,%%mm4 \n"

"mul4_skip%=: \n"

  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"

  "\tmovq (%%eax), %%mm0 \n"
  "\tadd $8, %%eax \n"
  "\tpfadd %%mm0, %%mm4 \n"
"mul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tpxor %%mm0, %%mm0 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tpfadd %%mm0, %%mm4 \n"
"even%=: \n"

  "\tpxor %%mm5, %%mm5 \n"
  "\tpfacc %%mm5, %%mm4 \n"
  "\tmovq %%mm4, (%%edx) \n"

  "\tpop %%ecx \n"
  "\tpop %%eax \n"
  "\tfemms \n"
  
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
  
  "\tpush %%eax \n"
  "\tpush %%ecx \n"
  "\tpxor %%mm4, %%mm4 \n"
  "\tpxor %%mm5, %%mm5 \n"

  "\tsub $4, %%ecx \n"
  "\tjb mul4_skip%= \n"

"mul4_loop%=: \n"
  "\tmovq (%%eax), %%mm0 \n"
  "\tpfmul %%mm0, %%mm0 \n"
  "\tpfadd %%mm0, %%mm4 \n"
  "\tmovq 8(%%eax), %%mm1 \n"
  "\tpfmul %%mm1, %%mm1 \n"
  "\tpfadd %%mm1, %%mm5 \n"

  "\tadd $16, %%eax \n"
  "\tsub $4,  %%ecx \n"

  "\tjae mul4_loop%= \n"

  "\tpfadd %%mm5,%%mm4 \n"

"mul4_skip%=: \n"

  "\tadd $2, %%ecx \n"
  "\tjae mul2_skip%= \n"

  "\tmovq (%%eax), %%mm0 \n"
  "\tpfmul %%mm0, %%mm0 \n"
  "\tadd $8, %%eax \n"
  "\tpfadd %%mm0, %%mm4 \n"
"mul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tpxor %%mm0, %%mm0 \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tpfmul %%mm0, %%mm0 \n"
  "\tpfadd %%mm0, %%mm4 \n"
"even%=: \n"

  "\tpxor %%mm5, %%mm5 \n"
  "\tpfacc %%mm5, %%mm4 \n"
  "\tmovq %%mm4, (%%edx) \n"

  "\tpop %%ecx \n"
  "\tpop %%eax \n"
  "\tfemms \n"
  
  : : "a" (a), "c" (len), "d" (sum)
CLOBBER_3DNOW
  );
    
  return sum[0];
}



inline void vec_inv_3dnow(const float *a, float *b, int len)
{
  __asm__ __volatile__ (
  
  "\tpush %%eax \n"
  "\tpush %%ecx \n"
  "\tpush %%edx \n"

  "\tsub $2, %%ecx \n"
  "\tjb mul2_skip%= \n"

"mul2_loop%=: \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tpfrcp %%mm0, %%mm2 \n"
  "\tmovd 4(%%eax), %%mm1 \n"
  "\tpfrcp %%mm1, %%mm3 \n"
  //punpckldq %%mm0, %%mm0
  //punpckldq %%mm1, %%mm1
  "\tpfrcpit1 %%mm2, %%mm0 \n"
  "\tpfrcpit1 %%mm3, %%mm1 \n"
  "\tpfrcpit2 %%mm2, %%mm0 \n"
  "\tpfrcpit2 %%mm3, %%mm1 \n"

  "\tmovd %%mm0, (%%edx) \n"
  "\tmovd %%mm1, 4(%%edx) \n"
  "\tadd $8, %%eax \n"
  "\tadd $8, %%edx \n"
  "\tsub $2,  %%ecx \n"

  "\tjae mul2_loop%= \n"

"mul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tmovd (%%eax), %%mm0 \n"
  "\tpfrcp %%mm0, %%mm2 \n"
  //punpckldq %%mm0, %%mm0
  "\tpfrcpit1 %%mm2, %%mm0 \n"
  "\tpfrcpit2 %%mm2, %%mm0 \n"
  "\tmovd %%mm0, (%%edx) \n"
"even%=: \n"

  "\tpop %%edx \n"
  "\tpop %%ecx \n"
  "\tpop %%eax \n"
  "\tfemms \n"
  
  : : "a" (a), "c" (len), "d" (b)
CLOBBER_3DNOW
  );
}



inline void vec_sqrt_3dnow(const float *a, float *b, int len)
{
  __asm__ __volatile__ (
  
  "\tpush %%eax \n"
  "\tpush %%ecx \n"
  "\tpush %%edx \n"

  "\tsub $2, %%ecx \n"
  "\tjb mul2_skip%= \n"

"mul2_loop%=: \n"
  "\tmovd (%%eax), %%mm0 \n"
  "\tmovd 4(%%eax), %%mm1 \n"
  "\tmovq %%mm0, %%mm6 \n"
  "\tmovq %%mm1, %%mm7 \n"
  "\tpfrsqrt %%mm0, %%mm2 \n"
  "\tpfrsqrt %%mm1, %%mm3 \n"
  "\tmovq %%mm2, %%mm4 \n"
  "\tmovq %%mm3, %%mm5 \n"
  "\tpfmul %%mm2, %%mm2 \n"
  "\tpfmul %%mm3, %%mm3 \n"
  "\tpfrsqit1 %%mm2, %%mm0 \n"
  "\tpfrsqit1 %%mm3, %%mm1 \n"
  "\tpfrcpit2 %%mm4, %%mm0 \n"
  "\tpfrcpit2 %%mm5, %%mm1 \n"
  "\tpfmul %%mm6, %%mm0 \n"
  "\tpfmul %%mm7, %%mm1 \n"

  "\tmovd %%mm0, (%%edx) \n"
  "\tmovd %%mm1, 4(%%edx) \n"
  "\tadd $8, %%eax \n"
  "\tadd $8, %%edx \n"
  "\tsub $2,  %%ecx \n"

  "\tjae mul2_loop%= \n"

"mul2_skip%=: \n"

  "\tand $1, %%ecx \n"
  "\tjz even%= \n"

  "\tmovd (%%eax), %%mm0 \n"
  "\tmovq %%mm0, %%mm6 \n"
  "\tpfrsqrt %%mm0, %%mm2 \n"
  "\tmovq %%mm2, %%mm4 \n"
  "\tpfmul %%mm2, %%mm2 \n"
  "\tpfrsqit1 %%mm2, %%mm0 \n"
  "\tpfrcpit2 %%mm4, %%mm0 \n"
  "\tpfmul %%mm6, %%mm0 \n"

  "\tmovd %%mm0, (%%edx) \n"
"even%=: \n"

  "\tpop %%edx \n"
  "\tpop %%ecx \n"
  "\tpop %%eax \n"
  "\tfemms \n"
  
  : : "a" (a), "c" (len), "d" (b)
CLOBBER_3DNOW
  );
}

#else /* _ENABLE_3DNOW */

#define ERROR_3DNOW_NI {throw new FD::GeneralException("Trying to use 3DNow!, but Overflow not compiled with _ENABLE_3DNOW. Bad, bad, this should never happen", __FILE__, __LINE__);}

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

}//namespace FD

#endif
