// Copyright (C) 2001 Jean-Marc Valin


//This file contains vector primitives and supports 3DNow! and SSE (incomplete) optimization
#ifndef VEC_H
#define VEC_H

#include <math.h>


/* Prefetch routines, may be used on any type (not only float and SSE2 double) */

#ifdef _USE_3DNOW /*This means we're likely to have 64-byte cache lines (unless it's a K6-II)*/

#ifndef CACHE_LINES
#define CACHE_LINES 64
#endif /*CACHE_LINES */

#ifndef MAX_PREFETCH
#define MAX_PREFETCH 8192  /* The Athlon L1 size is 64 kB */
#endif /*MAX_PREFETCH*/

#ifndef CACHE_MASK
#define CACHE_MASK 0xffffffd0
#endif /*CACHE_MASK */

#else /*_USE_3DNOW*/

#ifndef CACHE_LINES
#define CACHE_LINES 32
#endif /*CACHE_LINES */

#ifndef CACHE_MASK
#define CACHE_MASK 0xffffffe0
#endif /*CACHE_MASK */

#ifndef MAX_PREFETCH
#define MAX_PREFETCH 4096  /* The PIII L1 size is 32 kB */
#endif /*MAX_PREFETCH*/

#endif

#if defined(_USE_SSE) || defined(_USE_3DNOW) || defined (_USE_PREFETCH)

template <class T>
void vec_prefetchnta(T *a, int len)
{
   int size=sizeof(T)*len;
   if (size > MAX_PREFETCH)
      return;
   __asm__ /*__volatile__*/ (
   "
   push %%eax
   push %%ecx

prefetch_loop%=:
   prefetchnta (%%eax)
   add %%edi, %%eax
   sub %%edi, %%ecx
   ja prefetch_loop%=

   pop %%ecx
   pop %%eax
   "
   : : "a" (a), "D" (CACHE_LINES), "c" (size)
   : "memory"
   );
}

template <class T>
void vec_prefetch(T *a, int len)
{
   int size=sizeof(T)*len;
   if (size > MAX_PREFETCH)
      return;
   __asm__ /*__volatile__*/ (
   "
   push %%eax
   push %%ecx

prefetch_loop%=:
   prefetch0 (%%eax)
   add %%edi, %%eax
   sub %%edi, %%ecx
   ja prefetch_loop%=

   pop %%ecx
   pop %%eax
   "
   : : "a" (a), "D" (CACHE_LINES), "c" (size)
   : "memory"
   );
}


#else

template <class T>
void vec_prefetchnta(T *a, int len)
{
}

template <class T>
void vec_prefetch(T *a, int len)
{
}

#endif



template <class T>
inline void vec_copy(const T *x, T *y, int len)
{
   while (len > 3)
   {
      *y++ = *x++;
      *y++ = *x++;
      *y++ = *x++;
      *y++ = *x++;
      len -= 4;
   }
   while (len)
   {
      *y++ = *x++;
      len--;
   }
}


template <class T>
inline T vec_inner_prod(const T *a, const T *b, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  const T *end = a+len;
  while (a<end-3)
    {
      sum1+=a[0]*b[0];
      sum2+=a[1]*b[1];
      sum3+=a[2]*b[2];
      sum4+=a[3]*b[3];
      a+=4;
      b+=4;
    }
  while (a<end)
    {
      sum1+=a[0]*b[0];
      a++; b++;
    }
  return (sum1+sum2)+(sum3+sum4);
}

template <class T>
inline void vec_add_vec(const T *a, const T *b, T *c, int len)
{
  const T *end = a+len;
  while (a<end-3)
    {
      c[0]=a[0]+b[0];
      c[1]=a[1]+b[1];
      c[2]=a[2]+b[2];
      c[3]=a[3]+b[3];
      a+=4;
      b+=4;
      c+=4;
    }
  while (a<end)
    {
      c[0]=a[0]+b[0];
      a++; b++; c++;
    }
}

template <class T>
inline void vec_sub_vec(const T *a, const T *b, T *c, int len)
{
  const T *end = a+len;
  while (a<end-3)
    {
      c[0]=a[0]-b[0];
      c[1]=a[1]-b[1];
      c[2]=a[2]-b[2];
      c[3]=a[3]-b[3];
      a+=4;
      b+=4;
      c+=4;
    }
  while (a<end)
    {
      c[0]=a[0]-b[0];
      a++; b++; c++;
    }
}

template <class T>
inline void vec_mul_vec(const T *a, const T *b, T *c, int len)
{
  const T *end = a+len;
  while (a<end-3)
    {
      c[0]=a[0]*b[0];
      c[1]=a[1]*b[1];
      c[2]=a[2]*b[2];
      c[3]=a[3]*b[3];
      a+=4;
      b+=4;
      c+=4;
    }
  while (a<end)
    {
      c[0]=a[0]*b[0];
      a++; b++; c++;
    }
}

template <class T>
inline void vec_mul_and_add(const T *a, const T *b, T *c, int len)
{
  const T *end = a+len;
  while (a<end-3)
    {
      c[0]+=a[0]*b[0];
      c[1]+=a[1]*b[1];
      c[2]+=a[2]*b[2];
      c[3]+=a[3]*b[3];
      a+=4;
      b+=4;
      c+=4;
    }
  while (a<end)
    {
      c[0]+=a[0]*b[0];
      a++; b++; c++;
    }
}

template <class T>
inline void vec_mul_and_add(const T a, const T *b, T *c, int len)
{
  const T *end = b+len;
  while (b<end-3)
    {
      c[0]+=a*b[0];
      c[1]+=a*b[1];
      c[2]+=a*b[2];
      c[3]+=a*b[3];
      b+=4;
      c+=4;
    }
  while (b<end)
    {
      c[0]+=a*b[0];
      b++; c++;
    }
}

template <class T>
inline void vec_div_vec(const T *a, const T *b, T *c, int len)
{
  const T *end = a+len;
  while (a<end-3)
    {
      c[0]=a[0]/b[0];
      c[1]=a[1]/b[1];
      c[2]=a[2]/b[2];
      c[3]=a[3]/b[3];
      a+=4;
      b+=4;
      c+=4;
    }
  while (a<end)
    {
      c[0]=a[0]/b[0];
      a++; b++; c++;
    }
}


template <class T>
inline void vec_add_scal(const T a, const T *b, T *c, int len)
{
  const T *end = b+len;
  while (b<end-3)
    {
      c[0]=a+b[0];
      c[1]=a+b[1];
      c[2]=a+b[2];
      c[3]=a+b[3];
      b+=4;
      c+=4;
    }
  while (b<end)
    {
      c[0]=a+b[0];
      b++; c++;
    }
}

template <class T>
inline void vec_mul_scal(const T a, const T *b, T *c, int len)
{
  const T *end = b+len;
  while (b<end-3)
    {
      c[0]=a*b[0];
      c[1]=a*b[1];
      c[2]=a*b[2];
      c[3]=a*b[3];
      b+=4;
      c+=4;
    }
  while (b<end)
    {
      c[0]=a*b[0];
      b++; c++;
    }
}

template <class T>
inline T vec_dist2(const T *a, const T *b, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  const T *end = a+len;
  while (a<end-3)
    {
      sum1+=(a[0]-b[0])*(a[0]-b[0]);
      sum2+=(a[1]-b[1])*(a[1]-b[1]);
      sum3+=(a[2]-b[2])*(a[2]-b[2]);
      sum4+=(a[3]-b[3])*(a[3]-b[3]);
      a+=4;
      b+=4;
    }
  while (a<end)
    {
      sum1+=(a[0]-b[0])*(a[0]-b[0]);
      a++; b++;
    }
  return (sum1+sum2)+(sum3+sum4);
}

template <class T>
inline T vec_mahalanobis2(const T *a, const T *b, const T *c, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  const T *end = a+len;
  while (a<end-3)
    {
      sum1+=c[0]*(a[0]-b[0])*(a[0]-b[0]);
      sum2+=c[1]*(a[1]-b[1])*(a[1]-b[1]);
      sum3+=c[2]*(a[2]-b[2])*(a[2]-b[2]);
      sum4+=c[3]*(a[3]-b[3])*(a[3]-b[3]);
      a+=4;
      b+=4;
      c+=4;
    }
  while (a<end)
    {
      sum1+=c[0]*(a[0]-b[0])*(a[0]-b[0]);
      a++; b++; c++;
    }
  return (sum1+sum2)+(sum3+sum4);
}

template <class T>
inline T vec_sum(const T *a, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  const T *end = a+len;
  while (a<end-3)
    {
      sum1+=a[0];
      sum2+=a[1];
      sum3+=a[2];
      sum4+=a[3];
      a+=4;
    }
  while (a<end)
    {
      sum1+=a[0];
      a++;
    }
  return (sum1+sum2)+(sum3+sum4);
}

template <class T>
inline T vec_norm2(const T *a, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  const T *end = a+len;
  while (a<end-3)
    {
      sum1+=a[0]*a[0];
      sum2+=a[1]*a[0];
      sum3+=a[2]*a[0];
      sum4+=a[3]*a[0];
      a+=4;
    }
  while (a<end)
    {
      sum1+=a[0]*a[0];
      a++;
    }
  return (sum1+sum2)+(sum3+sum4);
}

template <class T>
inline void vec_inv(const T *a, T *b, int len)
{
  const T *end = a+len;
  while (a<end-3)
    {
      b[0]=1/a[0];
      b[1]=1/a[1];
      b[2]=1/a[2];
      b[3]=1/a[3];
      a+=4; b+=4;
    }
  while (a<end)
    {
      b[0]=1/a[0];
      a++; b++;
    }
}

template <class T>
inline void vec_sqrt(const T *a, T *b, int len)
{
  const T *end = a+len;
  while (a<end-3)
    {
      b[0]=sqrt(a[0]);
      b[1]=sqrt(a[1]);
      b[2]=sqrt(a[2]);
      b[3]=sqrt(a[3]);
      a+=4; b+=4;
    }
  while (a<end)
    {
      b[0]=sqrt(a[0]);
      a++; b++;
    }
}

template <class T>
inline void vec_corr_cont(const T *a, T *filt, T *out, int len, int filtLen)
{
   for (int i=0;i<len;i++)
      out[i] = vec_inner_prod(a-filtLen+1, filt, filtLen);
}

template <class T>
inline void vec_conv_cont(const T *a, T *filt, T *out, int len, int filtLen)
{
   T filt2[filtLen];
   for (int i=0;i<filtLen;i++)
      filt2[i] = filt[filtLen-i-1];
   for (int i=0;i<len;i++)
      out[i] = vec_inner_prod(a-filtLen+1, filt2, filtLen);
}

#ifdef _USE_3DNOW

#define FP_DIRTY   : "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)", "memory"


template <>
inline float vec_inner_prod<float>(const float *a, const float *b, int len)
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
  //jb mul4_skip
  jb .+47

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
  //jae .-39

  pfadd %%mm5,%%mm4

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+22

  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  add $8, %%eax
  add $8, %%edi
  pfmul %%mm0, %%mm1
  pfadd %%mm1, %%mm4
//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+22

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  pfadd %%mm1, %%mm4
//even:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%edi
  pop %%eax
  emms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (sum)
FP_DIRTY
  );
    
  return sum[0];
}


template <>
inline void vec_add_vec<float>(const float *a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  push %%edx

  sub $4, %%ecx
  //jb mul4_skip
  jb .+45

//mul4_loop:
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

  //jae mul4_loop
  jae .-41

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+24
  
  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%eax
  add $8, %%edi
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%edi
  pop %%eax
  emms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline void vec_sub_vec<float>(const float *a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  push %%edx

  sub $4, %%ecx
  //jb mul4_skip
  jb .+45

//mul4_loop:
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

  //jae mul4_loop
  jae .-41

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+24

  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  pfsubr %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%eax
  add $8, %%edi
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfsubr %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%edi
  pop %%eax
  emms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline void vec_mul_vec<float>(const float *a, const float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  push %%edx

  sub $4, %%ecx
  //jb mul4_skip
  jb .+45

//mul4_loop:
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

  //jae mul4_loop
  jae .-41

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+24

  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%eax
  add $8, %%edi
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%edi
  pop %%eax
  emms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline void vec_add_scal<float>(const float a, const float *b, float *c, int len)
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
  //jb mul4_skip
  jb .+35

//mul4_loop:
  movq (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movq 8(%%edi), %%mm2
  pfadd %%mm0, %%mm2

  movq %%mm1, (%%edx)
  movq %%mm2, 8(%%edx)
  add $16, %%edi
  add $16, %%edx
  sub $4,  %%ecx

  //jae mul4_loop
  jae .-31

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+18
  
  movq (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%edi
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfadd %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%edi
  emms
  "
  : : "a" (tmp), "D" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline void vec_mul_scal<float>(const float a, const float *b, float *c, int len)
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
  //jb mul4_skip
  jb .+35

//mul4_loop:
  movq (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movq 8(%%edi), %%mm2
  pfmul %%mm0, %%mm2

  movq %%mm1, (%%edx)
  movq %%mm2, 8(%%edx)
  add $16, %%edi
  add $16, %%edx
  sub $4,  %%ecx

  //jae mul4_loop
  jae .-31

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+18
  
  movq (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%edi
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfmul %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%edi
  emms
  "
  : : "a" (tmp), "D" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline float vec_dist2<float>(const float *a, const float *b, int len)
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
  //jb mul4_skip
  jb .+55

//mul4_loop:
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

  //jae mul4_loop
  jae .-47

  pfadd %%mm5,%%mm4

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+26

  movq (%%eax), %%mm0
  movq (%%edi), %%mm1
  add $8, %%eax
  add $8, %%edi
  pfsub %%mm0, %%mm1
  pfmul %%mm1, %%mm1
  pfadd %%mm1, %%mm4
//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+26

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%edi), %%mm1
  pfsub %%mm0, %%mm1
  pfmul %%mm1, %%mm1
  pfadd %%mm1, %%mm4
//even:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%edi
  pop %%eax
  emms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (sum)
FP_DIRTY
  );
    
  return sum[0];
}


template <>
inline float vec_sum<float>(const float *a, int len)
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
  //jb mul4_skip
  jb .+29

//mul4_loop:
  movq (%%eax), %%mm0
  pfadd %%mm0, %%mm4
  movq 8(%%eax), %%mm1
  pfadd %%mm1, %%mm5

  add $16, %%eax
  sub $4,  %%ecx

  //jae mul4_loop
  jae .-21

  pfadd %%mm5,%%mm4

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+12

  movq (%%eax), %%mm0
  add $8, %%eax
  pfadd %%mm0, %%mm4
//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+12

  pxor %%mm0, %%mm0
  movd (%%eax), %%mm0
  pfadd %%mm0, %%mm4
//even:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%eax
  emms
  "
  : : "a" (a), "c" (len), "d" (sum)
FP_DIRTY
  );
    
  return sum[0];
}

template <>
inline float vec_norm2<float>(const float *a, int len)
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
  //jb mul4_skip
  jb .+37

//mul4_loop:
  movq (%%eax), %%mm0
  pfmul %%mm0, %%mm0
  pfadd %%mm0, %%mm4
  movq 8(%%eax), %%mm1
  pfmul %%mm1, %%mm1
  pfadd %%mm1, %%mm5

  add $16, %%eax
  sub $4,  %%ecx

  //jae mul4_loop
  jae .-29

  pfadd %%mm5,%%mm4

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+16

  movq (%%eax), %%mm0
  pfmul %%mm0, %%mm0
  add $8, %%eax
  pfadd %%mm0, %%mm4
//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+16

  pxor %%mm0, %%mm0
  movd (%%eax), %%mm0
  pfmul %%mm0, %%mm0
  pfadd %%mm0, %%mm4
//even:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%eax
  emms
  "
  : : "a" (a), "c" (len), "d" (sum)
FP_DIRTY
  );
    
  return sum[0];
}


template <>
inline void vec_inv<float>(const float *a, float *b, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ecx
  push %%edx

  sub $2, %%ecx
  //jb mul2_skip
  jb .+51

//mul2_loop:
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

  //jae mul2_loop
  jae .-47

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+20

  movd (%%eax), %%mm0
  pfrcp %%mm0, %%mm2
  //punpckldq %%mm0, %%mm0
  pfrcpit1 %%mm2, %%mm0
  pfrcpit2 %%mm2, %%mm0
  movd %%mm0, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%eax
  emms
  "
  : : "a" (a), "c" (len), "d" (b)
FP_DIRTY
  );
}


template <>
inline void vec_sqrt<float>(const float *a, float *b, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ecx
  push %%edx

  sub $2, %%ecx
  //jb mul2_skip
  jb .+79

//mul2_loop:
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

  //jae mul2_loop
  jae .-75

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+34

  movd (%%eax), %%mm0
  movq %%mm0, %%mm6
  pfrsqrt %%mm0, %%mm2
  movq %%mm2, %%mm4
  pfmul %%mm2, %%mm2
  pfrsqit1 %%mm2, %%mm0
  pfrcpit2 %%mm4, %%mm0
  pfmul %%mm6, %%mm0

  movd %%mm0, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%eax
  emms
  "
  : : "a" (a), "c" (len), "d" (b)
FP_DIRTY
  );
}



#endif /* ifdef _USE_3DNOW */


#ifdef _USE_SSE

#define FP_DIRTY   : "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)", "memory"

template <>
inline float vec_inner_prod<float>(const float *a, const float *b, int len)
{
  float sum;
  __asm__ __volatile__ (
  "
  push %%eax
  push %%edi
  push %%ecx
  xorps %%xmm3, %%xmm3
  xorps %%xmm4, %%xmm4

  //jmp cond1

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
  shufps $33, %%xmm4, %%xmm4
  addss %%xmm4, %%xmm3
  movups %%xmm3, (%%edx)
  
  pop %%ecx
  pop %%edi
  pop %%eax
  //emms
  "
  : : "a" (a), "D" (b), "c" (len), "d" (&sum)
FP_DIRTY
  );
  return sum;
  //cerr << sum[0] << " " << sum[1] << endl;
  //return sum[0];//+sum[1];//+sum[2]+sum[3];
}

#endif


#endif /* ifndef VEC_H*/
