// Copyright (C) 2001 Jean-Marc Valin


//This file contains vector primitives and supports 3DNow! and SSE (incomplete) optimization
#ifndef VEC_H
#define VEC_H

#include <math.h>

#include "vec_sse.h"
#include "vec_3dnow.h"
#include "iextensions.h"

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
      sum2+=a[1]*a[1];
      sum3+=a[2]*a[2];
      sum4+=a[3]*a[4];
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




/*Here are float (REAL*4) specific versions of the routines */


inline float vec_inner_prod_float(const float *a, const float *b, int len)
{
   float sum1=0, sum2=0, sum3=0, sum4=0;
   const float *end = a+len;
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



template <>
inline float vec_inner_prod<float>(const float *a, const float *b, int len)
{
#ifndef WIN32
   if (_ALLOW_3DNOW && len >= 8 && IExtensions::have3DNow())
      vec_inner_prod_3dnow(a,b,len);
   else if (_ALLOW_SSE && len >=8 && IExtensions::haveSSE())
      vec_inner_prod_sse(a,b,len);
   else
#endif
      vec_inner_prod_float(a,b,len);      
}





#endif /* ifndef VEC_H*/
