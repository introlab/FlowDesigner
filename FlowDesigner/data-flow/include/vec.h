#ifndef VEC_H
#define VEC_H


template <class T>
inline void vec_copy(T *x, T *y, int len)
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
inline T vec_inner_prod(T *a, T *b, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  T *end = a+len;
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
inline void vec_add_vec(T *a, T *b, T *c, int len)
{
  T *end = a+len;
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
inline void vec_sub_vec(T *a, T *b, T *c, int len)
{
  T *end = a+len;
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
inline void vec_mul_vec(T *a, T *b, T *c, int len)
{
  T *end = a+len;
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
inline void vec_add_scal(T a, T *b, T *c, int len)
{
  T *end = b+len;
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
inline void vec_mul_scal(T a, T *b, T *c, int len)
{
  T *end = b+len;
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
inline T vec_dist2(T *a, T *b, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  T *end = a+len;
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
inline T vec_sum(T *a, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  T *end = a+len;
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
inline T vec_norm2(T *a, int len)
{
  T sum1=0, sum2=0, sum3=0, sum4=0;
  T *end = a+len;
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
inline void vec_inv(T *a, T *b, int len)
{
  T *end = a+len;
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
inline void vec_sqrt(T *a, T *b, int len)
{
  T *end = a+len;
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

#ifdef _USE_3DNOW

#define FP_DIRTY   : "memory", "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)"


template <>
inline float vec_inner_prod<float>(float *a, float *b, int len)
{
  //float sum=0;
  float sum[2]={0,0};
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ebx
  push %%ecx
  pxor %%mm4, %%mm4
  pxor %%mm5, %%mm5

  sub $4, %%ecx
  //jb mul4_skip
  jb .+47

//mul4_loop:
  movq (%%eax), %%mm0
  movq (%%ebx), %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%ebx), %%mm3
  add $16, %%eax
  add $16, %%ebx
  pfmul %%mm0, %%mm1
  pfmul %%mm2, %%mm3
  pfadd %%mm1, %%mm4
  pfadd %%mm3, %%mm5

  sub $4,  %%ecx

  //jae mul4_loop
  jae .-39

  pfadd %%mm5,%%mm4

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+22

  movq (%%eax), %%mm0
  movq (%%ebx), %%mm1
  add $8, %%eax
  add $8, %%ebx
  pfmul %%mm0, %%mm1
  pfadd %%mm1, %%mm4
//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+22

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%ebx), %%mm1
  pfmul %%mm0, %%mm1
  pfadd %%mm1, %%mm4
//even:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%ebx
  pop %%eax
  emms
  "
  : : "a" (a), "b" (b), "c" (len), "d" (sum)
FP_DIRTY
  );
    
  return sum[0];
}


template <>
inline void vec_add_vec<float>(float *a, float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ebx
  push %%ecx
  push %%edx

  sub $4, %%ecx
  //jb mul4_skip
  jb .+45

//mul4_loop:
  movq (%%eax), %%mm0
  movq (%%ebx), %%mm1
  pfadd %%mm0, %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%ebx), %%mm3
  pfadd %%mm2, %%mm3

  movq %%mm1, (%%edx)
  movq %%mm3, 8(%%edx)
  add $16, %%eax
  add $16, %%ebx
  add $16, %%edx
  sub $4,  %%ecx

  //jae mul4_loop
  jae .-41

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+24
  
  movq (%%eax), %%mm0
  movq (%%ebx), %%mm1
  pfadd %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%eax
  add $8, %%ebx
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%ebx), %%mm1
  pfadd %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%ebx
  pop %%eax
  emms
  "
  : : "a" (a), "b" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline void vec_sub_vec<float>(float *a, float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ebx
  push %%ecx
  push %%edx

  sub $4, %%ecx
  //jb mul4_skip
  jb .+45

//mul4_loop:
  movq (%%eax), %%mm0
  movq (%%ebx), %%mm1
  pfsubr %%mm0, %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%ebx), %%mm3
  pfsubr %%mm2, %%mm3

  movq %%mm1, (%%edx)
  movq %%mm3, 8(%%edx)
  add $16, %%eax
  add $16, %%ebx
  add $16, %%edx
  sub $4,  %%ecx

  //jae mul4_loop
  jae .-41

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+24

  movq (%%eax), %%mm0
  movq (%%ebx), %%mm1
  pfsubr %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%eax
  add $8, %%ebx
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%ebx), %%mm1
  pfsubr %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%ebx
  pop %%eax
  emms
  "
  : : "a" (a), "b" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline void vec_mul_vec<float>(float *a, float *b, float *c, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ebx
  push %%ecx
  push %%edx

  sub $4, %%ecx
  //jb mul4_skip
  jb .+45

//mul4_loop:
  movq (%%eax), %%mm0
  movq (%%ebx), %%mm1
  pfmul %%mm0, %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%ebx), %%mm3
  pfmul %%mm2, %%mm3

  movq %%mm1, (%%edx)
  movq %%mm3, 8(%%edx)
  add $16, %%eax
  add $16, %%ebx
  add $16, %%edx
  sub $4,  %%ecx

  //jae mul4_loop
  jae .-41

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+24

  movq (%%eax), %%mm0
  movq (%%ebx), %%mm1
  pfmul %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%eax
  add $8, %%ebx
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%ebx), %%mm1
  pfmul %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%ebx
  pop %%eax
  emms
  "
  : : "a" (a), "b" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline void vec_add_scal<float>(float a, float *b, float *c, int len)
{
  float tmp[2];
  tmp[0]=tmp[1]=a;
  __asm__ __volatile__ (
  "
  push %%ebx
  push %%ecx
  push %%edx
  movq (%%eax), %%mm0

  sub $4, %%ecx
  //jb mul4_skip
  jb .+35

//mul4_loop:
  movq (%%ebx), %%mm1
  pfadd %%mm0, %%mm1
  movq 8(%%ebx), %%mm2
  pfadd %%mm0, %%mm2

  movq %%mm1, (%%edx)
  movq %%mm2, 8(%%edx)
  add $16, %%ebx
  add $16, %%edx
  sub $4,  %%ecx

  //jae mul4_loop
  jae .-31

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+18
  
  movq (%%ebx), %%mm1
  pfadd %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%ebx
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%ebx), %%mm1
  pfadd %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%ebx
  emms
  "
  : : "a" (tmp), "b" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline void vec_mul_scal<float>(float a, float *b, float *c, int len)
{
  float tmp[2];
  tmp[0]=tmp[1]=a;
  __asm__ __volatile__ (
  "
  push %%ebx
  push %%ecx
  push %%edx
  movq (%%eax), %%mm0

  sub $4, %%ecx
  //jb mul4_skip
  jb .+35

//mul4_loop:
  movq (%%ebx), %%mm1
  pfmul %%mm0, %%mm1
  movq 8(%%ebx), %%mm2
  pfmul %%mm0, %%mm2

  movq %%mm1, (%%edx)
  movq %%mm2, 8(%%edx)
  add $16, %%ebx
  add $16, %%edx
  sub $4,  %%ecx

  //jae mul4_loop
  jae .-31

//mul4_skip:

  add $2, %%ecx
  //jae mul2_skip
  jae .+18
  
  movq (%%ebx), %%mm1
  pfmul %%mm0, %%mm1
  movq %%mm1, (%%edx)
  add $8, %%ebx
  add $8, %%edx

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+21

  pxor %%mm0, %%mm0
  pxor %%mm1, %%mm1
  movd (%%eax), %%mm0
  movd (%%ebx), %%mm1
  pfmul %%mm0, %%mm1
  movd %%mm1, (%%edx)
//even:

  pop %%edx
  pop %%ecx
  pop %%ebx
  emms
  "
  : : "a" (tmp), "b" (b), "c" (len), "d" (c)
FP_DIRTY
  );
}


template <>
inline float vec_dist2<float>(float *a, float *b, int len)
{
  //float sum=0;
  float sum[2]={0,0};
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ebx
  push %%ecx
  pxor %%mm4, %%mm4
  pxor %%mm5, %%mm5

  sub $4, %%ecx
  //jb mul4_skip
  jb .+55

//mul4_loop:
  movq (%%eax), %%mm0
  movq (%%ebx), %%mm1
  movq 8(%%eax), %%mm2
  movq 8(%%ebx), %%mm3
  add $16, %%eax
  add $16, %%ebx
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
  movq (%%ebx), %%mm1
  add $8, %%eax
  add $8, %%ebx
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
  movd (%%ebx), %%mm1
  pfsub %%mm0, %%mm1
  pfmul %%mm1, %%mm1
  pfadd %%mm1, %%mm4
//even:

  pxor %%mm5, %%mm5
  pfacc %%mm5, %%mm4
  movq %%mm4, (%%edx)

  pop %%ecx
  pop %%ebx
  pop %%eax
  emms
  "
  : : "a" (a), "b" (b), "c" (len), "d" (sum)
FP_DIRTY
  );
    
  return sum[0];
}


template <>
inline float vec_sum<float>(float *a, int len)
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
inline float vec_norm2<float>(float *a, int len)
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
inline void vec_inv<float>(float *a, float *b, int len)
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
inline void vec_sqrt<float>(float *a, float *b, int len)
{
  __asm__ __volatile__ (
  "
  push %%eax
  push %%ecx
  push %%edx

  sub $2, %%ecx
  //jb mul2_skip
  jb .+65

//mul2_loop:
  movd (%%eax), %%mm0
  movd 4(%%eax), %%mm1
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

  movd %%mm0, (%%edx)
  movd %%mm1, 4(%%edx)
  add $8, %%eax
  add $8, %%edx
  sub $2,  %%ecx

  //jae mul2_loop
  jae .-61

//mul2_skip:

  and $1, %%ecx
  //jz even
  jz .+27

  movd (%%eax), %%mm0
  pfrsqrt %%mm0, %%mm2
  movq %%mm2, %%mm4
  pfmul %%mm2, %%mm2
  pfrsqit1 %%mm2, %%mm0
  pfrcpit2 %%mm4, %%mm0

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



#endif /* ifdef USE_3DNOW */

#endif /* ifndef VEC_H*/
