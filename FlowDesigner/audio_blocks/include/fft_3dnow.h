#ifndef FFT_3DNOW_H
#define FFT_3DNOW_H


#include <math.h>


void fft_initCosSinTables_3dnow(complex<float> *w, int *bits, int M)
{
   int i,j;
   int tmp;
   int size = 1 << M;
   for (i=0;i<size;i++)
   {
      bits[i]=0;
      tmp=i;
      for (j=0;j<M;j+=2)
      {
	 bits[i] <<= 2;
	 bits[i] += tmp&3;
	 tmp>>=2;
      }
   }
   
   while (size)
   {
      int     k;
      float   tmp, p = (2.0 * M_PI) / size;
      complex<float> tmp2;
      for (k = 0; k < (size>>1); k++) {
	 tmp = k * p;
	 tmp2.re=cos(tmp);
	 tmp2.im=-sin(tmp);
	 *w++ = tmp2;
      }
      for (k = 0; k < (size>>2); k++) {
	 tmp = 2*k * p;
	 tmp2.re=cos(tmp);
	 tmp2.im=-sin(tmp);
	 *w++ = tmp2;
      }
      for (k = 0; k < (size>>2); k++) {
	 tmp = 3*k * p;
	 tmp2.re=cos(tmp);
	 tmp2.im=-sin(tmp);
	 *w++ = tmp2;
      }
      size >>= 1;
   }
}



class _negmask {
      int a;
      int b;
   public:
      _negmask() : a(0x80000000) , b(0x00000000) {}
};

inline void fft_3dnow(complex<float> *in, complex<float> *_x, int _M, complex<float> *_w, int *bits)
{
   complex<float> *w=_w+(1<<(_M+1))-2-6;
   _negmask mask;
   int rep = 1<<(_M-2);

      __asm__ __volatile__ (
	    "
      push %0
      push %1
      push %3
      push %4
      movq %2, %%mm7
.align 16
.loop%=:
      mov (%4), %%edx
      movq (%3,%%edx,8), %%mm0
      mov 8(%4), %%edx
      movq (%3,%%edx,8), %%mm1

      movq %%mm0, %%mm4
      pfadd %%mm1, %%mm0
      pfsub %%mm1, %%mm4

      mov 4(%4), %%edx
      movq (%3,%%edx,8), %%mm2
      mov 12(%4), %%edx
      movq (%3,%%edx,8), %%mm3

      movq %%mm2, %%mm5
      pfadd %%mm3, %%mm2

      mov 16(%4), %%edx
      pfsub %%mm3, %%mm5
      
      movq    %%mm0, %%mm1
      pfsub   %%mm2, %%mm0
      pfadd   %%mm2, %%mm1
      pswapd  %%mm5, %%mm5
      movq    %%mm0, 16(%0)
      movq    %%mm1, (%0)

      movq (%3,%%edx,8), %%mm0

      pxor    %%mm7, %%mm5
      movq    %%mm4, %%mm6
      mov 24(%4), %%edx
      pfsub   %%mm5, %%mm4
      pfadd   %%mm5, %%mm6

      movq (%3,%%edx,8), %%mm1

      movq    %%mm4, 8(%0)



      movq    %%mm6, 24(%0)
      mov 20(%4), %%edx
      movq (%3,%%edx,8), %%mm2
      mov 28(%4), %%edx
      movq (%3,%%edx,8), %%mm3

      movq %%mm0, %%mm4
      pfadd %%mm1, %%mm0

      movq %%mm2, %%mm5
      pfadd %%mm3, %%mm2

      pfsub %%mm1, %%mm4
      pfsub %%mm3, %%mm5
      
      movq    %%mm0, %%mm1
      pfsub   %%mm2, %%mm0
      pfadd   %%mm2, %%mm1
      pswapd  %%mm5, %%mm5
      movq    %%mm0, 48(%0)
      movq    %%mm1, 32(%0)

      pxor    %%mm7, %%mm5
      movq    %%mm4, %%mm6
      pfsub   %%mm5, %%mm4
      pfadd   %%mm5, %%mm6

      movq    %%mm6, 56(%0)
      movq    %%mm4, 40(%0)

      add $64, %0
      add $32, %4







      dec %1
      jne .loop%=
      pop %4
      pop %3
      pop %1
      pop %0
      "
	    : : "r" (_x), "q" (rep>>1), "m" (mask), "r" (in), "r" (bits)
	    : "edx", "memory", "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)");

   for (int M=4;M<=_M;M+=2)
   {
      complex<float> *x=_x;
      rep >>= 2;
      int repeat = rep;
      int mul=repeat;
      int N = 1 << M;
      int N2 = N >> 1;
      int N4 = N >> 2;
      w-=(N+N2);
      //cerr << "M = " << M << "\tN = " << N << "\twoff = " << w-_w << "\trepeat = " << repeat << endl;
           
	 while (repeat--)
	 {
	    {
	       __asm__ __volatile__ (
		  "
            push %0
            push %1
            push %6

            movq %5, %%mm7

            movq (%0), %%mm0
            movq (%3), %%mm1
            movq (%0,%2,8), %%mm2
            movq (%3,%2,8), %%mm3

            movq %%mm0, %%mm4    ;//x0
            movq %%mm1, %%mm5    ;//x1
            pfadd %%mm2, %%mm0   ;//es
            pfadd %%mm3, %%mm1   ;//os
            pfsub %%mm2, %%mm4   ;//ed
            pfsub %%mm3, %%mm5   ;//od
            pswapd %%mm5, %%mm5  ;//od'
            pxor %%mm7, %%mm5    ;//od'

            movq %%mm0, %%mm2    ;//es
            movq %%mm4, %%mm3    ;//ed

            pfadd %%mm1, %%mm0   ;//x0
            pfadd %%mm5, %%mm4   ;//x3
            pfsub %%mm1, %%mm2   ;//x2
            pfsub %%mm5, %%mm3   ;//x1
            movq %%mm0, (%0)
            movq %%mm4, (%3,%2,8)
            movq %%mm2, (%0,%2,8)
            movq %%mm3, (%3)

.align 16
loop%=:
            add $8, %3
            movq (%3), %%mm1
            add $8, %1
            movq (%1), %%mm4
            pswapd %%mm4, %%mm0
            add $8, %4
            add $8, %0

            movq (%4), %%mm5
            movq (%0,%2,8), %%mm2
            movq (%3,%2,8), %%mm3


            pfmul %%mm1, %%mm4
            pfmul %%mm0, %%mm1
            movq (%4,%2,4), %%mm6
            pswapd %%mm5, %%mm7
            pswapd %%mm6, %%mm0
            pfmul %%mm2, %%mm5
            pfmul %%mm7, %%mm2
            pfmul %%mm3, %%mm6
            pfmul %%mm0, %%mm3
            pfpnacc %%mm1, %%mm4
            pfpnacc %%mm2, %%mm5

            pfpnacc %%mm3, %%mm6

            movq (%0), %%mm0
            movq %5, %%mm7

            //1-4   2-5   3-6
            movq %%mm0, %%mm1    ;//x0
            movq %%mm4, %%mm2    ;//x1
            pfadd %%mm5, %%mm0   ;//es
            pfadd %%mm6, %%mm4   ;//os
            pfsub %%mm5, %%mm1   ;//ed
            pfsub %%mm6, %%mm2   ;//od
            pswapd %%mm2, %%mm2  ;//od'
            pxor %%mm7, %%mm2    ;//od'


            pfadd %%mm4, %%mm0   ;//x0
            pfsub %%mm4, %%mm5   ;//x2
            pfadd %%mm2, %%mm1   ;//x3
            pfsub %%mm2, %%mm6   ;//x1
            movq %%mm0, (%0)
            movq %%mm5, (%0,%2,8)
            movq %%mm1, (%3,%2,8)
            movq %%mm6, (%3)

            dec %6
            jne loop%=
            pop %6
            pop %1
            pop %0
            "
		  : : "r" (x), "r" (w), "q" (N2), "q" (x+N4), "q" (w+N2), "m" (mask), "q" (N4-1)
		  : "memory", "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)");
	    }
	    x+=N;

	 }
      
   }
  


/*
if (0&&M&1)
   {
      //recurs_fft(x, M-1, w+N, repeat<<1);
      
      while (repeat--)
      {
	 __asm__ __volatile__ (
      "
      push %0
      push %1
      push %2
      push %3
.loop%=:
      movq  (%0),  %%mm0
      movq  8(%0),  %%mm4
      pswapd %%mm0, %%mm2
      pswapd %%mm4, %%mm6
      movq  (%1),  %%mm1
      movq  8(%1), %%mm5
      pfmul %%mm1, %%mm0
      pfmul %%mm1, %%mm2
      pfmul %%mm5, %%mm4
      pfmul %%mm5, %%mm6
      pfpnacc %%mm2, %%mm0
      pfpnacc %%mm6, %%mm4
      movq  (%2),  %%mm3
      movq  8(%2), %%mm7
      movq  %%mm3, %%mm1
      movq  %%mm7, %%mm5
      pfsub %%mm0, %%mm3
      pfadd %%mm0, %%mm1
      pfsub %%mm4, %%mm7
      pfadd %%mm4, %%mm5
      movq  %%mm1, (%2)
      movq  %%mm5, 8(%2)
      add $16, %1
      add $16, %2
      movq  %%mm3, (%0)
      movq  %%mm7, 8(%0)
      add $16, %0
      dec %3
      jne .loop%=
      pop %3
      pop %2
      pop %1
      pop %0
      "
      : "=r" (dummy1), "=r" (dummy2), "=r" (dummy3), "=q" (dummy4) : "0" (x+N2), "1" (w), "2" (x), "3" (N2>>1)
      : "memory", "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)");

      }
   }*/




   __asm__ __volatile__ ("femms" : : : "memory", "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)");
}


#endif
