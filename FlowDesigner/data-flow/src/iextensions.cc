// Copyright (C) 2001 InfoSpace Speech Solutions
// author: Jean-Marc Valin


/* 
   These routines perform automatic detection of the SSE and 3DNow! extensions.
   The first way of doing that is to use the "cpuid" instructions and look for 
   the right bits. The other way is to attempt an SSE or 3DNow! instruction
   and check for an illegal instruction. While, the first method looks cleaner,
   the second method is prefered here because it is possible for the CPU to
   support SSE, while the OS doesn't support it. In this case, by only checking 
   the cpuid instruction, you think you can use SSE, while in fact you can't.
   
   SSE (SIMD Streaming Extensions) requires at least Linux 2.4 or Win NT SP6 
   (I don't know about millenium/2000/XP). Is is not yet supported on Solaris/x86.
   To compile C/C++ code with SSE assembly on MSVC++, you need the compiler 
   "processor pack". A recent version of binutils (2.10 I think) is required 
   under Linux (that should not be a problem).

   3DNow! does not require OS support, as it is only an extension to MMX.   
*/

#include "iextensions.h"
#include <iostream>

bool IExtensions::isse=false;
bool IExtensions::i3dnow=false;


void IExtensions::detect()
{
   detectSSE();
   detect3DNow();
}


#ifdef __GNUC__

#if defined (_ALLOW_SSE) || defined (_ALLOW_3DNOW)

#include <setjmp.h>
#include <signal.h>

static jmp_buf env;

/*Catches illegal instructions for SSE/3DNow! checks*/
static void illegal_inst(int sig)
{
   signal (SIGILL, SIG_DFL);
   longjmp(env,1);
}
#endif

#ifdef _ALLOW_SSE
#define CHECK_SSE
void IExtensions::detectSSE()
{
   if (setjmp(env))
   {
      cerr << "SSE not detected\n";
      isse=false;
      return;
   }
   signal (SIGILL, illegal_inst);
   __asm__ __volatile__ (
      "
      xorps %%xmm0, %%xmm0 "
      : :
      : "memory"
      );
   signal (SIGILL, SIG_DFL);
   isse=true;
   cerr << "SSE detected\n";
}
#endif /*_ALLOW_SSE*/

#ifdef _ALLOW_3DNOW
#define CHECK_3DNOW
void IExtensions::detect3DNow()
{
   if (setjmp(env))
   {
      cerr << "3DNow not detected\n";
      i3dnow=false;
      return;
   }
   signal (SIGILL, illegal_inst);
   __asm__ __volatile__ ("
        pfadd %%mm7, %%mm7
        femms
        ": : 
	: "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)", "memory"
        );
   signal (SIGILL, SIG_DFL);
   i3dnow=true;
   cerr << "3DNow detected\n";
}

#endif

#elif defined (WIN32)

/* SSE detection (and hence 3DNow! too) taken from example in Intel's guide*/

#ifdef _ALLOW_SSE
#define CHECK_SSE
void IExtensions::detectSSE()
{
   _try {
      _asm {
	 
	 xorps xmm0, xmm0
	    
	    }
   }
   _except(EXCEPTION_EXECUTE_HANDLER)
      {
	 //fprintf(stdout, "In catch\n");
	 if (_exception_code()==STATUS_ILLEGAL_INSTRUCTION)
	 {
	    cerr << "SSE detected\n";
	    isse=false;
	    return
	 }
      }
   cerr << "SSE notdetected\n";
   isse=true;
}
#endif


#ifdef _ALLOW_3DNOW
#define CHECK_3DNOW
void IExtensions::detect3DNow()
{
   _try {
      _asm {
	 
	 pfadd mm7 mm7
	 femms
	    
	    }
   }
   _except(EXCEPTION_EXECUTE_HANDLER)
      {
	 //fprintf(stdout, "In catch\n");
	 if (_exception_code()==STATUS_ILLEGAL_INSTRUCTION)
	 {
	    cerr << "3DNow! detected\n";
	    i3dnow=false;
	    return
	 }
      }
   cerr << "3DNow! notdetected\n";
   i3dnow=true;

}
#endif

#else /*WIN32*/


#if (defined (_ALLOW_SSE) || defined (_ALLOW_3DNOW))
#warning "SSE and 3DNow! only supported on GCC and MSVC++, help us get more support!"
#endif

#endif


#ifndef CHECK_SSE
void IExtensions::detectSSE()
{
   isse=false;
}
#endif /*CHECK_SSE*/

#ifndef CHECK_3DNOW

void IExtensions::detect3DNow()
{
   i3dnow=false;
}

#endif /*CHECK_3DNOW*/
