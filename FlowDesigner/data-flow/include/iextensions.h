// Copyright (C) 2001 InfoSpace Speech Solutions
// author: Jean-Marc Valin

#ifndef IEXTENSIONS_H
#define IEXTENSIONS_H

namespace FD {

class IExtensions {
   static bool isse;
   static bool i3dnow;
  public:
   static void detect();
   static void detectSSE();
   static void detect3DNow();
   static inline bool haveSSE() {return isse;}
   static inline bool have3DNow() {return i3dnow;}
   
};



#if defined(_ENABLE_SSE) || defined(_ENABLE_3DNOW)

#ifdef __GNUC__

inline void prefetchnta(void *ptr)
{
   if (IExtensions::have3DNow() || IExtensions::haveSSE())
      __asm__ __volatile__ ("prefetchnta (%0)" : : "r" (ptr));
}

inline void prefetcht0(void *ptr)
{
   if (IExtensions::have3DNow() || IExtensions::haveSSE())
      __asm__ __volatile__ ("prefetcht0 (%0)" : : "r" (ptr));
}

inline void prefetcht1(void *ptr)
{
   if (IExtensions::have3DNow() || IExtensions::haveSSE())
      __asm__ __volatile__ ("prefetcht1 (%0)" : : "r" (ptr));
}

inline void prefetcht2(void *ptr)
{
   if (IExtensions::have3DNow() || IExtensions::haveSSE())
      __asm__ __volatile__ ("prefetcht2 (%0)" : : "r" (ptr));
}


inline void emms()
{
   if (IExtensions::have3DNow())
   {
      __asm__ __volatile__ ("femms"
         : :
         : "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)");
   } else if (IExtensions::haveSSE())
   {
      __asm__ __volatile__ ("emms"
         : :
         : "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)");
   }
}

#endif /*__GUNC__*/

#ifdef WIN32


//FIXME: Until I understand how to use the prefetch* instructions under VC++
inline void prefetchnta(void *ptr) {}
inline void prefetcht0(void *ptr) {}
inline void prefetcht1(void *ptr) {}
inline void prefetcht2(void *ptr) {}
inline void emms() {}


#endif /*WIN32*/

#else

inline void prefetchnta(void *ptr) {}
inline void prefetcht0(void *ptr) {}
inline void prefetcht1(void *ptr) {}
inline void prefetcht2(void *ptr) {}
inline void emms() {}

#endif /*defined(_ENABLE_SSE) || defined(_ENABLE_3DNOW)*/


}//namespace FD

#endif /*IEXTENSIONS_H*/
