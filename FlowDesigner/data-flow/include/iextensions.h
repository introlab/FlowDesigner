// Copyright (C) 2001 InfoSpace Speech Solutions
// author: Jean-Marc Valin

#ifndef IEXTENSIONS_H
#define IEXTENSIONS_H

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



#if defined(_ALLOW_SSE) || defined(_ALLOW_3DNOW)

#ifdef __GNUC__

inline void prefetchnta(void *ptr)
{
   if (IExtension::have3DNow() || IExtension::haveSSE())
      __asm__ __volatile__ ("prefetchnta (%0)" : : "r" (ptr));
}

inline void prefetcht0(void *ptr)
{
   if (IExtension::have3DNow() || IExtension::haveSSE())
      __asm__ __volatile__ ("prefetcht0 (%0)" : : "r" (ptr));
}

inline void prefetcht1(void *ptr)
{
   if (IExtension::have3DNow() || IExtension::haveSSE())
      __asm__ __volatile__ ("prefetcht1 (%0)" : : "r" (ptr));
}

inline void prefetcht2(void *ptr)
{
   if (IExtension::have3DNow() || IExtension::haveSSE())
      __asm__ __volatile__ ("prefetcht2 (%0)" : : "r" (ptr));
}


inline void emms()
{
   if (IExtension::have3DNow())
   {
      __asm__ __volatile__ ("femms"
         : :
         : "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)");
   } else if (IExtension::haveSSE())
   {
      __asm__ __volatile__ ("emms"
         : :
         : "st", "st(1)", "st(2)", "st(3)", "st(4)", "st(5)", "st(6)", "st(7)");
   }
}

#endif /*__GUNC__*/

#ifdef WIN32
inline void prefetchnta(void *ptr) {}
inline void prefetch0(void *ptr) {}
inline void prefetch1(void *ptr) {}
inline void prefetch2(void *ptr) {}
inline void emms() {}
#endif /*WIN32*/

#else

inline void prefetchnta(void *ptr) {}
inline void prefetch0(void *ptr) {}
inline void prefetch1(void *ptr) {}
inline void prefetch2(void *ptr) {}
inline void emms() {}

#endif /*defined(_ALLOW_SSE) || defined(_ALLOW_3DNOW)*/




#endif /*IEXTENSIONS_H*/
