// Copyright (C) 2003 Jean-Marc Valin

#ifndef MULTITHREAD_H
#define MULTITHREAD_H

#ifdef MULTITHREAD

#include <pthread.h>

#if defined(_ENABLE_X86_ASM)

class AtomicCounter {
   int count;
public:
   AtomicCounter() : count(0) {}
   void inc() 
   {
      __asm__ __volatile__ ("lock; add $1, %0" 
                            : : "m" (count) : "memory");
   }
   bool dec()
   {
      int tmp;
      __asm__ __volatile__ ("lock; sub $1, %1\n\tmov $0, %0\n\tjle out%=\n\tmov $1, %0\n\tout%=:" 
                            : "=r" (tmp): "m" (count) : "memory");
      return tmp;
   }
};


class FastMutex {
   int spin;
public:
   FastMutex() {spin=1;}
   void lock()
   {
      int tmp=0;
      __asm__ __volatile__ ("spin%=:\n\txchg (%%eax), %0\n\tcmp $0, %0\n\tje spin%="
                            : "=r" (tmp) : "0" (tmp), "a" (&spin) : "memory");

   }
   void unlock() {spin=1;}
};


#else /*defined(_ENABLE_X86_ASM)*/

class AtomicCounter {
   int count;
   pthread_mutex_t mutex;
public:
   AtomicCounter() : count(0) {pthread_mutex_init(&mutex, NULL);}
   ~AtomicCounter() {pthread_mutex_destroy(&mutex);}

   void inc() 
   {
      pthread_mutex_lock(&mutex);
      ++count;
      pthread_mutex_unlock(&mutex);
   }

   bool dec() 
   {
      int tmp;
      pthread_mutex_lock(&mutex);
      --count;
      tmp=count;
      pthread_mutex_unlock(&mutex);
      return (tmp > 0);
   }
};


class FastMutex {
   pthread_mutex_t mutex;
public:
   FastMutex() {pthread_mutex_init(&mutex, NULL);}
   ~FastMutex() {pthread_mutex_destroy(&mutex);}
   void lock() {pthread_mutex_lock(&mutex);}
   void unlock() {pthread_mutex_unlock(&mutex);}
};



#endif /*defined(_ENABLE_X86_ASM)*/

#else /*ifdef MULTITHREAD*/

class AtomicCounter {
   int count;
public:
   AtomicCounter() : count (0) {}
   void inc() {++count;}
   bool dec() {--count;return (count > 0);}
};


class FastMutex {
public:
   void lock() {}
   void unlock() {}
};

#endif /*ifdef MULTITHREAD*/



#endif /*ifndef MULTITHREAD_H*/
