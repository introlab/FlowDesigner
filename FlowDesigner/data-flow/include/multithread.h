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
      __asm__ __volatile__ ("lock; add $1, %0" : : "m" (count) : "memory");
   }
   bool dec()
   {
      int tmp;
      __asm__ __volatile__ ("lock; sub $1, %1\n\tmov $0, %0\n\tjle out%=\n\tmov $1, %0\n\tout%=:" : "=r" (tmp): "m" (count) : "memory");
      return tmp;
   }
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

#endif /*defined(_ENABLE_X86_ASM)*/

#else /*ifdef MULTITHREAD*/

class AtomicCounter {
   int count;
public:
   AtomicCounter() : count (0) {}
   void inc() {++count;}
   bool dec() {--count;return (count > 0);}
};

#endif /*ifdef MULTITHREAD*/



#endif /*ifndef MULTITHREAD_H*/
