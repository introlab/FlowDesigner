// Copyright (C) 2001 Jean-Marc Valin

#ifndef SYNC_H
#define SYNC_H

#ifdef __i386__
typedef int spinlock_t; 

inline void spinlock_init(spinlock_t *spin)
{
   (*spin)=1;
}

inline void spinlock_lock(spinlock_t *spin)
{
   __asm__ __volatile__ (
   "
   pushl %%ebx
spin%=:
   xchg (%%eax), %%ebx
   cmp $0, %%ebx
   je spin%=
   popl %%ebx
   sfence
   "
   : : "a" (spin), "b" (int(0))
   : "memory"
   );
}

inline void spinlock_unlock(spinlock_t *spin)
{
   *spin=1;
}
#else

#include <pthread.h>

typedef pthread_mutex_t spinlock_t;

#define spinlock_init pthread_mutex_init

#define spinlock_lock pthread_mutex_lock

#define spinlock_unlock pthread_mutex_unlock

#endif /*ifdef __i386__*/


#endif /*ifndef SYNC_H*/
