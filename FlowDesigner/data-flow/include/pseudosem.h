// Copyright (C) 2001 Jean-Marc Valin

#include <pthread.h>

typedef struct pseudosem_t {
   pthread_mutex_t mutex;
   pthread_cond_t cond;
   int val;
} pseudosem_t;

inline void pseudosem_init(pseudosem_t *sem, int dummy, int _val)
{
   sem->val = _val;
   pthread_mutex_init(&sem->mutex, NULL);
   pthread_cond_init(&sem->cond, NULL);
}

inline void pseudosem_destroy(pseudosem_t *sem)
{
   pthread_mutex_destroy(&sem->mutex);
   pthread_cond_destroy(&sem->cond);
}

inline void pseudosem_wait(pseudosem_t *sem)
{
   pthread_mutex_lock(&sem->mutex);
   if (!sem->val)
   {
      cerr << "waiting\n";
      pthread_cond_wait(&sem->cond, &sem->mutex);
      cerr << "end waiting\n";
   }
   sem->val--;
   pthread_mutex_unlock(&sem->mutex);
}

inline void pseudosem_post(pseudosem_t *sem)
{
   cerr << "locking for post\n";
   pthread_mutex_lock(&sem->mutex);
   cerr << "locked for post\n";
   pthread_cond_signal(&sem->cond);
   sem->val++;
   pthread_mutex_unlock(&sem->mutex);
}

#ifdef HAVE_SEMAPHORE_H
#include <semaphore.h>
#define pseudosem_t sem_t
#define pseudosem_post sem_post
#define pseudosem_wait sem_wait
#define pseudosem_init sem_init
#define pseudosem_destroy sem_destroy
#endif
