// Copyright (C) 2001 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

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
      pthread_cond_wait(&sem->cond, &sem->mutex);
   sem->val--;
   pthread_mutex_unlock(&sem->mutex);
}

inline void pseudosem_post(pseudosem_t *sem)
{
   pthread_mutex_lock(&sem->mutex);
   pthread_cond_signal(&sem->cond);
   sem->val++;
   pthread_mutex_unlock(&sem->mutex);
}
