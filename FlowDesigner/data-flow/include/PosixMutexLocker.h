#ifndef _POSIX_MUTEX_LOCKER_H_
#define _POSIX_MUTEX_LOCKER_H_

#ifdef WIN32

#include <QMutex>
#include <QMutexLocker>

typedef QMutexLocker PosixMutexLocker;

#else

#include <pthread.h>

namespace FD
{

	class PosixMutexLocker
	{
		public:

		PosixMutexLocker(pthread_mutex_t *mutex)
		: m_mutex(mutex)
		{
			if (m_mutex)
			{
				pthread_mutex_lock(m_mutex);
			}
		}
		
		~PosixMutexLocker()
		{
			if (m_mutex)
			{
				pthread_mutex_unlock(m_mutex);
			}
		}
		
		private:
		
		pthread_mutex_t *m_mutex;
	};

}	

#endif
	
#endif

