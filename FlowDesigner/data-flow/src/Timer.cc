#include "Timer.h"
#include <errno.h>	//ETIMEDOUT, EINTR, EBUSY
#include <sys/time.h>	//gettimeofday()
#include <assert.h>

using namespace std;

namespace FD {

//(ER) for debugging
timespec startTime;


void* timer_thread (void * timerPtr) {

  Timer* my_timer = reinterpret_cast<Timer*>(timerPtr);

  assert(my_timer);

  cerr<<"timer start"<<endl;

  while (1) {
  
    my_timer->lock();

    long remaining_time = my_timer->m_remaining_time;

    my_timer->unlock();

    //sleeping 100 ms
    if (remaining_time >= 100) {
      usleep(100000);
    }
    else {
      usleep(remaining_time * 1000);
    }

    my_timer->lock();

    my_timer->m_remaining_time = max(0, remaining_time - 100);
  
    remaining_time = my_timer->m_remaining_time;

    my_timer->unlock();

    //cerr<<"remaining time "<<remaining_time<<endl;

    if (remaining_time <= 0) {
      cerr<<"timer done!"<<endl;
      return NULL;
    }
  }
}



// TIMING functions

//The definition for timespec structure, from time.h
/* POSIX.1b structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  */
//struct timespec
//  {
//    __time_t tv_sec;		/* Seconds.  */
//    long int tv_nsec;		/* Nanoseconds.  */
//  };

#define MILLION 1000000L
#define BILLION 1000000000L

// Puts current system time in the timespec structure
void vGetSystemTime( timespec* a_ptNow )
{
   struct timeval l_tNow;
   gettimeofday( &l_tNow, NULL );
   a_ptNow->tv_sec = l_tNow.tv_sec;
   a_ptNow->tv_nsec = l_tNow.tv_usec * 1000;
}

// Returns the result of ac_ptFinal - ac_ptInitial in milliseconds
long lMsBetweenTimes( timespec const* ac_ptInitial, timespec const* ac_ptFinal )
{
   long l_lReturnVal;
   l_lReturnVal = (((long)ac_ptFinal->tv_sec - (long)ac_ptInitial->tv_sec) * 1000 +
		   ((ac_ptFinal->tv_nsec - ac_ptInitial->tv_nsec) / MILLION));

   return l_lReturnVal;
}

// Returns the number of milliseconds remaining until 'a_ptFinal' time
long lMsRemainingUntil( timespec const* ac_ptFinal )
{
  struct timespec l_tNow;
  vGetSystemTime( &l_tNow );
  return lMsBetweenTimes( &l_tNow, ac_ptFinal );
}

// Returns the number of milliseconds elapsed since 'a_ptInitial' time
long lMsElapsedSince( timespec const* ac_ptInitial )
{
  struct timespec l_tCurrent;
  vGetSystemTime( &l_tCurrent );
  return lMsBetweenTimes( ac_ptInitial, &l_tCurrent );
}

// Adds 'a_ulNbrMs' milliseconds to 'a_ptInitial' time
void vAddMsToTime( unsigned long a_ulNbrMs, timespec* a_ptInitial )
{
  if( a_ulNbrMs > 1000 )
  {
    a_ptInitial->tv_sec += a_ulNbrMs / 1000;
    a_ulNbrMs %= 1000;
  }

  a_ptInitial->tv_nsec += (a_ulNbrMs * MILLION);

  if( a_ptInitial->tv_nsec >= BILLION )
  {
    a_ptInitial->tv_nsec -= BILLION;
    a_ptInitial->tv_sec += 1;
  }
  //NOTE : No overflow test done on tv_sec
}

//BE CAREFULL: The following functions should only be used to calculate
//	relatively short periods of time (max ~30 min). Otherwise, there will
//	be overflow.
// Returns the result of ac_ptFinal - ac_ptInitial in microseconds
long lUsBetweenTimes( timespec const* ac_ptInitial, timespec const* ac_ptFinal )
{
   long l_lReturnVal;
   l_lReturnVal = (((long)ac_ptFinal->tv_sec - (long)ac_ptInitial->tv_sec) * MILLION +
		   ((ac_ptFinal->tv_nsec - ac_ptInitial->tv_nsec) / 1000));

   return l_lReturnVal;
}

// Returns the number of microseconds remaining until 'a_ptFinal' time
long lUsRemainingUntil( timespec const* ac_ptFinal )
{
  struct timespec l_tNow;
  vGetSystemTime( &l_tNow );
  return lUsBetweenTimes( &l_tNow, ac_ptFinal );
}

// Returns the number of microseconds elapsed since 'a_ptInitial' time
long lUsElapsedSince( timespec const* ac_ptInitial )
{
  struct timespec l_tCurrent;
  vGetSystemTime( &l_tCurrent );
  return lUsBetweenTimes( ac_ptInitial, &l_tCurrent );
}
// Adds 'a_ulNbrUs' microseconds to 'a_ptInitial' time
void vAddUsToTime( unsigned long a_ulNbrUs, timespec* a_ptInitial )
{
  if( a_ulNbrUs > MILLION )
  {
    a_ptInitial->tv_sec += a_ulNbrUs / MILLION;
    a_ulNbrUs %= MILLION;
  }

  a_ptInitial->tv_nsec += (a_ulNbrUs * 1000);

  if( a_ptInitial->tv_nsec >= BILLION )
  {
    a_ptInitial->tv_nsec -= BILLION;
    a_ptInitial->tv_sec += 1;
  }
  //NOTE : No overflow test done on tv_sec
}




//Makes the current thread sleep for "a_ulNbrUs" microseconds
void thread_usleep( unsigned long a_ulNbrUs )
{
  //Since there's no really "sleep" function for threads, we use a faked
  //	condition variable on which we wait only for a specified period of time
  pthread_mutex_t l_tMutex = PTHREAD_MUTEX_INITIALIZER;
  pthread_cond_t l_tCondVar = PTHREAD_COND_INITIALIZER;

  timespec l_tWakeUpTime, l_tBedTime;
  vGetSystemTime( &l_tWakeUpTime );
  l_tBedTime.tv_sec = l_tWakeUpTime.tv_sec;
  l_tBedTime.tv_nsec = l_tWakeUpTime.tv_nsec;
  vAddUsToTime( a_ulNbrUs, &l_tWakeUpTime );

  pthread_mutex_lock( &l_tMutex );
//  cerr << "Calling 'pthread_cond_timedwait()' for " << a_ulNbrUs << "us at " << lUsElapsedSince( &startTime ) << " us." << endl;
  switch( pthread_cond_timedwait( &l_tCondVar, &l_tMutex, &l_tWakeUpTime ) )
  {
  case 0:
    break;
  case ETIMEDOUT:
//    cerr << "As expected, there has been a time out." << endl;
    break;
  case EINTR:
    cerr << "We had been interrupted by a signal ???" << endl;
    break;
  }
//  cerr << "Woken up at " << lUsElapsedSince( &startTime ) << " us (sleep of " << lUsElapsedSince( &l_tBedTime ) << "us)." << endl;
  
  pthread_mutex_unlock( &l_tMutex );
  switch( pthread_cond_destroy( &l_tCondVar ) )
  {
  case 0:
    break;
  case EBUSY:
    cerr << "Can't destroy condition variable: some threads are waiting on it." << endl;
    break;
  }
  switch( pthread_mutex_destroy( &l_tMutex ) )
  {
  case 0:
    break;
  case EBUSY:
    cerr << "Can't destroy mutex: it's currently locked." << endl;
    break;
  }

}

}//namespace FD

