#ifndef _TIMER_H_
#define _TIMER_H_

#include "Object.h"
#include "misc.h"
#include <list>
#include <unistd.h>
#include <sys/time.h>
#include <unistd.h>
#include <pthread.h>

namespace FD {
	

void* timer_thread (void * timerPtr);

//(ER) for debugging
extern timespec startTime;

class Timer : public FD::Object {

  friend void* timer_thread (void* timerPtr);

 private:  

  pthread_t m_thread;
  pthread_mutex_t m_mutex;
  long m_remaining_time;

  void lock () {
    pthread_mutex_lock(&m_mutex);
  }
  
  void unlock() {
    pthread_mutex_unlock(&m_mutex);
  }

 public:
  
  Timer(long msec) {
    m_remaining_time = msec;
    pthread_mutex_init(&m_mutex, NULL); 
  }
  
  ~Timer() {
    stop();   
  }

  void start() {

    lock();
    pthread_create(&m_thread, NULL,timer_thread, this);
    unlock();
  }
  
  void stop() {

    void *return_value;

    lock();
    m_remaining_time = 0;
    unlock();

    pthread_join(m_thread,&return_value);
  }

  bool ready() {
    lock();
    bool is_ready = (m_remaining_time == 0);
    unlock();
    return is_ready;
  }

  void reset (long msec) {

    //usec
    stop();
    m_remaining_time = msec;
    start();
  }

  virtual void printOn(std::ostream &out=std::cout) const {
    out<<"Timer : "<<m_remaining_time<<" msec"<<std::endl;
  }

};


// TIMING functions

//The definition for timespec structure, from time.h
/* POSIX.1b structure for a time value.  This is like a `struct timeval' but
   has nanoseconds instead of microseconds.  */
//struct timespec
//  {
//    __time_t tv_sec;		/* Seconds.  */
//    long int tv_nsec;		/* Nanoseconds.  */
//  };

// Puts current system time in the timespec structure
void vGetSystemTime( timespec* a_ptNow );
// Returns the result of ac_ptFinal - ac_ptInitial in milliseconds
long lMsBetweenTimes( timespec const* ac_ptInitial, timespec const* ac_ptFinal );
// Returns the number of milliseconds remaining until 'a_ptFinal' time
long lMsRemainingUntil( timespec const* ac_ptFinal );
// Returns the number of milliseconds elapsed since 'a_ptInitial' time
long lMsElapsedSince( timespec const* ac_ptInitial );
// Adds 'a_ulNbrMs' milliseconds to 'a_ptInitial' time
void vAddMsToTime( unsigned long a_ulNbrMs, timespec* a_ptInitial );

//BE CAREFULL: The following functions should only be used to calculate
//	relatively short periods of time (max ~30 min). Otherwise, there will
//	be overflow.
// Returns the result of ac_ptFinal - ac_ptInitial in microseconds
long lUsBetweenTimes( timespec const* ac_ptInitial, timespec const* ac_ptFinal );
// Returns the number of microseconds remaining until 'a_ptFinal' time
long lUsRemainingUntil( timespec const* ac_ptFinal );
// Returns the number of microseconds elapsed since 'a_ptInitial' time
long lUsElapsedSince( timespec const* ac_ptInitial );
// Adds 'a_ulNbrUs' microseconds to 'a_ptInitial' time
void vAddUsToTime( unsigned long a_ulNbrUs, timespec* a_ptInitial );

//Makes the current thread sleep for "a_ulNbrUs" microseconds
void thread_usleep( unsigned long a_ulNbrUs );

}//namespace FD

#endif

