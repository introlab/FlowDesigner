#ifndef _THREADED_ITERATOR_H_
#define _THREADED_ITERATOR_H_

/**
   The ThreadedIterator class is intended to compute at a defined Rate per second (RPS) 
   its output. It is useful for async computing.
   @author Dominic Letourneau, Jonathan Audet
*/

#include "Iterator.h"
#include <pthread.h>

class ThreadedIterator : public Iterator {

 public:

  /** The constructor with a nodeName and parameters */
  ThreadedIterator (string nodeName, ParameterSet params);
  
  /** The getOutput method overloaded from Node */
  virtual ObjectRef getOutput (int output_id, int count);

  /** Locking the thread */
  void iterator_lock (){pthread_mutex_lock(&mutex);}

  /** Unlocking the thread */
  void iterator_unlock(){pthread_mutex_unlock(&mutex);}
  
  /**Resets the node internal values and buffers*/
  virtual void reset();

  /** Starts the working thread */
  void start_thread();

  /** Stops the working thread */
  void stop_thread();

  /** Destructor */
  ~ThreadedIterator () {

    if (status != STATUS_STOPPED) {
      cout<<"Stopping the Iterator thread...";
      stop_thread();
      cout<<"Done!"<<endl;
    }

    //destroying the mutex
    pthread_mutex_destroy(mutex);
  }

 private:
  
  /** Default constructor that should not be used*/
  ThreadedIterator() {
    throw NodeException (NULL,"The default constructor should not be called from ThreadedIterator",__FILE__,__LINE__);
  }
   
  void loop();

  unsigned int rate_per_second;

  int internal_pc;

  int status;

  static const int STATUS_RUNNING;

  static const int STATUS_STOPPED;

  pthread_mutex_t mutex;

  pthread_t my_thread;

};
#endif