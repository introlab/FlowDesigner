#ifndef _THREADED_ITERATOR_CC_
#define _THREADED_ITERATOR_CC_

#include "ThreadedIterator.h"
#include <pthread.h>

const int ThreadedIterator::STATUS_RUNNING = 1;
const int ThreadedIterator::STATUS_STOPPED = 0;


ThreadedIterator::ThreadedIterator (string nodeName, ParameterSet params) 
   : Iterator(nodeName, params)
  , rate_per_second (parameters.get("RATE_PER_SECOND"))
  , internal_pc(0)
  , status(STATUS_STOPPED)  {  

  if (rate_per_second <=0 ) {
    throw NodeException (this, "RATE_PER_SECOND IN THREADED ITERATOR MUST BE GREATER THAN ZERO.",__FILE__,__LINE__);
  }
  
  my_thread = NULL;

  pthread_mutex_init(&mutex, NULL);
}

ObjectRef ThreadedIterator::getOutput (int output_id, int count) {
   
   if (!hasOutput(output_id)) throw NodeException (this, "Cannot getOutput id",__FILE__,__LINE__);

   iterator_lock();

   ObjectRef output;

   if (processCount != count) {

      try {
         
	//We are doing a little trick for the translator (real inputNode)
	//if it exists
	if (translator) {
	  translator->setProcessCount(count);
	}
	        
	output = sinkNode->getOutput(output_id,internal_pc);
       
      }
      catch (GenericCastException &e) {
         //We had a problem casting, our inputs are invalid?
         e.print();
         output = ObjectRef(new Object(Object::nil));         
      }      
      catch (BaseException &e) {
         //Something weird happened
         e.print();
         throw NodeException (this,string("Error in ThreadedIterator::getOutput"), __FILE__,__LINE__);
      }
      processCount = count;
   }

   iterator_unlock();

   return output;   
}

void ThreadedIterator::loop() {
  
  while (1) {

    iterator_lock();
  
    if (status != ThreadedIterator::STATUS_RUNNING) break;
    
    try {

      // updating all outputs
      sinkNode->getOutput(output_id,internal_pc++);
    
    }
    catch (GenericCastException &e) {
      //We had a problem casting, our inputs are invalid?
      e.print();
      throw NodeException (this,string("Error in ThreadedIterator::getOutput loop"), __FILE__,__LINE__);
    }      
    catch (BaseException &e) {
      //Something weird happened
      e.print();
      throw NodeException (this,string("Error in ThreadedIterator::getOutput loop"), __FILE__,__LINE__);
    }

    iterator_unlock();

  }
  
  cout<<"Exiting ThreadedIterator loop"<<endl;
  
}


void ThreadedIterator::reset() {
  
  stop_thread();
  
  //resetting every nodes
  this->Iterator::reset();

  internal_pc = 0;

}

void ThreadedIterator::start_thread() {


}

void ThreadedIterator::stop_thread() {

  iterator_lock();

  status = ThreadedIterator::STATUS_STOPPED;

  iterator_unlock();

  //wait for the thread

  void **my_status;

  pthread_join (my_thread,my_status);


}

#endif
