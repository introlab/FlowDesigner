// Copyright (C) 2001 Jean-Marc Valin

#ifndef _THREADED_ITERATOR_CC_
#define _THREADED_ITERATOR_CC_

#include "ThreadedIterator.h"
#include <pthread.h>
#include <time.h>
#include <unistd.h>

using namespace std;

namespace FD {

const int ThreadedIterator::STATUS_RUNNING = 1;
const int ThreadedIterator::STATUS_STOPPED = 0;


ThreadedIterator::ThreadedIterator (string nodeName, ParameterSet params) 
   : Iterator(nodeName, params)
  , m_in_getOutput(false)
  , internal_pc(0)
  , thread_status(STATUS_STOPPED)
{  

  try {
    rate_per_second = dereference_cast<int>(parameters.get("RATE_PER_SECOND"));
    //rate_per_second = 1;
    cout<<"ThreadedIterator constructor..."<<endl;
    
    if (rate_per_second <=0 ) {
      throw new NodeException (this, "RATE_PER_SECOND IN THREADED ITERATOR MUST BE GREATER THAN ZERO.",__FILE__,__LINE__);
    }
    
    
    pthread_mutex_init(& mutex, NULL);
    

  }    
  catch (BaseException *e) {
    //Something weird happened
    //e->print();
    throw e->add(new NodeException (this,string("Error in ThreadedIterator constructor"), __FILE__,__LINE__));
  }
  
  
}

ObjectRef ThreadedIterator::getOutput (int output_id, int count) {

  if (!hasOutput(output_id)) throw new NodeException (this, "Cannot getOutput id",__FILE__,__LINE__);
  
  m_in_getOutput = true;
  
   iterator_lock();
   
   //if (thread_status == ThreadedIterator::STATUS_STOPPED) {
     //We must start the thread
     //start_thread();
   //}

   //cerr<<"getOutput begin!"<<endl;

   //ObjectRef output;

   if (processCount != count) {

      try {
         
	//We are doing a little trick for the translator (real inputNode)
	//if it exists
	if (translator) {
	  translator->setProcessCount(count);
	}
	int out_id=0;
	while (sinkNode->hasOutput(out_id))
	{
	   output[out_id] = sinkNode->getOutput(output_id,internal_pc);
	   out_id++;
	}
       
      }     
      catch (BaseException *e) {
         //Something weird happened
         //e->print();
         throw e->add(new NodeException (this,string("Error in ThreadedIterator::getOutput"), __FILE__,__LINE__));
      }
      processCount = count;
   }

   //cerr<<"getOutput end!"<<endl;
   iterator_unlock();

    m_in_getOutput = false;

   return output[output_id];   
}




void ThreadedIterator::reset() {
  
  stop_thread();
  
  //resetting every nodes
  this->Iterator::reset();

  internal_pc = 0;

}

void ThreadedIterator::start_thread() {

  cerr<<"start_thread"<<endl;

  internal_pc = 0;

  thread_status = ThreadedIterator::STATUS_RUNNING;
  
  pthread_create(&work_thread, NULL, &workloop, this); 
  

}

void ThreadedIterator::stop_thread() {

  cerr<<"stop_thread"<<endl;

  if (thread_status == STATUS_RUNNING) {

    cerr<<"Setting the stop status"<<endl;
    thread_status = ThreadedIterator::STATUS_STOPPED;

    //to avoid any deadlocks
    iterator_unlock();
    cerr<<"Threaded iterator should stop thread here... but it doesn't."<<endl;
    
  }
  else {
    thread_status = ThreadedIterator::STATUS_STOPPED;
  }

  cerr<<"end stop thread."<<endl;
}

void ThreadedIterator::initialize() {

  start_thread();
  this->Network::initialize();
   
}

void * FD::workloop (void *param) {
  
  if (param == NULL) {
    throw new NodeException (NULL,string("Error in ThreadedIterator::getOutput workloop: NULL param."), __FILE__,__LINE__);
  }

  ThreadedIterator *ptr = (ThreadedIterator *) param;

  cerr<<"Starting the workloop."<<endl;

  while (ptr->thread_status == ThreadedIterator::STATUS_RUNNING) {

    cerr<<"status : "<<ptr->thread_status<<endl;
    ptr->iterator_lock();
  
    time_t begin =  time(NULL);
    
    try {

      // updating all outputs
      for (int i =0; ;i++) {	
	if (ptr->hasOutput(i)) {
	  ptr->sinkNode->getOutput(i,ptr->internal_pc);
	}
	else {
	  //no more outputs
	  break;
	}
      }

      ptr->internal_pc++;
    
    }
    catch (GenericCastException *e) {
      //We had a problem casting, our inputs are invalid?
      e->print();
      throw new NodeException (ptr,string("Error in ThreadedIterator::getOutput workloop"), __FILE__,__LINE__);
    }      
    catch (BaseException *e) {
      //Something weird happened
      e->print();
      throw new NodeException (ptr,string("Error in ThreadedIterator::getOutput workloop"), __FILE__,__LINE__);
    }


    ptr->iterator_unlock();

    time_t end = time(NULL);

    //cout<<"got time : "<<end - begin <<endl;

    //period in ms
    int period = 1000 / ptr->rate_per_second;
    
    if (end - begin < period) {
      //SLEEP the amount of time needed
      //to be verified...
      //usleep ((period - (end - begin)) * 1000); 
    }
    
    usleep((int)(1.0 /(float)ptr->rate_per_second * 1000000.0));

  }
  
  cerr<<"Exiting ThreadedIterator loop"<<endl;
  
  return NULL;
}

}//namespace FD

#endif
