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

#include "BufferedNode.h"
#include "Buffer.h"
#include <pthread.h>
#include "ExceptionObject.h"

class ParallelThread;

DECLARE_NODE(ParallelThread)
/*Node
 *
 * @name ParallelThread
 * @category Flow
 * @description Parallel threading
 *
 * @input_name INPUT1
 * @input_type any
 * @input_description First parallelized input
 *
 * @input_name INPUT2
 * @input_type any
 * @input_description Second parallelized input
 *
 * @output_name OUTPUT1
 * @output_type any
 * @output_description First parallelized output
 *
 * @output_name OUTPUT2
 * @output_type any
 * @output_description Second parallelized output
 *
END*/


class ParallelThread : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int output1ID;
   int output2ID;

   //True when the calculation thread is asked to terminate
   bool resetState;

   //Main calculation thread
   pthread_t thread;

   //Incremented (by getOutput) when a new calculation can be done
   pthread_cond_t sendSem;

   //Incremented (by threadLoop) when a new result is available
   pthread_cond_t recSem;

   pthread_mutex_t lock;
   pthread_mutex_t lock2;

   //Protecting the whole getOutput() method
   pthread_mutex_t bigLock;

   int calcCount;

   //Destroy thread data
   void destroyThread()
   {
      resetState = true;
      pthread_mutex_lock(&lock);
      calcCount = 1;
      pthread_mutex_unlock(&lock);
      pthread_cond_signal(&sendSem);
      pthread_join (thread, NULL);
      pthread_cond_destroy(&sendSem);
      pthread_cond_destroy(&recSem);
      pthread_mutex_destroy(&lock);
      pthread_mutex_destroy(&lock2);
      pthread_mutex_destroy(&bigLock);
      resetState = false;
   }

   //Init thread data
   void initThread()
   {
      pthread_cond_init(&sendSem,NULL);
      pthread_cond_init(&recSem,NULL);
      pthread_mutex_init(&lock, NULL);
      pthread_mutex_init(&lock2, NULL);
      pthread_mutex_init(&bigLock, NULL);
   }

public:
   ParallelThread(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , resetState(false)
      , calcCount(-1)
   {
      input1ID = addInput("INPUT1");
      input2ID = addInput("INPUT2");
      output1ID = addOutput("OUTPUT1");
      output2ID = addOutput("OUTPUT2");
      initThread();
   }
   
   ~ParallelThread()
   {
      destroyThread();
   }

   void specificInitialize()
   {
      BufferedNode::specificInitialize();
      pthread_create(&thread, NULL, runThread, (void *) this);
   }

   static void *runThread(void *node)
   {
      //cerr << "In runThread\n";
      ((ParallelThread*) node)->threadLoop();
   }

   void threadLoop()
   {
      while (1)
      {
	 pthread_mutex_lock(&lock);
	 if (calcCount == -1)
	    pthread_cond_wait(&sendSem, &lock);
	 pthread_mutex_unlock(&lock);

	 if (resetState)
	    break;
	 //cerr << calcCount << endl;
	 calc();
	 pthread_mutex_lock(&lock2);
	 calcCount = -1;
	 pthread_cond_signal(&recSem);
	 pthread_mutex_unlock(&lock2);
      }
   }

   void calc()
   {
      try {
	 ObjectRef input1Value = getInput(input1ID, calcCount);
	 (*outputs[output1ID].buffer)[calcCount] = input1Value;
      } catch (BaseException *e)
      {
	 (*outputs[output1ID].buffer)[calcCount] = 
	    new ExceptionObject(e->add(new GeneralException ("Exception caught in ParallelThread", __FILE__, __LINE__)));
      } catch (...)
      {
	 //cerr << "caught\n";
	 (*outputs[output1ID].buffer)[calcCount] = 
	    new ExceptionObject(new GeneralException ("Unknown exception caught in ParallelThread", __FILE__, __LINE__));
      }
      
   }
      
   void calculate(int output_id, int count, Buffer &out)
   {
      throw NodeException(this, "This should never, ever happen (ParallelThread::calculate called)", __FILE__, __LINE__);
   }

   ObjectRef getOutput(int output_id, int count)      
   {
      pthread_mutex_lock(&bigLock);
      Buffer &out1 = *(outputs[output1ID].buffer);
      Buffer &out2 = *(outputs[output2ID].buffer);
      
      ObjectRef result;

      
      if (output_id == output1ID)
      {
	 if (typeid(*out1[count]) == typeid(ExceptionObject))
	 {
	    object_cast<ExceptionObject> (out1[count]).doThrow();
	 }
	 result = out1[count];
      } else if (output_id == output2ID)
      {
	 if (typeid(*out2[count]) == typeid(ExceptionObject))
	 {
	    object_cast<ExceptionObject> (out2[count]).doThrow();
	 }
	 result = out2[count];
      } else
	 throw NodeException (this, "Wrong output ID", __FILE__, __LINE__);
      
      if (result->status == Object::valid)
      {
	 pthread_mutex_unlock(&bigLock);
	 return result;
      }
      
      pthread_mutex_lock(&lock);
      calcCount = count;
      pthread_cond_signal(&sendSem);	 
      pthread_mutex_unlock(&lock);

      try {
	 ObjectRef input2Value = getInput(input2ID, count);
	 out2[count] = input2Value;
      } catch (BaseException *e)
      {
	 //cerr << "caught\n";
	 out2[count] = 
	    new ExceptionObject(e->add(new GeneralException ("Exception caught in ParallelThread", __FILE__, __LINE__)));
      } catch (...)
      {
	 out2[count] = 
	    new ExceptionObject(new GeneralException ("Unknown exception caught in ParallelThread", __FILE__, __LINE__));
      }

      pthread_mutex_lock(&lock2);
      if (calcCount != -1)
	 pthread_cond_wait(&recSem, &lock2);
      pthread_mutex_unlock(&lock2);
      

      if (output_id == output1ID)
      {
	 if (typeid(*out1[count]) == typeid(ExceptionObject))
	 {
	    object_cast<ExceptionObject> (out1[count]).doThrow();
	 }
	 result = out1[count];
      } else if (output_id == output2ID)
      {
	 if (typeid(*out2[count]) == typeid(ExceptionObject))
	 {
	    object_cast<ExceptionObject> (out2[count]).doThrow();
	 }
	 result = out2[count];
      } else 
	 throw NodeException (this, "Wrong output ID", __FILE__, __LINE__);

      pthread_mutex_unlock(&bigLock);
      return result;
   }

      
};
