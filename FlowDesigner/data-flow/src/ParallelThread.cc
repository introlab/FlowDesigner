// Copyright (C) 2001 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include <pthread.h>
#include "ExceptionObject.h"

#include "pseudosem.h"


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

   //True when the calculation thread is started
   bool threadStarted;

   //Main calculation thread
   pthread_t thread;

   //Incremented (by getOutput) when a new calculation can be done
   pseudosem_t sendSem;

   //Incremented (by threadLoop) when a new result is available
   pseudosem_t recSem;

   //Protecting the whole getOutput() method
   //pthread_mutex_t bigLock;

   int calcCount;

   void endThread()
   {
      resetState = true;
      //cerr << "lock\n";
      pseudosem_post(&sendSem);
      //cerr << "done\n";
      pthread_join (thread, NULL);
      resetState = false;      
   }

   //Destroy thread data
   void destroyThread()
   {
      pseudosem_destroy(&sendSem);
      pseudosem_destroy(&recSem);
      //pthread_mutex_destroy(&bigLock);
   }

   //Init thread data
   void initThread()
   {
      pseudosem_init(&sendSem,0,0);
      pseudosem_init(&recSem,0,0);
      //pthread_mutex_init(&bigLock, NULL);
   }

public:
   ParallelThread(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , resetState(false)
      , calcCount(-1)
      , threadStarted(false)
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
      threadStarted=true;
      pthread_create(&thread, NULL, runThread, (void *) this);
   }

   static void *runThread(void *node)
   {
      //cerr << "In runThread\n";
      ((ParallelThread*) node)->threadLoop();
      return NULL;
   }

   void threadLoop()
   {
      while (1)
      {
	 pseudosem_wait(&sendSem);

	 if (resetState)
	    break;
	 //cerr << calcCount << endl;
	 calc();
	 pseudosem_post(&recSem);
      }
   }

   void cleanupNotify()
   {
      if (threadStarted)
	 endThread();
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
      throw new NodeException(this, "This should never, ever happen (ParallelThread::calculate called)", __FILE__, __LINE__);
   }

   ObjectRef getOutput(int output_id, int count)      
   {
      //pthread_mutex_lock(&bigLock);
      Buffer &out1 = *(outputs[output1ID].buffer);
      Buffer &out2 = *(outputs[output2ID].buffer);
      
      ObjectRef result;

      
      if (output_id == output1ID && out1.isValid(count))
      {
	 if (typeid(*out1.get(count)) == typeid(ExceptionObject))
	 {
	    object_cast<ExceptionObject> (out1.get(count)).doThrow();
	 }
	 return out1.get(count);
      } else if (output_id == output2ID && out2.isValid(count))
      {
	 if (typeid(*out2.get(count)) == typeid(ExceptionObject))
	 {
	    object_cast<ExceptionObject> (out2.get(count)).doThrow();
	 }
	 return out2[count];
      } else
	 if (output_id != output1ID && output_id != output2ID)
	    throw new NodeException (this, "Wrong output ID", __FILE__, __LINE__);
            
      calcCount = count;
      pseudosem_post(&sendSem);	 

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

      pseudosem_wait(&recSem);
      

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
	 throw new NodeException (this, "Wrong output ID", __FILE__, __LINE__);

      //pthread_mutex_unlock(&bigLock);
      return result;
   }

      
};
