// Copyright (C) 2001 Jean-Marc Valin

#include "Node.h"
#include "Buffer.h"
#include "ExceptionObject.h"
#include <pthread.h>
#include "pseudosem.h"
#include "FlowException.h"

using namespace std;

class SerialThread;

DECLARE_NODE(SerialThread)
/*Node
 *
 * @name SerialThread
 * @category Flow
 * @description Provides a pipeline-type multi-threading. A thread is started and computes inputs before the are needed by the output node.
 *
 * @input_name INPUT
 * @input_type any
 * @input_description Flow input (asynchronous flow)
 *
 * @output_name OUTPUT
 * @output_type any
 * @output_description Output flow (synchronous)
 *
 * @parameter_name LOOKAHEAD
 * @parameter_type int
 * @parameter_description Pipeline look-ahead
 *
END*/


class SerialThread : public Node {

   int inputID;
   int outputID;
      
   int processCount;

   int lookAhead;
   int reqLookAhead;
   int reqLookBack;

   RCPtr<Buffer> buff;

   //True when the calculation thread is asked to terminate
   volatile bool resetState;

   //True when a calculation thread is running
   bool threadStarted;

   //Main calculation thread
   pthread_t thread;

   //mutex protecting the output buffer
   pthread_mutex_t bufferLock;

   //Incremented (by getOutput) when a new calculation can be done
   pseudosem_t sendSem;

   //Incremented (by threadLoop) when a new result is available
   pseudosem_t recSem;

   void endThread()
   {
      resetState=true;
      pseudosem_post(&sendSem);
      //cerr << "Joining " << name << "\n";
      pthread_join (thread, NULL);
      //cerr << "done\n";
      resetState=false;
      threadStarted=false;
   }

   //Destroy thread data
   void destroyThread()
   {
      pthread_mutex_destroy(&bufferLock);
      pseudosem_destroy(&sendSem);
      pseudosem_destroy(&recSem);
   }


   //Init thread data
   void initThread()
   {
      pthread_mutex_init(&bufferLock, NULL);
      pseudosem_init(&sendSem, 0, lookAhead);
      pseudosem_init(&recSem, 0, 0);
   }

public:
   SerialThread(string nodeName, const ParameterSet &params)
      : Node(nodeName, params)
      , resetState(false)
      , threadStarted(false)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
      reqLookAhead=0;
      reqLookBack=0;
   }
   
   ~SerialThread()
   {
      destroyThread();
   }


   void initialize()
   {
      processCount = -1;
      initThread();
      buff = RCPtr<Buffer>(new Buffer (lookAhead + reqLookAhead + reqLookBack + 1));
      Node::initialize();
   }

   void cleanupNotify()
   {
      if (threadStarted)
	 endThread();
   }

   void reset()
   {
      processCount = -1;
      endThread();
      destroyThread();
      initThread();
      buff = RCPtr<Buffer>(new Buffer (lookAhead + 1));
      Node::reset();
   }
   
   void request(int outputID, const ParameterSet &req)
   {
      ParameterSet r;
      
      if (req.exist("LOOKAHEAD"))
	 reqLookAhead = max(reqLookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
      if (req.exist("LOOKBACK"))
	 reqLookBack = max(reqLookBack,dereference_cast<int> (req.get("LOOKBACK")));

      r.add("LOOKAHEAD", ObjectRef(Int::alloc(lookAhead+reqLookAhead)));
      r.add("LOOKBACK", ObjectRef(Int::alloc(reqLookBack)));
      inputs[inputID].node->request(inputs[inputID].outputID, r);

   }

   void threadLoop()
   {
      int threadCount = 0;
      while (1)
      {
	 //Wait for permission to compute
	 //cerr << "waiting\n";
	 pseudosem_wait(&sendSem);
	 //cerr << "posted\n";
	 if (resetState)
	 {
	    //cerr << "break\n";
	    break;
	 }
	 //ObjectRef inputValue;
	 //Compute
	 bool inside_lock=false;
	 try {
	    ObjectRef inputValue = getInput(inputID, threadCount);

	    //Put result in buffer
	    pthread_mutex_lock(&bufferLock);
	    inside_lock=true;
	    (*buff)[threadCount] = inputValue;

	    //cerr << "calculated " << threadCount << endl;
	    pthread_mutex_unlock(&bufferLock);
	    inside_lock=false;

	 } catch (BaseException *e)
	 {
	    if (!inside_lock)
	       pthread_mutex_lock(&bufferLock);
	    (*buff)[threadCount] = new ExceptionObject(e->add(new GeneralException ("Exception caught in SerialThread", __FILE__, __LINE__)));

	    pthread_mutex_unlock(&bufferLock);
	 } catch (RCPtr<FlowException> e)
         {
            if (!inside_lock)
	       pthread_mutex_lock(&bufferLock);
	    (*buff)[threadCount] = e;

	    pthread_mutex_unlock(&bufferLock);
         } 
	 catch (...)
	 {
	    if (!inside_lock)
	       pthread_mutex_lock(&bufferLock);
	    (*buff)[threadCount] = new ExceptionObject(new GeneralException ("Unknown exception caught in SerialThread", __FILE__, __LINE__));

	    pthread_mutex_unlock(&bufferLock);
	 }
	 

	 //Notify that the result is ready
	 //cerr << "pseudosem_post(&recSem);\n";
	 pseudosem_post(&recSem);
	 //cerr << "pseudosem_post done\n";
	 threadCount++;
      }
   }
   
   static void *runThread(void *node)
   {
      ((SerialThread*) node)->threadLoop();
      return NULL;
   }

   void startThread()
   {
      threadStarted=true;
      pthread_create(&thread, NULL, runThread, (void *) this);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      //Start computation thread
      if (!threadStarted)
	 startThread();

      //Make sure that all results up to "count" are computed
      while (processCount < count)
      {
	 pseudosem_post(&sendSem);
	 pseudosem_wait(&recSem);
	 
	 processCount++;
      }

      //Get result in buffer
      pthread_mutex_lock(&bufferLock);
      //cerr << "returning " << count << endl;
      ObjectRef returnValue = buff->get(count);
      pthread_mutex_unlock(&bufferLock);
      if (typeid(*returnValue) == typeid(ExceptionObject))
      {
	 object_cast<ExceptionObject> (returnValue).doThrow();
      } else if (typeid(*returnValue) == typeid(FlowException))
      {
         throw RCPtr<FlowException> (returnValue);
      }
      return returnValue;
   }      
};
