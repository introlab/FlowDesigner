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

#include "Node.h"
#include "pthread.h"
#include "Buffer.h"
#include <semaphore.h>
#include "ExceptionObject.h"



class SerialThread;

DECLARE_NODE(SerialThread)
/*Node
 *
 * @name SerialThread
 * @category Flow
 * @description Pipeline threading
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

   int lookAhead;
   int reqLookAhead;
   int reqLookBack;

   RCPtr<Buffer> buff;

   //True when the calculation thread is asked to terminate
   bool resetState;

   //True when a calculation thread is running
   bool threadStarted;

   //Main calculation thread
   pthread_t thread;

   //mutex protecting the output buffer
   pthread_mutex_t bufferLock;

   //Incremented (by getOutput) when a new calculation can be done
   sem_t sendSem;

   //Incremented (by threadLoop) when a new result is available
   sem_t recSem;

   //Destroy thread data
   void destroyThread()
   {
      resetState=true;
      sem_post(&sendSem);
      pthread_join (thread, NULL);
      pthread_mutex_destroy(&bufferLock);
      sem_destroy(&sendSem);
      sem_destroy(&recSem);
      resetState=false;
   }

   //Init thread data
   void initThread()
   {
      pthread_mutex_init(&bufferLock, NULL);
      sem_init(&sendSem, 0, lookAhead);
      sem_init(&recSem, 0, 0);
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


   void specificInitialize()
   {
      initThread();
      buff = RCPtr<Buffer>(new Buffer (lookAhead + reqLookAhead + reqLookBack + 1));
      ParameterSet req;
      req.add("LOOKAHEAD", ObjectRef(new Int(lookAhead+reqLookAhead)));
      req.add("LOOKBACK", ObjectRef(new Int(reqLookBack)));
      inputs[inputID].node->request(inputs[inputID].outputID, req);
      Node::specificInitialize();
   }


   void reset()
   {
      destroyThread();
      initThread();
      buff = RCPtr<Buffer>(new Buffer (lookAhead + 1));
      Node::reset();
   }
   
   void request(int outputID, const ParameterSet &req)
   {
      if (req.exist("LOOKAHEAD"))
	 reqLookAhead = max(reqLookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
      if (req.exist("LOOKBACK"))
	 reqLookBack = max(reqLookBack,dereference_cast<int> (req.get("LOOKBACK")));
      Node::request(outputID, req);
   }

   void threadLoop()
   {
      int threadCount = 0;
      while (1)
      {
	 //Wait for permission to compute
	 if (sem_wait(&sendSem))
	    perror("sem_wait(&sendSem)) ");
	 if (resetState)
	    break;

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
	 } catch (...)
	 {
	    pthread_mutex_lock(&bufferLock);
	    (*buff)[threadCount] = new ExceptionObject(new GeneralException ("Unknown exception caught in SerialThread", __FILE__, __LINE__));
	    pthread_mutex_unlock(&bufferLock);
	 }
	 

	 //Notify that the result is ready
	 if (sem_post(&recSem))
	    perror("sem_post(&recSem) ");
	 
	 threadCount++;
      }
   }
   
   static void *runThread(void *node)
   {
      ((SerialThread*) node)->threadLoop();
   }

   void startThread()
   {
      threadStarted=true;
      pthread_create(&thread, NULL, runThread, (void *) this);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      //Start computation thread if not started
      if (!threadStarted)
	 startThread();

      //Make sure that all results up to "count" are computed
      while (processCount < count)
      {
	 if (sem_post(&sendSem))
	    perror("sem_post(&sendSem) ");
	 if (sem_wait(&recSem))
	    perror("sem_wait(&recSem) ");
	 
	 processCount++;
      }

      //Get result in buffer
      pthread_mutex_lock(&bufferLock);
      //cerr << "returning " << count << endl;
      ObjectRef returnValue = (*buff)[count];
      pthread_mutex_unlock(&bufferLock);
      if (typeid(*returnValue) == typeid(ExceptionObject))
      {
	 object_cast<ExceptionObject> (returnValue).doThrow();
      }
      return returnValue;
   }      
};
