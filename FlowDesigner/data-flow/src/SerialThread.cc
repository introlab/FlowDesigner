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

   bool resetState;
   bool threadStarted;
   pthread_t thread;
   pthread_mutex_t bufferLock;
   sem_t sendSem;
   sem_t recSem;

      //int internalCount;
public:
   SerialThread(string nodeName, const ParameterSet &params)
      : Node(nodeName, params)
      , resetState(false)
      , threadStarted(false)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      int lookAhead = dereference_cast<int> (parameters.get("LOOKAHEAD"));
   }
   
   ~SerialThread()
   {
      destroyThread();
   }

   void request(int outputID, const ParameterSet &req)
   {
      if (req.exist("LOOKAHEAD"))
	 reqLookAhead = max(reqLookAhead,dereference_cast<int> (req.get("LOOKAHEAD")));
      if (req.exist("LOOKBACK"))
	 reqLookBack = max(reqLookBack,dereference_cast<int> (req.get("LOOKBACK")));
      Node::request(outputID, req);
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

   void initThread()
   {
      pthread_mutex_init(&bufferLock, NULL);
      sem_init(&sendSem, 0, lookAhead);
      sem_init(&recSem, 0, 0);
      //internalCount = 0;
   }

   void reset()
   {
      destroyThread();
      initThread();
      buff = RCPtr<Buffer>(new Buffer (lookAhead + 1));
      Node::reset();
   }
   
   void threadLoop()
   {
      int threadCount = 0;
      while (1)
      {
	 cerr << "waiting\n";
	 sem_wait(&sendSem);
	 cerr << "receiving\n";
	 if (resetState)
	    break;
	 //cerr << "calling getOutput " << threadCount << endl;
	 ObjectRef inputValue = getInput(inputID, threadCount);
	 pthread_mutex_lock(&bufferLock);
	 (*buff)[threadCount] = inputValue;
	 cerr << "calculated " << threadCount << endl;
	 pthread_mutex_unlock(&bufferLock);

	 sem_post(&recSem);
	 
	 threadCount++;
      }
   }
   
   static void *runThread(void *node)
   {
      ((SerialThread*) node)->threadLoop();
   }

   void startThread()
   {
      cerr << "thread started\n";
      threadStarted=true;
      pthread_create(&thread, NULL, runThread, (void *) this);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      if (!threadStarted)
	 startThread();
      while (processCount < count)
      {
	 //cerr << "posting\n";
	 sem_post(&sendSem);
	 sem_wait(&recSem);
	 
	 processCount++;
      }
      //sem_wait(&recSem);
      pthread_mutex_lock(&bufferLock);
      cerr << "returning " << count << endl;
      ObjectRef returnValue = (*buff)[count];
      pthread_mutex_unlock(&bufferLock);
      return returnValue;
   }      
};
