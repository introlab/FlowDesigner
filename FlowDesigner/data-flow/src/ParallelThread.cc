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
#include <semaphore.h>
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

   //mutex protecting the output buffer
      //pthread_mutex_t bufferLock;

   //Incremented (by getOutput) when a new calculation can be done
   sem_t sendSem;

   //Incremented (by threadLoop) when a new result is available
   sem_t recSem;

   int calcCount;

   //Destroy thread data
   void destroyThread()
   {
      resetState = true;
      sem_post(&sendSem);
      pthread_join (thread, NULL);
      sem_destroy(&sendSem);
      sem_destroy(&recSem);
      resetState = false;
   }

   //Init thread data
   void initThread()
   {
      sem_init(&sendSem, 0, 0);
      sem_init(&recSem, 0, 0);
   }

public:
   ParallelThread(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , resetState(false)
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
	 if (sem_wait(&sendSem))
	    perror("sem_wait(&sendSem)) ");
	 if (resetState)
	    break;
	 calc();
	 if (sem_post(&recSem))
	    perror("sem_post(&recSem) ");	 
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

      try {
	 ObjectRef input2Value = getInput(input2ID, calcCount);
	 (*outputs[output2ID].buffer)[calcCount] = input2Value;
      } catch (BaseException *e)
      {
	 //cerr << "caught\n";
	 (*outputs[output2ID].buffer)[calcCount] = 
	    new ExceptionObject(e->add(new GeneralException ("Exception caught in ParallelThread", __FILE__, __LINE__)));
      } catch (...)
      {
	 (*outputs[output2ID].buffer)[calcCount] = 
	    new ExceptionObject(new GeneralException ("Unknown exception caught in ParallelThread", __FILE__, __LINE__));
      }

      
      //ObjectRef input2Value = getInput(input2ID, calcCount);
      //(*outputs[output2ID].buffer)[calcCount] = input2Value;      
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      calcCount = count;
      if (sem_post(&sendSem))
	 perror("sem_post(&sendSem) ");
      if (sem_wait(&recSem))
	 perror("sem_wait(&recSem) ");
      //cerr << "calculate\n";
      if (typeid(*(*outputs[output2ID].buffer)[calcCount]) == typeid(ExceptionObject))
      {
	 //cerr << "throwing2\n";
	 object_cast<ExceptionObject> ((*outputs[output2ID].buffer)[calcCount]).doThrow();
      }
      if (typeid(*(*outputs[output1ID].buffer)[calcCount]) == typeid(ExceptionObject))
      {
	 //cerr << "throwing1\n";
	 object_cast<ExceptionObject> ((*outputs[output1ID].buffer)[calcCount]).doThrow();
      }
   }

      
};
