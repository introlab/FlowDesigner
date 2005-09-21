// Copyright (C) 2001 Jean-Marc Valin

#include "Node.h"
#include <pthread.h>

using namespace std;

namespace FD {

class ThreadJoin;

DECLARE_NODE(ThreadJoin)
/*Node
 *
 * @name ThreadJoin
 * @category Flow
 * @description Acts like a mutex and prevents two Overflow threads from accessing the same (input) node at the same time.
 *
 * @input_name INPUT
 * @input_description The input
 *
 * @output_name OUTPUT
 * @output_description The output = The input
 *
END*/


class ThreadJoin : public Node {
protected:
   int inputID;
   int outputID;

   pthread_mutex_t lock;

public:
   ThreadJoin(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
         inputID = addInput("INPUT");
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         //e->print();
         throw e->add(new NodeException (NULL, "Exception caught in ThreadJoin constructor", __FILE__, __LINE__));
      }
      pthread_mutex_init(&lock, NULL);

   }

   ~ThreadJoin()
   {
      pthread_mutex_destroy(&lock);
   }

   void reset()
   {

      pthread_mutex_destroy(&lock);
      pthread_mutex_init(&lock, NULL);
      Node::reset();
   }

   ObjectRef getOutput(int output_id, int count)
   {
      ObjectRef inputValue;
      pthread_mutex_lock(&lock);
      try {
	 inputValue = getInput(inputID,count);
      } catch (...)
      {
	 pthread_mutex_unlock(&lock);
	 throw;
      }
      pthread_mutex_unlock(&lock);  
      return inputValue;
   }

};
}//namespace FD
