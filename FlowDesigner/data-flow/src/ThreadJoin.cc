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

class ThreadJoin;

DECLARE_NODE(ThreadJoin)
/*Node
 *
 * @name ThreadJoin
 * @category Flow
 * @description Joining thread so there's no race at the input nodes
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

   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req)
   {
      inputs[inputID].node->request(inputs[inputID].outputID,req);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      pthread_mutex_lock(&lock);
      ObjectRef inputValue = getInput(inputID,count);
      pthread_mutex_unlock(&lock);  
      return inputValue;
   }

};
