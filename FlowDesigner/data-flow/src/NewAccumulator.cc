// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <iostream>
#include <sstream>

class NewAccumulator;

DECLARE_NODE(NewAccumulator)
/*Node
 *
 * @name NewAccumulator
 * @category General
 * @description Creates a new Accumulator (Vector<ObjectRef>)
 *
 * @output_name OUTPUT
 * @output_type Vector<ObjectRef>
 * @output_description Empty accumulator
 *
END*/


/** A constant node contains a value that will never changes. */
class NewAccumulator : public Node
{

protected:

   /**The value of the constant*/
   ObjectRef value;

   /**The ID of the 'value' output*/
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   NewAccumulator(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
   {
      outputID = addOutput("OUTPUT");
   }

   void specificInitialize()
   {
      value = new Vector<ObjectRef>;
      Node::specificInitialize();
   }

   void reset()
   {
      value = new Vector<ObjectRef>;
      Node::reset();
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) return value;
      else throw new NodeException (this, "NewAccumulator: Unknown output id", __FILE__, __LINE__);
   }
};
