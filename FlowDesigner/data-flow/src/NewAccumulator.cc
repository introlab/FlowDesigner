// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "BufferedNode.h"
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


class NewAccumulator : public BufferedNode
{

protected:

   /**The ID of the 'OUTPUT' output*/
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   NewAccumulator(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params) 
   {
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      out[count] = new Vector<ObjectRef>;
   }
};
