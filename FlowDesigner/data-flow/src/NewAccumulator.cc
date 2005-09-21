// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau


#include "BufferedNode.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <iostream>
#include <sstream>

using namespace std;

namespace FD {

class NewAccumulator;

DECLARE_NODE(NewAccumulator)
/*Node
 *
 * @name NewAccumulator
 * @category General
 * @description Creates a new Accumulator, that is a vector of Objects References. Accumulators are often used as the input "ACCUM" of the node "Accumulate".
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

}//namespace FD
