//Copyright (C) 2003 Laborius
//Author: Victor Bao Long Tran


#include "Node.h"
#include "Vector.h"
#include "ObjectParser.h"
#include <iostream>
#include <sstream>

using namespace std;
using namespace FD;

class PushBack;

DECLARE_NODE(PushBack)
/*Node
 *
 * @name PushBack
 * @category Vector
 * @description Add value in the Vector
 *
 * @input_name INPUT
 * @input_description Input object
 * @input_type float
 *
 * @input_name VECTOR
 * @input_description Accumulator where to put the input
 * @input_type Vector<float>
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description The vector
 *
END*/


/** A constant node contains a value that will never changes. */
class PushBack : public Node
{

protected:
   /**The ID of the 'value' output*/
   int inputID;
   int vectorID;
   int outputID;
public:

   /**Constructor, takes the name of the node and a set of parameters*/
   PushBack(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      //, value (parameters.get("VALUE"))
   {
      inputID = addInput("INPUT");
      vectorID = addInput("VECTOR");
      outputID = addOutput("OUTPUT");
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
   {
      ObjectRef inputValue = getInput(inputID,count);
      ObjectRef vectorValue = getInput(vectorID,count);
      Vector<float> &vector = object_cast<Vector<float> > (vectorValue);
      float &value = dereference_cast<float>(inputValue);

      vector.push_back(value);
      return vectorValue;
   }
};
