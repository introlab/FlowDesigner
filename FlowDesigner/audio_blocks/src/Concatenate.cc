// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;
using namespace FD;

class Concatenate;

DECLARE_NODE(Concatenate)
/*Node
 *
 * @name Concatenate
 * @category Vector
 * @description Concatenates two vectors together (deprecated, use Concat instead)
 *
 * @input_name INPUT1
 * @input_type Vector<float>
 * @input_description First input vector
 *
 * @input_name INPUT2
 * @input_type Vector<float>
 * @input_description Second input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Concatenated vector
 *
END*/


class Concatenate : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Concatenate(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("INPUT1");
      input2ID = addInput("INPUT2");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef input1Value = getInput(input1ID, count);
      ObjectRef input2Value = getInput(input2ID, count);


      const Vector<float> &in1 = object_cast<Vector<float> > (input1Value);
      const Vector<float> &in2 = object_cast<Vector<float> > (input2Value);
      int input1Length = in1.size();
      int input2Length = in2.size();
      int outputLength = input1Length + input2Length;

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;
      
      for (int i=0;i<input1Length;i++)
      {
         output[i]=in1[i];
      }
      for (int i=0;i<input2Length;i++)
      {
         output[i+input1Length]=in2[i];
      }
   }

NO_ORDER_NODE_SPEEDUP(Concatenate);
};
