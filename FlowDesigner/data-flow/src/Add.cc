// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "operators.h"

using namespace std;
using namespace FD;

class Add;

DECLARE_NODE(Add)
/*Node
 *
 * @name Add
 * @category Operator
 * @description Adds two input values and returns the result
 *
 * @input_name INPUT1
 * @input_description First value
 *
 * @input_name INPUT2
 * @input_description Second value
 *
 * @output_name OUTPUT
 * @output_description Result of the addition
 *
END*/


class Add : public BufferedNode {

   int m_input1ID;
   int m_outputID;

public:
   Add(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      m_input1ID = addInput("INPUT1");
      m_outputID = addOutput("OUTPUT");
   }

   virtual int translateInput (string inputName)
   {
      for (unsigned int i=0; i< inputs.size(); i++)
      {
         if (inputs[i].name == inputName)
         {
            return i;
         }
      }
      return addInput(inputName);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef ReturnValue = getInput(m_input1ID, count);

      for (int j = 1; j < inputs.size(); j++)
      {
         ObjectRef inputValue = getInput(j, count);
         ReturnValue = ReturnValue + inputValue;
      }

      out[count] = ReturnValue;
   }

   NO_ORDER_NODE_SPEEDUP(Add)
};
