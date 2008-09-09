// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau

#include "BufferedNode.h"
#include "operators.h"

using namespace std;

namespace FD {

class Min;
DECLARE_NODE(Min)
/*Node
 *
 * @name Min
 * @category Operator
 * @description Selects the minimum between two values
 *
 * @input_name INPUT1
 * @input_description The first value
 *
 * @input_name INPUT2
 * @input_description The second value
 *
 * @output_name OUTPUT
 * @output_description The minimum value between INPUT1 and INPUT2
 *
END*/


class Min : public BufferedNode {
protected:
   ///The ID of the 'output' output
   int m_outputID;

   ///The ID of the 'input1' input
   int m_input1ID;

public:
   ///Constructor, takes the name of the node and a set of parameters
   Min(string nodeName, ParameterSet params) 
     : BufferedNode(nodeName, params)
   {
      m_input1ID = addInput ("INPUT1");
      m_outputID = addOutput ("OUTPUT");
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
      ObjectRef ReturnObject = getInput(m_input1ID,count);

      for (size_t j = 1; j < inputs.size(); j++)
      {
         ObjectRef InputValue = getInput(j,count);
         ReturnObject = min(ReturnObject, InputValue);
      }
     out[count] = ReturnObject;
   }

};

}//namespace FD
