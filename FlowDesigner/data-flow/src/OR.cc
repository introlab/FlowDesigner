// Copyright (C) 1999-2001 Dominic Letourneau & Jean-Marc Valin

#include "BufferedNode.h"
#include "Exception.h"

using namespace std;
using namespace FD;

class OR;

DECLARE_NODE(OR)
/*Node
 *
 * @name OR
 * @category Logic
 * @description Logical OR between two inputs
 *
 * @input_name INPUT1
 * @input_type bool
 * @input_description First boolean input
 *
 * @input_name INPUT2
 * @input_type bool
 * @input_description Second boolean input
 *
 * @output_name OUTPUT
 * @output_type bool
 * @output_description Boolean output
 *
 * @parameter_name PULL_ANYWAY
 * @parameter_type bool
 * @parameter_description Pull on INPUT2 even if INPUT1 is true
 * @parameter_value false
 *
END*/

class OR : public BufferedNode
{
   int m_outputID;
   bool m_pullAnyway;
public:


   OR(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      m_outputID = addOutput("OUTPUT");
      if (parameters.exist("PULL_ANYWAY"))
      {
         m_pullAnyway = dereference_cast<bool> (parameters.get("PULL_ANYWAY"));
      }
      else
      {
         m_pullAnyway = false;
      }
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
      bool outValue = false;

      for (int j = 0; j < inputs.size(); j++)
      {
         ObjectRef inputValue = getInput(j, count);
         if (dereference_cast<bool> (inputValue) && !outValue)
         {
            outValue = true;
         }
         if (!m_pullAnyway && outValue)
         {
            break;
         }
      }
      if (outValue)
      {
         out[count] = TrueObject;
      }
      else
      {
         out[count] = FalseObject;
      }
   }
};
