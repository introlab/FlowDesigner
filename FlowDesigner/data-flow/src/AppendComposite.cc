// Copyright (C) 2002 Jean-Marc Valin

#include "BufferedNode.h"
#include "CompositeType.h"

using namespace std;
using namespace FD;

class AppendComposite;

DECLARE_NODE(AppendComposite)
/*Node
 *
 * @name AppendComposite
 * @category General
 * @description Append a objectRef to an existing Composite object.
 *
 * @input_name COMPOSITE
 * @input_type CompositeType
 * @input_description Composite object to append
 *
 * @output_name OUTPUT
 * @output_description Create a new composite object. 
 *
 * @parameter_name OVERRIDE
 * @parameter_description if true, insertion of new object ref with key existing in given composite will override existing object ref
 * @parameter_type bool
 * @parameter_value true
 *
END*/

class AppendComposite : public BufferedNode {
   
   int outputID;
   int inputID;
   int paramsID;
   
   bool m_override;
   
public:
   AppendComposite(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      inputID = addInput("COMPOSITE");
      if (parameters.exist("OVERRIDE"))
      {
         m_override = dereference_cast<bool>(parameters.get("OVERRIDE"));
      }
      else
      {
         m_override = true;
      }
   }

   int translateInput (string inputName)
   {
      for (unsigned int i=0; i< inputs.size(); i++) {
         if (inputs[i].name == inputName) {
            return i;
         }
      }  
      return addInput(inputName);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      RCPtr<CompositeType> composite = getInput(inputID, count);
      out[count] = composite;
      
      for (unsigned int i=0; i< inputs.size(); i++)
      {
         ObjectRef in = getInput(i, count);
         if (m_override)
         {
            composite->addField(inputs[i].name, in);
         }
         else
         {
            composite->conservativeAddField(inputs[i].name, in);
         }
      }
   }

   NO_ORDER_NODE_SPEEDUP(AppendComposite)
      
};
