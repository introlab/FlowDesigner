// Copyright (C) 2002 Jean-Marc Valin

#include "BufferedNode.h"
#include "CompositeType.h"

class MakeComposite;

DECLARE_NODE(MakeComposite)
/*Node
 *
 * @name MakeComposite
 * @category General
 * @description Creates a composite object
 *
 * @output_name OUTPUT
 * @output_description New composite object
 *
END*/

class MakeComposite : public BufferedNode {
   
   //vector<int> inputID;
   int outputID;

public:
   MakeComposite(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
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
      CompositeType &output = *new CompositeType;
      out[count] = &output;
      
      for (unsigned int i=0; i< inputs.size(); i++)
      {
         ObjectRef in = getInput(i, count);
         output.addField(inputs[i].name, in);
      }
   }

   NO_ORDER_NODE_SPEEDUP(MakeComposite)
      
};
