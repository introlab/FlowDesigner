// Copyright (C) 2002 Jean-Marc Valin

#include "BufferedNode.h"
#include "CompositeType.h"

class MakeComposite;

DECLARE_NODE(MakeComposite)
/*Node
 *
 * @name MakeComposite
 * @category General
 * @description Creates a composite object. A composite object is somewhere like a structure in C++. Indeed, a composite object is a regrouping of inputs like a structure is a regrouping of variables. You can chose the name and the number of inputs that you want your node to containt. To add inputs in the node: double-click on it and click on the tab "Input/Outputs". Give a name to the input and press "Add". Repeat as long as you wish. Then, the node will regoup all of these inputs together in only one output (a composite object).
 *
 * @output_name OUTPUT
 * @output_description Create a new composite object. 
 *
END*/

class MakeComposite : public BufferedNode {
   
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
