// Copyright (C) 2002 Jean-Marc Valin

#include "BufferedNode.h"
#include "CompositeType.h"

class GetComposite;

DECLARE_NODE(GetComposite)
/*Node
 *
 * @name GetComposite
 * @category General
 * @description Creates a composite object
 *
 * @input_name INPUT
 * @input_type CompositeType
 * @input_description Composite object
 *
END*/

class GetComposite : public BufferedNode {
   
   int inputID;
   //vector<string> outputs;

public:
   GetComposite(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
   }

   int translateOutput (string outputName)
   {
      for (unsigned int i=0; i< outputNames.size(); i++) 
      {
         if (outputNames[i] == outputName) {
            return i;
         }
      }  
      return addOutput(outputName);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef in = getInput(inputID, count);
      CompositeType &comp = object_cast<CompositeType> (in);
      
      for (unsigned int i=0; i< outputNames.size(); i++)
      {
         (*outputs[i].buffer)[count] = comp.get(outputNames[i]);
      }
   }

   NO_ORDER_NODE_SPEEDUP(GetComposite)
      
};
