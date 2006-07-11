// Copyright (C) 2002 Jean-Marc Valin

#include "BufferedNode.h"
#include "CompositeType.h"

using namespace std;

namespace FD {

class GetComposite;

DECLARE_NODE(GetComposite)
/*Node
 *
 * @name GetComposite
 * @category General
 * @description Split up a composite object. This node makes just the opposite of the node "MakeComposite", that is, split up his compressed input (the composite object) into several outputs. However, the outputs must be added manually by by users. To add outputs to the node: double-click on it and click on the tab "Input/Outputs". Give a name to the output and press "Add". Repeat as long as you wish. Therefore, you can regroup inputs with "MakeComposite", send them in one output (the composite object) and get them back with "GetComposite". However, if you want to do so, inputs of "MakeComposite" and outputs of "GetComposite" must have corresponding names. 
 *
 * @input_name INPUT
 * @input_type CompositeType
 * @input_description Composite object
 *
END*/

class GetComposite : public BufferedNode {
   
   int inputID;

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
	try {
	  (*outputs[i].buffer)[count] = comp.get(outputNames[i]);
	}
	catch (BaseException *e) {
	  e->print(cerr);
	  e->add(new GeneralException("Unable to get composite element named " + outputNames[i],__FILE__,__LINE__));
	  throw e;
	}
      }
   }

   NO_ORDER_NODE_SPEEDUP(GetComposite)
      
};

}//namespace FD
