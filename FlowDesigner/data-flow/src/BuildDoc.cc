// Copyright (C) 2001 Jean-Marc Valin

#include "BufferedNode.h"
#include "UIDocument.h"
#include "Network.h"

class BuildDoc;

DECLARE_NODE(BuildDoc)
/*Node
 *
 * @name BuildDoc
 * @require UIClasses
 * @category General
 * @description Builds a network from a document
 *
 * @input_name INPUT
 * @input_type UIDocument
 * @input_description Loaded document
 *
 * @output_name OUTPUT
 * @output_type Network
 * @output_description built network
 *
END*/

class BuildDoc : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   BuildDoc(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      UIDocument &doc = object_cast<UIDocument > (inputValue);
      ParameterSet params;
      Network *net = doc.build("MAIN", params);

      out[count] = ObjectRef(net);
   }

   NO_ORDER_NODE_SPEEDUP(BuildDoc)
      
};
