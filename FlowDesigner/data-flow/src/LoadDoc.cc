// Copyright (C) 2001 Jean-Marc Valin

#include "BufferedNode.h"
#include "UIDocument.h"

class LoadDoc;

DECLARE_NODE(LoadDoc)
/*Node
 *
 * @name LoadDoc
 * @require UIClasses
 * @category DSP:Base
 * @description Loads an Overflow XML document
 *
 * @input_name INPUT
 * @input_type string
 * @input_description Document name
 *
 * @output_name OUTPUT
 * @output_type UIDocument
 * @output_description loaded document
 *
END*/

class LoadDoc : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   LoadDoc(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      String &filename = object_cast<String > (inputValue);
      UIDocument *doc = new UIDocument(filename);
      doc->load();
      out[count] = ObjectRef(doc);
   }

   NO_ORDER_NODE_SPEEDUP(LoadDoc)
      
};
