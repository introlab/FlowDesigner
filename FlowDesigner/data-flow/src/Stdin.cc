// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "Node.h"

class Stdin;

DECLARE_NODE(Stdin)
/*Node
 *
 * @name Stdin
 * @category IO
 * @description Returns the stdin stream (cin)
 *
 * @output_name OUTPUT
 * @output_description Stdin stream
 * @output_type Stream
 *
END*/


class Stdin : public Node
{

protected:

   ObjectRef value;

   int outputID;
public:

   Stdin(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      , value (ObjectRef(new IStream(&cin,false)))
   {
      outputID = addOutput("OUTPUT");
   }

   /**Do nothing for requests since we have no inputs*/
   virtual void request(int outputID, const ParameterSet &req) {}

   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) 
	 return value;
      else 
	 throw new NodeException (this, "Stdin: Unknown output id", __FILE__, __LINE__);
   }

};
