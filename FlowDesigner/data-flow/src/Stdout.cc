// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "Node.h"
#include "Stream.h"

class Stdout;

DECLARE_NODE(Stdout)
/*Node
 *
 * @name Stdout
 * @category IO
 * @description Returns the stdout stream (cout)
 *
 * @output_name OUTPUT
 * @output_description Stdout stream
 * @output_type Stream
 *
END*/


class Stdout : public Node
{

protected:

   ObjectRef value;

   int outputID;
public:

   Stdout(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      , value (ObjectRef(new OStream(&cout,false)))
   {
      outputID = addOutput("OUTPUT");
   }

   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) 
	 return value;
      else 
	 throw new NodeException (this, "Stdout: Unknown output id", __FILE__, __LINE__);
   }

};
