// Copyright (C) 1999 Jean-Marc Valin & Dominic Letourneau

#include "Node.h"
#include "Stream.h"

using namespace std;
using namespace FD;

class Stderr;

DECLARE_NODE(Stderr)
/*Node
 *
 * @name Stderr
 * @category IO
 * @description Returns the stderr stream (cerr)
 *
 * @output_name OUTPUT
 * @output_description Stderr stream
 * @output_type Stream
 *
END*/


class Stderr : public Node
{

protected:

   ObjectRef value;

   int outputID;
public:

   Stderr(string nodeName, ParameterSet params)
      : Node(nodeName, params) 
      , value (ObjectRef(new OStream(&cerr,false)))
   {
      outputID = addOutput("OUTPUT");
   }

   virtual ObjectRef getOutput(int output_id, int count)
   {
      if (output_id==outputID) 
	 return value;
      else 
	 throw new NodeException (this, "Stderr: Unknown output id", __FILE__, __LINE__);
   }

};
