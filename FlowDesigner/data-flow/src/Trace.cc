// Copyright (C) 2001 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "Node.h"

class Trace;

DECLARE_NODE(Trace)
/*Node
 *
 * @name Trace
 * @category Probe
 * @description Pass Through, tracing initialization, requests, inputs and exceptions
 *
 * @input_name INPUT
 * @input_description The input
 *
 * @output_name OUTPUT
 * @output_description The output = The input
 *
 * @parameter_name TAG
 * @parameter_type string
 * @parameter_description Tag to put on the lines
 *
END*/


class Trace : public Node {
protected:
   int inputID;
   int outputID;
   String tag;

public:
   Trace(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      try {
	 tag = object_cast<String> (parameters.get("TAG"));
	 cerr << tag << ": constructor" << endl;
         inputID = addInput("INPUT");
	 outputID=addOutput("OUTPUT");
      } catch (BaseException *e)
      {
         //e->print();
         throw e->add(new NodeException (NULL, "Exception caught in Trace constructor", __FILE__, __LINE__));
      }
      
   }

   /**Standard request-passing method between nodes during initialization*/
   virtual void request(int outputID, const ParameterSet &req)
   {
      cerr << tag << ": request" << endl;
      req.print(cerr);
      inputs[inputID].node->request(inputs[inputID].outputID,req);
   }

   ObjectRef getOutput(int output_id, int count)
   {
      cerr << tag << ": getting input " << count << endl;
      try {
	 ObjectRef inputValue = getInput(inputID,count);
	 cerr << tag << ": input = " << *inputValue << endl;
	 return inputValue;
      } catch (BaseException *e) 
      {
	 cerr << tag << ": exception caught" << endl;
	 e->print(cerr);
	 throw;
      } catch  (...)
      {
	 cerr << tag << ": unknown exception" << endl;
	 throw;
      }
   }

};
