// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "vmethod.h"

class VMethod;

DECLARE_NODE(VMethod)
/*Node
 *
 * @name VMethod
 * @category General
 * @description Applies a certain method on an object
 *
 * @input_name INPUT
 * @input_type any
 * @input_description Object (this)
 *
 * @output_name OUTPUT
 * @output_type any
 * @output_description Return value of the method
 *
 * @parameter_name METHOD
 * @parameter_type string
 * @parameter_description The name of the method to call
 *
END*/


class VMethod : public BufferedNode {
   
   int inputID;
   int outputID;
   string methodName;
   int methID;

public:
   VMethod(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      methodName = object_cast<String> (parameters.get("METHOD"));
      methID = vmethod()->lookup(methodName);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }

      out[count] = vmethod()->call(methID, inputValue);
   }

      
};
