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

#include "Sum.h"
#include "net_types.h"
#include "Object.h"

DECLARE_NODE(Sum)
/*Node

 * @name Sum
 * @category Math
 * @description No description available

 * @input_name INPUT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

END*/


Sum::Sum(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   outputID = addOutput("OUTPUT");
   inputID = addInput("INPUT");
}

void Sum::specificInitialize()
{
   this->Node::specificInitialize();
   sum = 0;
}

void Sum::reset()
{
   this->Node::reset();
}

ObjectRef Sum::getOutput(int output_id, int count)
{
   if (output_id==outputID)
   {
      for (int i=processCount+1 ; i<=count ; i++ )
      {
         NodeInput input = inputs[inputID];
         ObjectRef inputResult = input.node->getOutput(input.outputID, i);
         if (!inputResult->status)
         {
            sum += dereference_cast<float> (inputResult);
            //cerr << "sum = " << sum << endl;
         }
         processCount = i;
      }
      return ObjectRef(new Float(sum));
   }
   else 
      throw new NodeException (this, "Sum: Unknown output id", __FILE__, __LINE__);
}
