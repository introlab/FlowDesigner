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

class StrCat;

DECLARE_NODE(StrCat)
/*Node
 *
 * @name StrCat
 * @category General
 * @description Concatenates two strings together
 *
 * @input_name INPUT1
 * @input_type String
 * @input_description First input string
 *
 * @input_name INPUT2
 * @input_type String
 * @input_description Second input string
 *
 * @output_name OUTPUT
 * @output_type String
 * @output_description Concatenated strings
 *
END*/


class StrCat : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   StrCat(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("INPUT1");
      input2ID = addInput("INPUT2");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef input1Value = getInput(input1ID, count);
      ObjectRef input2Value = getInput(input2ID, count);

      if (input1Value->status != Object::valid)
      {
	 out[count] = input1Value;
         return;
      }
      if (input2Value->status != Object::valid)
      {
	 out[count] = input2Value;
         return;
      }

      const String &in1 = object_cast<String> (input1Value);
      const String &in2 = object_cast<String> (input2Value);

      out[count] = new String(in1+in2);
      
   }

};
