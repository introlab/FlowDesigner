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
#include "operators.h"

class Div;

DECLARE_NODE(Div)
/*Node
 *
 * @name Div
 * @category Math
 * @description Divides a numerator by a denominator
 *
 * @input_name NUM
 * @input_description The numerator
 *
 * @input_name DEN
 * @input_description The denominator
 *
 * @output_name OUTPUT
 * @output_description The result of the division
 *
END*/


class Div : public BufferedNode {
   
   int input1ID;
   int input2ID;
   int outputID;

public:
   Div(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      input1ID = addInput("NUM");
      input2ID = addInput("DEN");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(input1ID, count);
      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }

      ObjectRef input2Value = getInput(input2ID, count);
      if (input2Value->status != Object::valid)
      {
	 out[count] = input2Value;
	 return;
      }

      out[count] = inputValue / input2Value;
   }

};