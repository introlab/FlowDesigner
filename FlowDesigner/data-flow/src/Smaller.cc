// Copyright (C) 2001 Jean-Marc Valin & Dominic Letourneau
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
#include "operators.h"

class Smaller;
DECLARE_NODE(Smaller)
/*Node
 *
 * @name Smaller
 * @category Logic
 * @description Verifies if INPUT1 is smaller than INPUT2
 *
 * @input_name INPUT1
 * @input_description The first operand
 *
 * @input_name INPUT2
 * @input_description The second operand
 *
 * @output_name OUTPUT
 * @output_description Boolean output
 *
END*/


class Smaller : public BufferedNode {
protected:
   ///The ID of the 'output' output
   int outputID;

   ///The ID of the 'input1' input
   int input1ID;

   ///The ID of the 'input2' input
   int input2ID;


public:
   ///Constructor, takes the name of the node and a set of parameters
   Smaller(string nodeName, ParameterSet params) 
     : BufferedNode(nodeName, params)
   {
      input1ID = addInput ("INPUT1");
      input2ID = addInput ("INPUT2");
      outputID = addOutput ("OUTPUT");


   }
   
  void calculate(int output_id, int count, Buffer &out) {

     ObjectRef InputValue1 = getInput(input1ID,count);
     
     if (InputValue1->status != Object::valid) {
       out[count] = InputValue1;
       return;
     }
     
     ObjectRef InputValue2 = getInput(input2ID,count);

     if (InputValue2->status != Object::valid) {
       out[count] = InputValue2;
       return;
     }

     
     out[count] = InputValue1 < InputValue2;

   }
   
};

