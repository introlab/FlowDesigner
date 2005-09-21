/* Copyright (C) 2003 Victor Bao Long Tran(v.tran@usherbrooke.ca)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*/
#ifndef _BINARY_AND_CC_
#define _BINARY_AND_CC_

#include "BinaryAND.h"

using namespace std;

namespace FD {

DECLARE_NODE(BinaryAND);

/*Node
 *
 * @name BinaryAND
 * @category Logic
 * @description none
 *
 * @input_name MASK
 * @input_type int
 * @input_description Mask.
 *
 * @input_name Input1
 * @input_type int
 * @input_description the value to mask
 *
 * @output_name OUTPUT
 * @output_type int
 * @output_description new value
 *
END*/

BinaryAND::BinaryAND(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
{
   //inputs
   m_maskInID = addInput("MASK");
   m_InputID = addInput("Input1");

   //outputs
   m_outputID = addOutput("OUTPUT");

   //parameters
}


BinaryAND::~BinaryAND()
{
}


void BinaryAND::calculate(int output_id, int count, Buffer &out)
{
   int mask = dereference_cast<int>(getInput(m_maskInID,count));
   int input = dereference_cast<int>(getInput(m_InputID,count));

   (*outputs[m_outputID].buffer)[count] = ObjectRef(Int::alloc(input&mask));
}

}//namespace FD
#endif //_BINARY_AND_CC_

