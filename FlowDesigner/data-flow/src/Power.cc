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
#ifndef _POWER_CC_
#define _POWER_CC_

#include "Power.h"

using namespace std;

DECLARE_NODE(Power);

/*Node
 *
 * @name Power
 * @category Operator
 * @description none
 *
 * @input_name BASE
 * @input_type int
 * @input_description base
 *
 * @input_name EXP
 * @input_type int
 * @input_description exponent
 *
 * @output_name OUTPUT
 * @output_type int
 * @output_description base power exponent
 *
END*/

Power::Power(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
{
   //inputs
   m_baseInID = addInput("BASE");
   m_expID = addInput("EXP");

   //outputs
   m_outputID = addOutput("OUTPUT");

   //parameters
}


Power::~Power()
{
}


void Power::calculate(int output_id, int count, Buffer &out)
{
   RCPtr<Float> base = getInput(m_baseInID,count);//will do automatic conversion to float
   RCPtr<Float> exp = getInput(m_expID,count); //will do automatic conversion to float

   (*outputs[m_outputID].buffer)[count] = ObjectRef(Float::alloc(pow(base->val(),exp->val())));
}
#endif //_POWER_CC_

