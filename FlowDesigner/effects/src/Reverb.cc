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
#include "reverb.h"

class Reverb;

DECLARE_NODE(Reverb)
/*Node
 *
 * @name Reverb
 * @category Signal:Base
 * @description Stereo Reverb
 *
 * @input_name LEFT
 * @input_type Vector
 * @input_description Right Input Channel
 *
 * @input_name RIGHT
 * @input_type Vector
 * @input_description Left Input Channel
 *
 * @output_name LEFT
 * @output_type Vector
 * @output_description Right Output Channel
 *
 * @output_name RIGHT
 * @output_type Vector
 * @output_description Left Output Channel
 *
END*/


class Reverb : public BufferedNode {
   
   int rightInID, leftInID;
   int rightOutID, leftOutID;

   revmodel *rev;

public:
   Reverb(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      leftInID = addInput("LEFT");
      rightInID = addInput("RIGHT");
      leftOutID = addOutput("LEFT");
      rightOutID = addOutput("RIGHT");
      inOrder = true;
   }

   ~Reverb()
   {
      delete rev;
   }
   
   void specificInitialize()
   {
      BufferedNode::specificInitialize();
      rev = new revmodel;
   }

   void reset()
   {
      BufferedNode::reset();
      delete rev;
      rev = new revmodel;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef leftValue = getInput(leftInID, count);
      ObjectRef rightValue = getInput(rightInID, count);
      if (leftValue->status != Object::valid || rightValue->status != Object::valid)
      {
	 (*outputs[leftOutID].buffer)[count] = leftValue;
	 (*outputs[rightOutID].buffer)[count] = rightValue;
         return;
      }


      const Vector<float> &rightIn = object_cast<Vector<float> > (rightValue);
      const Vector<float> &leftIn = object_cast<Vector<float> > (leftValue);
      if (leftIn.size() != rightIn.size())
	 throw new NodeException(this, "Two different frame lengths for Reverb", __FILE__, __LINE__);
      int inputLength = rightIn.size();
      
      Vector<float> &leftOut = *Vector<float>::alloc(inputLength);
      Vector<float> &rightOut = *Vector<float>::alloc(inputLength);

      (*outputs[leftOutID].buffer)[count] = &leftOut;
      (*outputs[rightOutID].buffer)[count] = &rightOut;
      
      rev->processreplace(&leftIn[0], &rightIn[0], &leftOut[0], &rightOut[0], inputLength, 1);
      
   }

      
};
