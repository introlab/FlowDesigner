// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "reverb.h"

class Reverb;

DECLARE_NODE(Reverb)
/*Node
 *
 * @name Reverb
 * @category DSP:Effects
 * @require Reverb
 * @description Stereo reverb effect
 *
 * @input_name LEFT
 * @input_type Vector<float>
 * @input_description Right Input Channel
 *
 * @input_name RIGHT
 * @input_type Vector<float>
 * @input_description Left Input Channel
 *
 * @output_name LEFT
 * @output_type Vector<float>
 * @output_description Right Output Channel
 *
 * @output_name RIGHT
 * @output_type Vector<float>
 * @output_description Left Output Channel
 *
 * @parameter_name ROOMSIZE
 * @parameter_type float
 * @parameter_value .5
 * @parameter_description 0 < Room Size < 1 (default = .5)
 *
 * @parameter_name DAMP
 * @parameter_type float
 * @parameter_value .5
 * @parameter_description 0 < Damp < 1 (default = .5)
 *
 * @parameter_name WET
 * @parameter_type float
 * @parameter_value .33
 * @parameter_description 0 < Wet < 1 (default = 1/3)
 *
 * @parameter_name DRY
 * @parameter_type float
 * @parameter_value 0
 * @parameter_description 0 < Dry < 1 (default = 0)
 *
 * @parameter_name WIDTH
 * @parameter_type float
 * @parameter_value 1
 * @parameter_description 0 < Width < 1 (default = 1)
 *
END*/


class Reverb : public BufferedNode {
   
   int rightInID, leftInID;
   int rightOutID, leftOutID;

   revmodel *rev;

public:
   Reverb(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
      , rev(NULL)
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
   
   void setReverbParams()
   {
      if (parameters.exist("ROOMSIZE"))
      {
	 rev->setroomsize(dereference_cast<float> (parameters.get("ROOMSIZE")));
      }
      if (parameters.exist("DAMP"))
      {
	 rev->setdamp(dereference_cast<float> (parameters.get("DAMP")));
      }
      if (parameters.exist("WET"))
      {
	 rev->setwet(dereference_cast<float> (parameters.get("WET")));
      }
      if (parameters.exist("DRY"))
      {
	 rev->setdry(dereference_cast<float> (parameters.get("DRY")));
      }
      if (parameters.exist("WIDTH"))
      {
	 rev->setwidth(dereference_cast<float> (parameters.get("WIDTH")));
      }
   }

   void specificInitialize()
   {
      BufferedNode::specificInitialize();
      rev = new revmodel;
      setReverbParams();
   }

   void reset()
   {
      BufferedNode::reset();
      delete rev;
      rev = new revmodel;
      setReverbParams();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef leftValue = getInput(leftInID, count);
      ObjectRef rightValue = getInput(rightInID, count);

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
