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

#include <stream.h>
#include "FrameOperation.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

class SmoothAdd;

DECLARE_NODE(SmoothAdd)
/*Node

 * @name SmoothAdd
 * @category Signal:Special
 * @description No description available

 * @input_name LEFT
 * @input_description No description available

 * @input_name CENTER
 * @input_description No description available

 * @input_name RIGHT
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name LENGTH
 * @parameter_description No description available

END*/


class SmoothAdd : public FrameOperation {
   
   int input1ID;
   int input2ID;
   int input3ID;
   int inputLength;
   vector<float> hanning;

public:
   SmoothAdd(string nodeName, ParameterSet params)
   : FrameOperation(nodeName, params)
   {
      input1ID = addInput("LEFT");
      input2ID = addInput("CENTER");
      input3ID = addInput("RIGHT");
      //cerr << "construct\n";

      if (parameters.exist("INPUTLENGTH"))
         inputLength = dereference_cast<int> (parameters.get("INPUTLENGTH"));
      else inputLength = dereference_cast<int> (parameters.get("LENGTH"));

      //cerr << "construct22\n";
   }

   ~SmoothAdd() {}

   virtual void specificInitialize()
   {
      //cerr << "init\n";
      hanning.resize(inputLength);
      for (int i=0;i<inputLength;i++)
	 hanning[i] = .5-.5*cos((2*M_PI*i)/inputLength);
      this->FrameOperation::specificInitialize();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      //cerr << "a\n";
      Vector<float> &output = object_cast<Vector<float> > (out[count]);

      NodeInput input1 = inputs[input1ID];
      ObjectRef input1Value = input1.node->getOutput(input1.outputID, count);

      NodeInput input2 = inputs[input2ID];
      ObjectRef input2Value = input2.node->getOutput(input2.outputID, count);
      if (input2Value->status != Object::valid)
      {
	 output.status = input2Value->status;
	 return;
      }
      //cerr << "b\n";

      NodeInput input3 = inputs[input3ID];
      ObjectRef input3Value = input3.node->getOutput(input3.outputID, count);
      
      const Vector<float> &in2 = object_cast<Vector<float> > (input2Value);
      
      for (int i=0;i<outputLength;i++)
      {
	 output[i]=hanning[i]*in2[i];
      }
      //cerr << "c\n";

      if (input1Value->status == Object::valid)
      {
	 const Vector<float> &in1 = object_cast<Vector<float> > (input1Value);
	 int half = outputLength >> 1;
	 for (int i=0;i<half;i++)
	 {
	    output[i]+=(1.0-hanning[i])*in1[i];
	 }
      }
      //cerr << "d\n";

      if (input3Value->status == Object::valid)
      {
	 const Vector<float> &in3 = object_cast<Vector<float> > (input3Value);
	 int half = outputLength >> 1;
	 for (int i=half;i<outputLength;i++)
	 {
	    output[i]+=(1-hanning[i])*in3[i];
	 }
      }
      //cerr << "e\n";

      output.status = Object::valid;
   }

};
