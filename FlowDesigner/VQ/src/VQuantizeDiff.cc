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
#include "kmeans.h"

class VQuantizeDiff;

DECLARE_NODE(VQuantizeDiff)
/*Node
 *
 * @name VQuantizeDiff
 * @category VQ
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name VQ
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
END*/


class VQuantizeDiff : public BufferedNode {
   
   int inputID;
   int VQinputID;
   int outputID;
   vector<float> previous;
   int length;

public:
   VQuantizeDiff(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inOrder = true;
      inputID = addInput("INPUT");
      VQinputID = addInput("VQ");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
      previous.resize(length,0);
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef VQValue = getInput(VQinputID, count);
      if (VQValue->status != Object::valid)
      {
	 out[count] = VQValue;
         return;
      }

      ObjectRef inputValue = getInput(inputID, count);
      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
         return;
      }

      const KMeans &vq = object_cast<KMeans> (VQValue);
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      float diff[length];
      for (int i=0;i<length;i++)
	 diff[i] = in[i]-previous[i];
      
      
      int classID = vq.getClassID(&diff[0]);
      const vector<float> &mean = vq[classID];

      

      for (int i=0;i<inputLength;i++)
         output[i]=mean[i]+previous[i];

      for (int i=0;i<inputLength;i++)
	 previous[i] = output[i];

      if (0) {
	 static int count=0;
	 static double sse=0;
	 for (int i=0;i<inputLength;i++)
	    sse += (output[i]-in[i]) * (output[i]-in[i]);
	 count++;
	 if (count % 100 == 0)
	    cout << sse/inputLength/count << endl;
      }
   }

};
