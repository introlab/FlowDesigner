// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "kmeans.h"

using namespace std;

namespace FD {

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

      ObjectRef inputValue = getInput(inputID, count);

      const KMeans &vq = object_cast<KMeans> (VQValue);
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      DYN_VEC(float, length, diff);
      //float diff[length];
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
}//namespace FD
