// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include <math.h>

class Dist;

DECLARE_NODE(Dist)
/*Node
 *
 * @name Dist
 * @category DSP:Misc
 * @description Calculates the distance between two vectors
 *
 * @input_name INPUT1
 * @input_type Vector
 * @input_description First input vector
 *
 * @input_name INPUT2
 * @input_type Vector
 * @input_description Second input vector
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Distance between INPUT1 and INPUT2
 *
END*/


class Dist : public BufferedNode {
   
   int input2ID;
   int input1ID;
   int outputID;

   static float ncos_dist(const float *x1, const float *x2, int len)
   {
      double xx=0,xy=0,yy=0;
      double sx=0,sy=0;
      for (int i=0;i<len;i++)
      {
	 sy+=x2[i];
	 sx+=x1[i];
      }
      sx/=len;
      sy/=len;
      for (int i=0;i<len;i++)
      {
	 //cerr << x1[i] << " " << x2[i] << endl;
	 xx+=(x1[i]-sx)*(x1[i]-sx);
	 xy+=(x1[i]-sx)*(x2[i]-sy);
	 yy+=(x2[i]-sy)*(x2[i]-sy);
      }
      return xy/sqrt(xx*yy);
   }

   static float cos_dist(const float *x1, const float *x2, int len)
   {
      double xx=0,xy=0,yy=0;
      double sx=0,sy=0;
      for (int i=0;i<len;i++)
      {
	 sy+=x2[i];
	 sx+=x1[i];
      }
      sx/=len;
      sy/=len;
      for (int i=0;i<len;i++)
      {
	 //cerr << x1[i] << " " << x2[i] << endl;
	 xx+=(x1[i])*(x1[i]);
	 xy+=(x1[i])*(x2[i]);
	 yy+=(x2[i])*(x2[i]);
      }
      return xy/sqrt(xx*yy);
   }

public:
   Dist(string nodeName, ParameterSet params)
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


      const Vector<float> &in1 = object_cast<Vector<float> > (input1Value);
      const Vector<float> &in2 = object_cast<Vector<float> > (input2Value);
      
      if (in1.size() != in2.size())
	 throw new NodeException(this, "Vector size don't match", __FILE__, __LINE__);
      
      float dist = cos_dist(&in1[0], &in2[0], in1.size());
      
      out[count] = ObjectRef (new Float (dist));

   }

};
