// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Vector.h"

class GCMS2;

DECLARE_NODE(GCMS2)
/*Node
 *
 * @name GCMS2
 * @category DSP:TimeFreq
 * @description Growing-Window Cepstral Mean Subtraction, counting only speech
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input frames
 *
 * @input_name IS_SPEECH
 * @input_type bool
 * @input_description Whether the frame is speech
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description CMS output
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Frame lentgh (features)
 *
END*/

class GCMS2 : public BufferedNode {
protected:
   /**Length of input frames*/
   int length;

   /** inputID*/
   int inputID;

   int speechID;

   /** outputID */
   int outputID;

   /** sum for the running average */
   vector<float> sum;

   /** number of frames accumulated */
   int accumCount;

public:
   GCMS2(string nodeName, const ParameterSet &params) 
      : BufferedNode(nodeName, params) 
      , accumCount(0)
   {
      outputID = addOutput ("OUTPUT");
      inputID = addInput("INPUT");
      speechID = addInput("IS_SPEECH");
      length = dereference_cast<int> (parameters.get("LENGTH"));
      sum.resize(length);
      inOrder = true;
   }

   void initialize()
   {
      BufferedNode::initialize();
      
      for (int i=0;i<length;i++)
	 sum[i]=0;
      accumCount=0;
   }
   
   void reset()
   {
      BufferedNode::reset();
      
      for (int i=0;i<length;i++)
	 sum[i]=0;
      accumCount=0;
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      int i;
      ObjectRef inputValue = getInput(inputID, count);

      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;
      Vector<float> &in = object_cast<Vector<float> > (inputValue);

      ObjectRef speechValue = getInput(speechID, count);
      if (dereference_cast<bool> (speechValue) || count==0)
      {
	 accumCount++;
	 float inv_accum=1.0/accumCount;
	 for (i=0;i<length;i++)
	    output[i] = in[i]-sum[i];	    
      }

      for (i=0;i<length;i++)
	 output[i] = in[i]-sum[i];
   }

};









