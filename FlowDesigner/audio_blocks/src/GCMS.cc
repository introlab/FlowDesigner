// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Vector.h"

class GCMS;

DECLARE_NODE(GCMS)
/*Node
 *
 * @name GCMS
 * @category DSP:TimeFreq
 * @description Growing-Window Cepstral Mean Subtraction
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input frames
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description CMS output
 *
 * @parameter_name LENGTH
 * @parameter_type int
 * @parameter_description Frame lentgh (features)
 *
END*/

class GCMS : public BufferedNode {
protected:
   /**Length of input frames*/
   int length;

   /** inputID*/
   int inputID;

   /** outputID */
   int outputID;

   /** sum for the running average */
   vector<float> sum;

   /** number of frames accumulated */
   int accumCount;

public:
   GCMS(string nodeName, const ParameterSet &params) 
      : BufferedNode(nodeName, params) 
      , accumCount(0)
   {
      outputID = addOutput ("OUTPUT");
      inputID = addInput("INPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
      sum.resize(length);
      inOrder = true;
   }

   void specificInitialize()
   {
      //cerr << "GCMS initialize...\n";
      BufferedNode::specificInitialize();
      
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
      if (inputValue->status != Object::valid)
      {
	 out[count] = inputValue;
	 return;
      }

      //Vector<float> &cms = object_cast<Vector<float> > (out[count]);
      Vector<float> &output = *Vector<float>::alloc(length);
      out[count] = &output;
      Vector<float> &in = object_cast<Vector<float> > (inputValue);
      accumCount++;
      float inv_accum=1.0/accumCount;
      for (i=0;i<length;i++)
      {
	 sum[i] = (1-inv_accum)*sum[i] + inv_accum*in[i];
	 output[i] = in[i]-sum[i];
           
      }
   }

protected:
   /**Default constructor, should not be used*/
   GCMS() {throw new GeneralException("GCMS copy constructor should not be called",__FILE__,__LINE__);}

};









