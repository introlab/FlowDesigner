// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "vec.h"

class Gain;

DECLARE_NODE(Gain)
/*Node
 *
 * @name Gain
 * @category DSP:Base
 * @description Applies a gain to a vector
 *
 * @input_name INPUT
 * @input_type Vector
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector
 * @output_description Output vector (after gain)
 *
 * @parameter_name GAIN
 * @parameter_type float
 * @parameter_description Value of the gain
 *
END*/

#define NO_ORDER_NODE_SPEEDUP(nodeClass)                    \
ObjectRef getOutput(int output_id, int count)               \
{                                                           \
   try {                                                    \
      Buffer &outBuffer = *(outputs[output_id].buffer);     \
      if (!outBuffer.isValid(count))                        \
	 nodeClass::calculate (output_id, count, outBuffer);           \
      return outBuffer.get(count);                          \
   } catch (BaseException *e)                               \
   {throw e->add(new NodeException (this, "Exception caught in BufferedNode::getOutput", __FILE__, __LINE__));}}


#define NO_ORDER_NODE_SPEEDUP2(nodeClass)                   \
ObjectRef getOutput(int output_id, int count)               \
{                                                           \
      Buffer &outBuffer = *(outputs[output_id].buffer);     \
      if (!outBuffer.isValid(count))                        \
	 nodeClass::calculate (output_id, count, outBuffer);\
      return outBuffer.get(count);}

class Gain : public BufferedNode {
   
   int inputID;
   int outputID;
   float gain;

public:
   Gain(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      gain = dereference_cast<float> (parameters.get("GAIN"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;

      vec_mul_scal(gain, &in[0], &output[0], inputLength);
      
   }
NO_ORDER_NODE_SPEEDUP(Gain)
/*ObjectRef getOutput(int output_id, int count)
{
   Buffer &outBuffer = *(outputs[output_id].buffer);
   
   if (count > outBuffer.getCurrentPos() || !outBuffer.isValid(count))
   {
	 Gain::calculate (output_id, count, outBuffer);
   }
   return outBuffer.get(count);
   
}
*/
      
};
