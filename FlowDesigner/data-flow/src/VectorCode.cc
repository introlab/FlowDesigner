// Copyright (C) 2001 Jean-Marc Valin

#include "BufferedNode.h"
#include "Vector.h"
#include "compile_object.h"

class VectorCode;

DECLARE_NODE(VectorCode)
/*Node
 *
 * @name VectorCode
 * @category Vector
 * @description Modifies a vector using C++ code
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Output vector (after gain)
 *
 * @parameter_name CODE
 * @parameter_type string
 * @parameter_description C++ code inside function [void func(const float *x, float *y, int length)]
 *
END*/

class VectorCode : public BufferedNode {
   typedef float (*funcptr)(const float *x, float *y, int length);
   int inputID;
   int outputID;
   funcptr func;
   CompileObject comp;
public:
   VectorCode(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      string code = object_cast<String>(parameters.get("CODE"));
      code = "#include<math.h>\nextern \"C\" {void func(float *x, float *y, int length){" + code + "}}";
      comp.setCode(code);
      comp.compile();
      func=(funcptr)comp.getFuncPtr();
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength);
      out[count] = &output;
      
      func(&in[0], &output[0], inputLength);

   }

   NO_ORDER_NODE_SPEEDUP(VectorCode)
      
};
