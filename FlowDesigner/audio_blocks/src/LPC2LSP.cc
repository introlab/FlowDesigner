// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "lsp.h"

using namespace std;

class LPC2LSP;

DECLARE_NODE(LPC2LSP)
/*Node
 *
 * @name LPC2LSP
 * @category DSP:Adaptive
 * @description Converts LSP to LPC
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description LSP vector
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Corresponding LPC
 *
END*/

class LPC2LSP : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   LPC2LSP(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();

      Vector<float> &output = *Vector<float>::alloc(inputLength-1);
      out[count] = &output;

      float x[3*inputLength];
      float tmp[2*inputLength+10];
      lpc_to_lsp (const_cast<float*>(&in[0]), inputLength-1, &output[0], 7, .001, tmp);
      for (int i=0;i<inputLength-1;i++)
         output[i]=acos(output[i]);
   }

   NO_ORDER_NODE_SPEEDUP(LPC2LSP)
      
};
