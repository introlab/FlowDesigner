// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "lsp.h"

class LSP2LPC;

DECLARE_NODE(LSP2LPC)
/*Node
 *
 * @name LSP2LPC
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

class LSP2LPC : public BufferedNode {
   
   int inputID;
   int outputID;

public:
   LSP2LPC(string nodeName, ParameterSet params)
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

      Vector<float> &output = *Vector<float>::alloc(inputLength+1);
      out[count] = &output;

      float x[inputLength];
      float tmp[2*inputLength+10];
      for (int i=0;i<inputLength;i++)
         x[i]=cos(in[i]);
      lsp_to_lpc(x, &output[0], inputLength, tmp);
   }

   NO_ORDER_NODE_SPEEDUP(LSP2LPC)
      
};
