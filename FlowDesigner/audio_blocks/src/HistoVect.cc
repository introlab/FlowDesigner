// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"

using namespace std;

class HistoVect;

DECLARE_NODE(HistoVect)
/*Node
 *
 * @name HistoVect
 * @category DSP:Manip
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name LENGTH
 * @parameter_description No description available
 *
END*/


class HistoVect : public BufferedNode {
   
   int inputID;
   int outputID;
   int length;
   
public:
   HistoVect(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      outputID = addOutput("OUTPUT");
      length = dereference_cast<int> (parameters.get("LENGTH"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);

      int &in = dereference_cast<int> (inputValue);

      out[count] = Vector<float>::alloc(length);
      Vector<float> &output = object_cast<Vector<float> > (out[count]);
      
      for (int i=0;i<length;i++)
         output[i]=0;
      output[in]=1;
   }

};
