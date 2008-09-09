// Copyright (C) 1999 Jean-Marc Valin

#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "CodebookMap.h"

using namespace std;

namespace FD {

class CMCalc;

DECLARE_NODE(CMCalc)
/*Node
 *
 * @name CMCalc
 * @category VQ
 * @require CMap
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name CM
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available
 *
END*/


class CMCalc : public BufferedNode {
   
   int inputID;
   int CMinputID;
   int outputID;
   int outputLength;

public:
   CMCalc(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      CMinputID = addInput("CM");
      outputID = addOutput("OUTPUT");
      outputLength = dereference_cast<int> (parameters.get("OUTPUTLENGTH"));
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef CMValue = getInput(CMinputID, count);

      ObjectRef inputValue = getInput(inputID, count);

      const CodebookMap &cmap = object_cast<CodebookMap> (CMValue);
      const Vector<float> &in = object_cast<Vector<float> > (inputValue);

      Vector<float> &output = *Vector<float>::alloc(outputLength);
      out[count] = &output;



      
      //int classID = vq.getClassID(in.begin());
      //const vector<float> &mean = vq[classID];
      const float *netOut = cmap.calcOutput(&in[0]);
      
      for (int i=0;i<outputLength;i++)
         output[i]=netOut[i];
   }

};
}//namespace FD
