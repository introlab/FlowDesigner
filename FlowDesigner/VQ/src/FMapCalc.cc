// Copyright (C) 1999 Jean-Marc Valin

#include <iostream>
#include "BufferedNode.h"
#include "Buffer.h"
#include "Vector.h"
#include "FeatureMap.h"

class FMapCalc;

DECLARE_NODE(FMapCalc)
/*Node
 *
 * @name FMapCalc
 * @category VQ
 * @description No description available
 *
 * @input_name INPUT
 * @input_description No description available
 *
 * @input_name FMAP
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name INPUTLENGTH
 * @parameter_description No description available
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_description No description available
 *
END*/


class FMapCalc : public BufferedNode {
   
   int inputID;
   int netInputID;
   int outputID;

public:
   FMapCalc(string nodeName, ParameterSet params)
   : BufferedNode(nodeName, params)
   {
      inputID = addInput("INPUT");
      netInputID = addInput("FMAP");
      outputID = addOutput("OUTPUT");
   }

   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef inputValue = getInput(inputID, count);
      if (inputValue->status != Object::valid)
      {
         out[count] = inputValue;
         return;
      }

      ObjectRef netValue = getInput(netInputID, count);
      if (netValue->status != Object::valid)
      {
         out[count] = netValue;
         return;
      }

      FeatureMap &fmap = object_cast<FeatureMap> (netValue);

      out[count] = Vector<float>::alloc(fmap.getOutDimension());
      Vector<float> &output = object_cast<Vector<float> > (out[count]);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();
      
      fmap.calc(in.begin(), output.begin());
   }

};
