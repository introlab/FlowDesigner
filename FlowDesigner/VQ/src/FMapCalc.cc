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
 * @require FeatureMap
 * @description Calculates the result of an hetero-associative map (trained by FMapTrain) for an input vector
 *
 * @input_name INPUT
 * @input_type Vector<float>
 * @input_description Input vector
 *
 * @input_name FMAP
 * @input_type FeatureMap
 * @input_description The feature map that will be used
 *
 * @output_name OUTPUT
 * @output_type Vector<float>
 * @output_description Output features
 *
 * @parameter_name INPUTLENGTH
 * @parameter_type int
 * @parameter_description Number of input features
 *
 * @parameter_name OUTPUTLENGTH
 * @parameter_type int
 * @parameter_description Number of output features
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

      ObjectRef netValue = getInput(netInputID, count);

      FeatureMap &fmap = object_cast<FeatureMap> (netValue);

      out[count] = Vector<float>::alloc(fmap.getOutDimension());
      Vector<float> &output = object_cast<Vector<float> > (out[count]);

      const Vector<float> &in = object_cast<Vector<float> > (inputValue);
      int inputLength = in.size();
      
      fmap.calc(&in[0], &output[0]);
   }

};
