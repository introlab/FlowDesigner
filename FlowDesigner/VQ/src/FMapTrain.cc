// Copyright (C) 1999 Jean-Marc Valin


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "FeatureMap.h"
#include "Vector.h"

using namespace std;

class FMapTrain;

DECLARE_NODE(FMapTrain)
/*Node

 * @name FMapTrain
 * @category VQ
 * @require FeatureMap
 * @description Trains an hetero-associative map based on a decision tree.
 *
 * @input_name TRAIN_IN
 * @input_type Vector<ObjectRef>
 * @input_description An accumulator with input features
 *
 * @input_name TRAIN_OUT
 * @input_type Vector<ObjectRef>
 * @input_description An accumulator with input features
 *
 * @output_name OUTPUT
 * @output_type FeatureMap
 * @output_description The trained 'feature map'
 *
 * @parameter_name LEVELS
 * @parameter_description Number of levels to the decision tree
 *
END*/


class FMapTrain : public BufferedNode {

protected:
   
   /**The ID of the 'trainIN' input*/
   int trainInID;

   /**The ID of the 'trainOut' input*/
   int trainOutID;

   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'frames' input*/
   int framesInputID;

   int levels;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   FMapTrain(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");
      levels = dereference_cast<int> (parameters.get("LEVELS"));
      
      
   }
      
   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &out)
   {
      cerr << "getOutput in FMapTrain\n";
      int i,j;
      NodeInput trainInInput = inputs[trainInID];
      ObjectRef trainInValue = trainInInput.node->getOutput(trainInInput.outputID,count);

      NodeInput trainOutInput = inputs[trainOutID];
      ObjectRef trainOutValue = trainOutInput.node->getOutput(trainOutInput.outputID,count);

      cerr << "inputs calculated\n";
      Vector<ObjectRef> &inBuff = object_cast<Vector<ObjectRef> > (trainInValue);
      Vector<ObjectRef> &outBuff = object_cast<Vector<ObjectRef> > (trainOutValue);

      cerr << "inputs converted\n";
      vector <float *> in(inBuff.size());
      for (i=0;i<inBuff.size();i++)
         in[i]=&object_cast<Vector<float> > (inBuff[i])[0];

      vector <float *> vout(outBuff.size());
      for (i=0;i<outBuff.size();i++)
         vout[i]=&object_cast<Vector<float> > (outBuff[i])[0];


      FeatureMap *fmap = new FeatureMap(object_cast <Vector<float> > (inBuff[0]).size(), object_cast <Vector<float> > (outBuff[0]).size()); 
	       
      fmap->recursiveSplit(in, vout, levels);
      //net->train(in, out, maxEpoch, learnRate, momentum, increase, decrease, errRatio);
      //net->trainlm(in, out, maxEpoch);

      out[count] = ObjectRef(fmap);
   }


};
