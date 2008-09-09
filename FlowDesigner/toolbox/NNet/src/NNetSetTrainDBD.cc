// Copyright (C) 1999 Jean-Marc Valin


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "NNetSet.h"

using namespace std;

namespace FD {

class NNetSetTrainDBD;

DECLARE_NODE(NNetSetTrainDBD)
/*Node
 *
 * @name NNetSetTrainDBD
 * @category NNet
 * @description No description available
 *
 * @input_name TRAIN_IN
 * @input_description No description available
 *
 * @input_name TRAIN_OUT
 * @input_description No description available
 *
 * @input_name TRAIN_ID
 * @input_description No description available
 *
 * @input_name NNET
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name MAX_EPOCH
 * @parameter_description No description available
 *
 * @parameter_name LEARN_RATE
 * @parameter_description No description available
 *
 * @parameter_name INCREASE
 * @parameter_description No description available
 *
 * @parameter_name DECREASE
 * @parameter_description No description available
 *
END*/


class NNetSetTrainDBD : public BufferedNode {

protected:
   
   /**The ID of the 'trainIN' input*/
   int trainInID;

   /**The ID of the 'trainOut' input*/
   int trainOutID;

   /**The ID of the 'trainID' input*/
   int trainIDID;

   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'nnet' input*/
   int netInputID;

   int maxEpoch;
      
   float learnRate;

   float decrease;

   float increase;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetSetTrainDBD(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      netInputID = addInput("NNET");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");
      trainIDID = addInput("TRAIN_ID");
      
      if (parameters.exist("MAX_EPOCH"))
	 maxEpoch = dereference_cast<int> (parameters.get("MAX_EPOCH"));
      else maxEpoch = 200;
      
      if (parameters.exist("LEARN_RATE"))
	    learnRate = dereference_cast<float> (parameters.get("LEARN_RATE"));
      else learnRate = .00001;
      
      if (parameters.exist("INCREASE"))
	 increase = dereference_cast<float> (parameters.get("INCREASE"));
      else increase = 1.05;

      if (parameters.exist("DECREASE"))
	 decrease = dereference_cast<float> (parameters.get("DECREASE"));
      else decrease = .7;

   }

   virtual void calculate(int output_id, int count, Buffer &out)
   {
      cerr << "getOutput in NNetSetTrainDBD\n";
   
      NodeInput trainInInput = inputs[trainInID];
      ObjectRef trainInValue = trainInInput.node->getOutput(trainInInput.outputID,count);

      NodeInput trainOutInput = inputs[trainOutID];
      ObjectRef trainOutValue = trainOutInput.node->getOutput(trainOutInput.outputID,count);

      NodeInput trainIDInput = inputs[trainIDID];
      ObjectRef trainIDValue = trainIDInput.node->getOutput(trainIDInput.outputID,count);

      NodeInput netInput = inputs[netInputID];
      ObjectRef netValue = netInput.node->getOutput(netInput.outputID,count);

      cerr << "inputs calculated\n";
      Vector<ObjectRef>  &inBuff = object_cast<Vector<ObjectRef> > (trainInValue);
      Vector<ObjectRef>  &outBuff = object_cast<Vector<ObjectRef> > (trainOutValue);
      Vector<ObjectRef>  &idBuff = object_cast<Vector<ObjectRef> > (trainIDValue);

      cerr << "inputs casted\n";
      vector <float *> in(inBuff.size());
      for (size_t i=0;i<inBuff.size();i++)
         in[i]=&object_cast <Vector<float> > (inBuff[i])[0];

      vector <float *> vout(outBuff.size());
      for (size_t i=0;i<outBuff.size();i++)
         vout[i]=&object_cast <Vector<float> > (outBuff[i])[0];

      vector <int> id(idBuff.size());
      for (size_t i=0;i<idBuff.size();i++)
         id[i]=int(floor(object_cast <Vector<float> > (idBuff[i])[0]+.5));

      cerr << "vectors filled\n";
      //FFNet *net = new FFNet( topo ); 
      NNetSet &net = object_cast<NNetSet> (netValue);
      cerr << "training...\n";
      net.trainDeltaBar(id, in, vout, maxEpoch, learnRate, increase, decrease);
      //net->trainlm(in, out, maxEpoch);

      out[count] = netValue;
      //exit(1);
   }


};
}//namespace FD
