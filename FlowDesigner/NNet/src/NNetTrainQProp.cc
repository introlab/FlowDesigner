// Copyright (C) 1999 Jean-Marc Valin


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "FFNet.h"
#include "TrainingAlgo.h"

using namespace std;
using namespace FD;

class NNetTrainQProp;

DECLARE_NODE(NNetTrainQProp)
/*Node
 *
 * @name NNetTrainQProp
 * @category NNet
 * @require FFNetTrain
 * @description Neural network (MLP) training unsing the Quickprop algorithm
 *
 * @input_name TRAIN_IN
 * @input_type Vector<ObjectRef>
 * @input_description Input data accumulator
 *
 * @input_name TRAIN_OUT
 * @input_type Vector<ObjectRef>
 * @input_description Output data accumulator
 *
 * @input_name NNET
 * @input_type FFNet
 * @input_description Neural network that will be trained
 *
 * @output_name OUTPUT
 * @output_type FFNet
 * @output_description Trained network
 *
 * @parameter_name MAX_EPOCH
 * @parameter_type int
 * @parameter_description Number of training epoch (default 2000)
 *
 * @parameter_name LEARN_RATE
 * @parameter_type float
 * @parameter_description Initial learning rate (default 0.000001)
 *
 * @parameter_name INCREASE
 * @parameter_type float
 * @parameter_description Learning rate increment (> 1.0) factor (default 1.04)
 *
 * @parameter_name DECREASE
 * @parameter_type float
 * @parameter_description Learning rate decrement (< 1.0) factor (default 0.6)
 *
END*/


class NNetTrainQProp : public BufferedNode {

protected:
   
   /**The ID of the 'trainIN' input*/
   int trainInID;

   /**The ID of the 'trainOut' input*/
   int trainOutID;

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
   NNetTrainQProp(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      netInputID = addInput("NNET");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");
      
      if (parameters.exist("MAX_EPOCH"))
	 maxEpoch = dereference_cast<int> (parameters.get("MAX_EPOCH"));
      else maxEpoch = 2000;
      
      if (parameters.exist("LEARN_RATE"))
	    learnRate = dereference_cast<float> (parameters.get("LEARN_RATE"));
      else learnRate = .000001;
            
      if (parameters.exist("INCREASE"))
	 increase = dereference_cast<float> (parameters.get("INCREASE"));
      else increase = 1.04;

      if (parameters.exist("DECREASE"))
	 decrease = dereference_cast<float> (parameters.get("DECREASE"));
      else decrease = .6;
      
   }
      

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &out)
   {
      cerr << "getOutput in NNetTrainQProp\n";
      int i,j;

      ObjectRef trainInValue = getInput(trainInID, count);
      ObjectRef trainOutValue = getInput(trainOutID, count);
      ObjectRef netValue = getInput(netInputID, count);
      
      //cerr << "inputs calculated\n";
      Vector<ObjectRef>  &inBuff = object_cast<Vector<ObjectRef> > (trainInValue);
      Vector<ObjectRef>  &outBuff = object_cast<Vector<ObjectRef> > (trainOutValue);
      

      //cerr << "inputs converted\n";
      vector <float *> tin(inBuff.size());
      for (i=0;i<inBuff.size();i++)
	 tin[i]=&object_cast <Vector<float> > (inBuff[i])[0];
      
      vector <float *> tout(outBuff.size());
      for (i=0;i<outBuff.size();i++)
	 tout[i]=&object_cast <Vector<float> > (outBuff[i])[0];
      
      
      FFNet &net = object_cast<FFNet> (netValue);
      //net.setDerivOffset(.05);
      TrainingQProp::train(&net, tin, tout, maxEpoch, learnRate);
      
      out[count] = netValue;

   }

protected:
   /**Default constructor, should not be used*/
   NNetTrainQProp() {throw new GeneralException("NNetTrainQProp copy constructor should not be called",__FILE__,__LINE__);}

};
