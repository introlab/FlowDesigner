// Copyright (C) 1999 Jean-Marc Valin


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "FFNet.h"
#include "TrainingAlgo.h"

class NNetTrainDBD;

DECLARE_NODE(NNetTrainDBD)
/*Node
 *
 * @name NNetTrainDBD
 * @category NNet
 * @description Neural network (MLP) training unsing the Delta-bar-delta algorithm
 * @require FFNetTrain
 *
 * @input_name TRAIN_IN
 * @input_type Vector
 * @input_description Input data accumulator
 *
 * @input_name TRAIN_OUT
 * @input_type Vector
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
 * @parameter_name NB_SETS
 * @parameter_type int
 * @parameter_description Number of batch subsets for accelerated training (default 1)
 *
 * @parameter_name ALLOC_CHUNK
 * @parameter_type bool
 * @parameter_description If true, a big vector is allocated to store all the inputs (default false)
 *
 * @parameter_name RPROP
 * @parameter_type bool
 * @parameter_description If true, use the RProp variant of delta-bar-delta (default false)
 *
END*/


class NNetTrainDBD : public BufferedNode {

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

   int nbSets;

   bool allocChunk;

   bool rprop;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetTrainDBD(string nodeName, ParameterSet params)
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
      
      if (parameters.exist("NB_SETS"))
	 nbSets = dereference_cast<int> (parameters.get("NB_SETS"));
      else nbSets = 1;

      if (parameters.exist("ALLOC_CHUNK"))
	 allocChunk = dereference_cast<bool> (parameters.get("ALLOC_CHUNK"));
      else allocChunk = false;

      if (parameters.exist("RPROP"))
	 rprop = dereference_cast<bool> (parameters.get("RPROP"));
      else rprop = false;
      
   }
      

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &out)
   {
      cerr << "getOutput in NNetTrainDBD\n";
      int i,j;

      ObjectRef trainInValue = getInput(trainInID, count);
      ObjectRef trainOutValue = getInput(trainOutID, count);
      ObjectRef netValue = getInput(netInputID, count);
      
      //cerr << "inputs calculated\n";
      Vector<ObjectRef>  &inBuff = object_cast<Vector<ObjectRef> > (trainInValue);
      Vector<ObjectRef>  &outBuff = object_cast<Vector<ObjectRef> > (trainOutValue);
      

      //cerr << "inputs converted\n";
      
      int nbSamples = inBuff.size();
      if (nbSamples != outBuff.size())
	 throw new NodeException(this, "Input buffer sizes don't fit", __FILE__, __LINE__);
      int inLength = object_cast <Vector<float> > (inBuff[0]).size();
      int outLength = object_cast <Vector<float> > (outBuff[0]).size();
      
      vector <float *> tin(nbSamples);
      vector <float *> tout(nbSamples);
      float *buff;

      if (allocChunk)
      {
	 buff = new float [nbSamples*(inLength+outLength)];
	 for (i=0;i<nbSamples;i++)
	 {
	    tin[i] = buff+i*(inLength+outLength);
	    tout[i] = buff+i*(inLength+outLength)+inLength;
	    Vector<float> &vin = object_cast <Vector<float> > (inBuff[i]);
	    Vector<float> &vout = object_cast <Vector<float> > (outBuff[i]);
	    if (inLength != vin.size() || outLength != vout.size())
	       throw new NodeException(this, "Vectors in buffers have different sizes", __FILE__, __LINE__);
	    for (int j=0;j<inLength;j++)
	       tin[i][j] = vin[j];
	    for (int j=0;j<outLength;j++)
	       tout[i][j] = vout[j];
            //vec_copy(&vin[0], tin[i], inLength);
	    //vec_copy(&vout[0], tout[i], outLength);
	 }
	 
      } else {
	 for (i=0;i<nbSamples;i++)
	 {
	    tin[i]=&object_cast <Vector<float> > (inBuff[i])[0];
	    tout[i]=&object_cast <Vector<float> > (outBuff[i])[0];
	 }
      }
      /*
      vector <float *> tin(inBuff.size());
      for (i=0;i<inBuff.size();i++)
	 tin[i]=&object_cast <Vector<float> > (inBuff[i])[0];
      
      vector <float *> tout(outBuff.size());
      for (i=0;i<outBuff.size();i++)
	 tout[i]=&object_cast <Vector<float> > (outBuff[i])[0];
      */


      FFNet &net = object_cast<FFNet> (netValue);
      //net.setDerivOffset(.05);
      TrainingDeltaBarDelta::train(&net, tin, tout, maxEpoch, learnRate, increase, decrease, nbSets, rprop);
      
      if (allocChunk)
	 delete [] buff;

      out[count] = netValue;

   }

protected:
   /**Default constructor, should not be used*/
   NNetTrainDBD() {throw new GeneralException("NNetTrainDBD copy constructor should not be called",__FILE__,__LINE__);}

};
