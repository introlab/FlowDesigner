// Copyright (C) 1999 Jean-Marc Valin


#include "Node.h"
#include "ObjectRef.h"
#include "FFNet.h"

using namespace std;

namespace FD {

class NNetTrainRecurrent;

DECLARE_NODE(NNetTrainRecurrent)
/*Node

 * @name NNetTrainRecurrent
 * @category NNet
 * @require FFNetTrain
 * @description No description available

 * @input_name TRAIN_IN
 * @input_description No description available

 * @input_name TRAIN_OUT
 * @input_description No description available

 * @input_name NNET
 * @input_description No description available

 * @output_name OUTPUT
 * @output_description No description available

 * @parameter_name MAX_EPOCH
 * @parameter_description No description available

 * @parameter_name LEARN_RATE
 * @parameter_description No description available

 * @parameter_name MOMENTUM
 * @parameter_description No description available

 * @parameter_name INCREASE
 * @parameter_description No description available

 * @parameter_name DECREASE
 * @parameter_description No description available

END*/


class NNetTrainRecurrent : public Node {

protected:
   
   /**The ID of the 'trainIN' input*/
   int trainInID;

   /**The ID of the 'trainOut' input*/
   int trainOutID;

   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'nnet' input*/
   int netInputID;

   /**Reference to the current stream*/
   ObjectRef currentNet;

   int maxEpoch;
      
   double learnRate;

   double momentum;

   double decrease;

   double increase;

   int nbSets;
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetTrainRecurrent(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      netInputID = addInput("NNET");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");
      
      if (parameters.exist("MAX_EPOCH"))
	 maxEpoch = dereference_cast<int> (parameters.get("MAX_EPOCH"));
      else maxEpoch = 200;
      
      if (parameters.exist("LEARN_RATE"))
	    learnRate = dereference_cast<float> (parameters.get("LEARN_RATE"));
      else learnRate = .00001;
      
      if (parameters.exist("MOMENTUM"))
	 momentum = dereference_cast<float> (parameters.get("MOMENTUM"));
      else momentum = .9;
      
      if (parameters.exist("INCREASE"))
	 increase = dereference_cast<float> (parameters.get("INCREASE"));
      else increase = 1.05;

      if (parameters.exist("DECREASE"))
	 decrease = dereference_cast<float> (parameters.get("DECREASE"));
      else decrease = .7;

      if (parameters.exist("BATCH_SETS"))
	 nbSets = dereference_cast<int> (parameters.get("BATCH_SETS"));
      else nbSets = 1;
      
   }
      
   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize()
      {
	 NodeInput trainInInput = inputs[trainInID];
	 cerr << "in name = " << trainInInput.outputID << endl ;
	 
	 NodeInput trainOutInput = inputs[trainOutID];
	 cerr << "out name = " << trainOutInput.outputID << endl;
	 this->Node::initialize();
      }

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset()
      {
	 this->Node::reset();
      }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual ObjectRef getOutput(int output_id, int count)
      {
	 if (output_id==outputID)
	 {
	    if (count != processCount)
	    {
	       cerr << "getOutput in NNetTrainRecurrent\n";
	       int i,j;
	       NodeInput trainInInput = inputs[trainInID];
	       ObjectRef trainInValue = trainInInput.node->getOutput(trainInInput.outputID,count);

	       NodeInput trainOutInput = inputs[trainOutID];
	       ObjectRef trainOutValue = trainOutInput.node->getOutput(trainOutInput.outputID,count);

	       NodeInput netInput = inputs[netInputID];
	       ObjectRef netValue = netInput.node->getOutput(netInput.outputID,count);

	       //cerr << "inputs calculated\n";
	       Vector<ObjectRef>  &inBuff = object_cast<Vector<ObjectRef> > (trainInValue);
	       Vector<ObjectRef>  &outBuff = object_cast<Vector<ObjectRef> > (trainOutValue);

	       //cerr << "inputs converted\n";
	       vector <float *> in(inBuff.size());
	       for (i=0;i<inBuff.size();i++)
		  in[i]=&object_cast <Vector<float> > (inBuff[i])[0];

	       vector <float *> out(outBuff.size());
	       for (i=0;i<outBuff.size();i++)
		  out[i]=&object_cast <Vector<float> > (outBuff[i])[0];


	       //FFNet *net = new FFNet( topo ); 
	       FFNet &net = object_cast<FFNet> (netValue);
	       net.trainRecurrent(in, out, maxEpoch, learnRate, momentum, increase, decrease);
	       //net.trainDeltaBar(in, out, maxEpoch, learnRate, momentum, increase, decrease, nbSets);
	       //net->trainlm(in, out, maxEpoch);

	       currentNet = netValue;
	       //exit(1);
	    }
	    return currentNet;
	 }
	 else 
	    throw new NodeException (this, "NNetTrainRecurrent: Unknown output id", __FILE__, __LINE__);
      }

protected:
   /**Default constructor, should not be used*/
   NNetTrainRecurrent() {throw new GeneralException("NNetTrainRecurrent copy constructor should not be called",__FILE__,__LINE__);}

};
}//namespace FD
