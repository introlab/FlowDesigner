// Copyright (C) 1999 Jean-Marc Valin


#include "Node.h"
#include "ObjectRef.h"
#include "NNetSet.h"

class NNetSetTrain;

DECLARE_NODE(NNetSetTrain)
/*Node
 *
 * @name NNetSetTrain
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
 * @parameter_name MOMENTUM
 * @parameter_description No description available
 *
 * @parameter_name INCREASE
 * @parameter_description No description available
 *
 * @parameter_name DECREASE
 * @parameter_description No description available
 *
 * @parameter_name ERR_RATIO
 * @parameter_description No description available
 *
 * @parameter_name BATCH_SETS
 * @parameter_description No description available
 *
END*/


class NNetSetTrain : public Node {

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

   /**Reference to the current stream*/
   ObjectRef currentNet;

   int maxEpoch;
      
   double learnRate;

   double momentum;

   double decrease;

   double increase;

   double errRatio;

   int nbSets;

   int processCount;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetSetTrain(string nodeName, ParameterSet params)
      : Node(nodeName, params)
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
      
      if (parameters.exist("MOMENTUM"))
	 momentum = dereference_cast<float> (parameters.get("MOMENTUM"));
      else momentum = .9;
      
      if (parameters.exist("INCREASE"))
	 increase = dereference_cast<float> (parameters.get("INCREASE"));
      else increase = 1.05;

      if (parameters.exist("DECREASE"))
	 decrease = dereference_cast<float> (parameters.get("DECREASE"));
      else decrease = .7;

      if (parameters.exist("ERR_RATIO"))
	 errRatio = dereference_cast<float> (parameters.get("ERR_RATIO"));
      else errRatio = 1.04;

      if (parameters.exist("BATCH_SETS"))
	 nbSets = dereference_cast<int> (parameters.get("BATCH_SETS"));
      else nbSets = 1;

   }
      
   /**Class specific initialization routine.
      Each class will call its subclass specificInitialize() method*/
   virtual void specificInitialize()
   {
      processCount = -1;
      NodeInput trainInInput = inputs[trainInID];
      cerr << "in name = " << trainInInput.outputID << endl ;
      
      NodeInput trainOutInput = inputs[trainOutID];
      cerr << "out name = " << trainOutInput.outputID << endl;
      this->Node::specificInitialize();
   }

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset()
   {
      processCount = -1;
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
	       cerr << "getOutput in NNetSetTrain\n";
	       int i,j;
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
	       for (i=0;i<inBuff.size();i++)
		  in[i]=&object_cast <Vector<float> > (inBuff[i])[0];

	       vector <float *> out(outBuff.size());
	       for (i=0;i<outBuff.size();i++)
		  out[i]=&object_cast <Vector<float> > (outBuff[i])[0];

	       vector <int> id(idBuff.size());
	       for (i=0;i<idBuff.size();i++)
		  id[i]=int(floor(object_cast <Vector<float> > (idBuff[i])[0]+.5));

	       cerr << "vectors filled\n";
	       //FFNet *net = new FFNet( topo ); 
	       NNetSet &net = object_cast<NNetSet> (netValue);
	       cerr << "training...\n";
	       net.train(id, in, out, maxEpoch, learnRate, momentum, increase, decrease, errRatio, nbSets);
	       //net->trainlm(in, out, maxEpoch);

	       currentNet = netValue;
	       //exit(1);
	    }
	    return currentNet;
	 }
	 else 
	    throw new NodeException (this, "NNetSetTrain: Unknown output id", __FILE__, __LINE__);
      }

protected:
   /**Default constructor, should not be used*/
   NNetSetTrain() {throw new GeneralException("NNetSetTrain copy constructor should not be called",__FILE__,__LINE__);}

};
