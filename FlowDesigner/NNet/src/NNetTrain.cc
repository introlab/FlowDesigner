// Copyright (C) 1999 Jean-Marc Valin
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2, or (at your option)
// any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this file.  If not, write to the Free Software Foundation,
// 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


#include "Node.h"
#include "ObjectRef.h"
#include "FFNet.h"
#include "Buffer.h"

class NNetTrain;

NODE_INFO(NNetTrain,"NNet", "TRAIN_IN:TRAIN_OUT:NNET", "OUTPUT", "MAX_EPOCH:LEARN_RATE:MOMENTUM:INCREASE:DECREASE:ERR_RATIO:BATCH_SETS")

class NNetTrain : public Node {

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

   double errRatio;

   int nbSets;
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetTrain(string nodeName, ParameterSet params)
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
	       cerr << "getOutput in NNetTrain\n";
	       int i,j;
	       NodeInput trainInInput = inputs[trainInID];
	       ObjectRef trainInValue = trainInInput.node->getOutput(trainInInput.outputID,count);

	       NodeInput trainOutInput = inputs[trainOutID];
	       ObjectRef trainOutValue = trainOutInput.node->getOutput(trainOutInput.outputID,count);

	       NodeInput netInput = inputs[netInputID];
	       ObjectRef netValue = netInput.node->getOutput(netInput.outputID,count);

	       //cerr << "inputs calculated\n";
	       Buffer &inBuff = object_cast<Buffer> (trainInValue);
	       Buffer &outBuff = object_cast<Buffer> (trainOutValue);

	       //cerr << "inputs converted\n";
	       vector <float *> in(inBuff.getCurrentPos());
	       for (i=0;i<inBuff.getCurrentPos();i++)
		  in[i]=object_cast <Vector<float> > (inBuff[i]).begin();

	       vector <float *> out(outBuff.getCurrentPos());
	       for (i=0;i<outBuff.getCurrentPos();i++)
		  out[i]=object_cast <Vector<float> > (outBuff[i]).begin();


	       //FFNet *net = new FFNet( topo ); 
	       FFNet &net = object_cast<FFNet> (netValue);
	       net.train(in, out, maxEpoch, learnRate, momentum, increase, decrease, errRatio, nbSets);
	       //net->trainlm(in, out, maxEpoch);

	       currentNet = netValue;
	       //exit(1);
	    }
	    return currentNet;
	 }
	 else 
	    throw new NodeException (this, "NNetTrain: Unknown output id", __FILE__, __LINE__);
      }

protected:
   /**Default constructor, should not be used*/
   NNetTrain() {throw new GeneralException("NNetTrain copy constructor should not be called",__FILE__,__LINE__);}

};
