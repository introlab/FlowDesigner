// Copyright (C) 1999 Jean-Marc Valin


#include "Node.h"
#include "ObjectRef.h"
#include "FeatureMap.h"
#include "Vector.h"

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


class FMapTrain : public Node {

protected:
   
   /**The ID of the 'trainIN' input*/
   int trainInID;

   /**The ID of the 'trainOut' input*/
   int trainOutID;

   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'frames' input*/
   int framesInputID;

   /**Reference to the current stream*/
   ObjectRef currentNet;

   int levels;
     
   int processCount;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   FMapTrain(string nodeName, ParameterSet params)
      : Node(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");
      levels = dereference_cast<int> (parameters.get("LEVELS"));
      
      
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

	       vector <float *> out(outBuff.size());
	       for (i=0;i<outBuff.size();i++)
		  out[i]=&object_cast<Vector<float> > (outBuff[i])[0];


	       FeatureMap *fmap = new FeatureMap(object_cast <Vector<float> > (inBuff[0]).size(), object_cast <Vector<float> > (outBuff[0]).size()); 
	       
	       fmap->recursiveSplit(in, out, levels);
	       //net->train(in, out, maxEpoch, learnRate, momentum, increase, decrease, errRatio);
	       //net->trainlm(in, out, maxEpoch);

	       currentNet = ObjectRef(fmap);
	       //exit(1);
	    }
	    return currentNet;
	 }
	 else 
	    throw new NodeException (this, "FMapTrain: Unknown output id", __FILE__, __LINE__);
      }

protected:
   /**Default constructor, should not be used*/
   FMapTrain() {throw new GeneralException("FMapTrain copy constructor should not be called",__FILE__,__LINE__);}

};
