// Copyright (C) 1999 Jean-Marc Valin


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "FFNet.h"
#include <sstream>
#include "ObjectParser.h"
#include "Vector.h"

class NNetInit;

DECLARE_NODE(NNetInit)
/*Node
 *
 * @name NNetInit
 * @category NNet
 * @require FFNet
 * @description Initialized the neural network weights to fit the input/output set
 *
 * @input_name TRAIN_IN
 * @input_type Vector<ObjectRef>
 * @input_description Training input data
 *
 * @input_name TRAIN_OUT
 * @input_type Vector<ObjectRef>
 * @input_description Training output data
 *
 * @output_name OUTPUT
 * @output_type FFNet
 * @output_description Initialized feed-forward neural network
 *
 * @parameter_name TOPO
 * @parameter_type string
 * @parameter_value <Vector >
 * @parameter_description Number of units on each layer (including input and output layers)
 *
 * @parameter_name FUNCTIONS
 * @parameter_type string
 * @parameter_value <Vector >
 * @parameter_description Activation functions for each layer (except the input layer)
 *
 * @parameter_name RAND_SEED
 * @parameter_type int
 * @parameter_description Sets to random seed to RAND_SEED before initialization
 *
END*/


class NNetInit : public BufferedNode {

protected:
   
   /**The ID of the 'TRAIN_IN' input*/
   int trainInID;

   /**The ID of the 'TRAIN_OUT' input*/
   int trainOutID;

   /**The ID of the 'OUTPUT' output*/
   int outputID;

   /**Network topology (see TOPO parameter)*/
   Vector<int> topo;
   
   /**Activation functions (see FUNCTIONS parameter)*/
   Vector<string> functions;
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetInit(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");
      
      istringstream str_vector(object_cast <String> (parameters.get("TOPO")));
      str_vector >> topo;

      istringstream str_func(object_cast <String> (parameters.get("FUNCTIONS")));
      str_func >> functions;

      if (parameters.exist("RAND_SEED"))
	 srand(dereference_cast<int> (parameters.get("RAND_SEED")));

   }
      
   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef trainInValue = getInput(trainInID, count);
      ObjectRef trainOutValue = getInput(trainOutID, count);

      int i,j;

      Vector<ObjectRef> &inBuff = object_cast<Vector<ObjectRef> > (trainInValue);
      Vector<ObjectRef> &outBuff = object_cast<Vector<ObjectRef> > (trainOutValue);
      
      //cerr << "inputs converted\n";
      vector <float *> tin(inBuff.size());
      for (i=0;i<inBuff.size();i++)
	 tin[i]=&object_cast <Vector<float> > (inBuff[i])[0];
      
      vector <float *> tout(outBuff.size());
      for (i=0;i<outBuff.size();i++)
	 tout[i]=&object_cast <Vector<float> > (outBuff[i])[0];
      
      FFNet *net = new FFNet(topo, functions, tin, tout);

      out[count] = ObjectRef(net);
   }
      
protected:

   NNetInit() {throw new GeneralException("NNetInit copy constructor should not be called",__FILE__,__LINE__);}

};
