// Copyright (C) 1999 Jean-Marc Valin


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "kmeans.h"
#include "Vector.h"
#include "CodebookMap.h"

using namespace std;

class CMTrain;

DECLARE_NODE(CMTrain)
/*Node
 *
 * @name CMTrain
 * @category VQ
 * @require CMap
 * @description Trains a codebook map
 *
 * @input_name TRAIN_IN
 * @input_type Vector<ObjectRef>
 * @input_description Input feature accumulator
 *
 * @input_name TRAIN_OUT
 * @input_type Vector<ObjectRef>
 * @input_description Output feature accumulator
 *
 * @input_name VQ
 * @input_type KMeans
 * @input_description Already trained vector quantizer
 *
 * @output_name OUTPUT
 * @output_type CodebookMap
 * @output_description Resulting codebook map
 *
END*/


class CMTrain : public BufferedNode {

protected:
   
   /**The ID of the 'trainIN' input*/
   int trainInID;

   /**The ID of the 'trainOut' input*/
   int trainOutID;

   /**The ID of the 'output' output*/
   int outputID;

   /**The ID of the 'nnet' input*/
   int netInputID;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   CMTrain(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      netInputID = addInput("VQ");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");      

      
   }
      
   /**Class specific initialization routine.
      Each class will call its subclass initialize() method*/
   virtual void initialize()
   {
      processCount=-1;
      NodeInput trainInInput = inputs[trainInID];
      //cerr << "in name = " << trainInInput.outputID << endl ;
      
      NodeInput trainOutInput = inputs[trainOutID];
      //cerr << "out name = " << trainOutInput.outputID << endl;
      this->Node::initialize();
   }

   /**Class reset routine.
      Each class will call its superclass reset() method*/
   virtual void reset()
   {
      processCount=-1;
      this->Node::reset();
   }

   /**Ask for the node's output which ID (number) is output_id 
      and for the 'count' iteration */
   virtual void calculate(int output_id, int count, Buffer &out)
   {
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

      vector <float *> vout(outBuff.size());
      for (i=0;i<outBuff.size();i++)
         vout[i]=&object_cast <Vector<float> > (outBuff[i])[0];


      //FFNet *net = new FFNet( topo ); 
      RCPtr<VQ> vq = netValue;
	       
      out[count] = ObjectRef(new CodebookMap(vq,in,vout,object_cast <Vector<float> > (outBuff[0]).size()));
   }

};
