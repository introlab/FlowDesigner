// Copyright (C) 1999 Jean-Marc Valin


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "NNetSet.h"
#include <sstream>
#include "ObjectParser.h"
#include "Vector.h"

using namespace std;

class NNetSetChooseBest;

DECLARE_NODE(NNetSetChooseBest)
/*Node
 *
 * @name NNetSetChooseBest
 * @category NNet
 * @description Initialized the neural network weights to fit the input/output set
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
 * @input_name NET1
 * @input_description No description available
 *
 * @input_name NET2
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
 *
END*/


class NNetSetChooseBest : public BufferedNode {

protected:
   
   /**The ID of the 'TRAIN_IN' input*/
   int trainInID;

   /**The ID of the 'TRAIN_OUT' input*/
   int trainOutID;

   /**The ID of the 'TRAIN_ID' input*/
   int trainIDID;

   /**The ID of the 'OUTPUT' output*/
   int outputID;

   int net1ID;

   int net2ID;

public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetSetChooseBest(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");
      trainIDID = addInput("TRAIN_ID");
      net1ID = addInput("NET1");
      net2ID = addInput("NET2");

   }
      
   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef trainInValue = getInput(trainInID, count);
      ObjectRef trainOutValue = getInput(trainOutID, count);
      ObjectRef trainIDValue = getInput(trainIDID, count);

      ObjectRef net1Value = getInput(net1ID, count);
      ObjectRef net2Value = getInput(net2ID, count);

      int i,j;

      Vector<ObjectRef>  &inBuff = object_cast<Vector<ObjectRef> > (trainInValue);
      Vector<ObjectRef>  &outBuff = object_cast<Vector<ObjectRef> > (trainOutValue);
      Vector<ObjectRef>  &idBuff = object_cast<Vector<ObjectRef> > (trainIDValue);
      
      //cerr << "inputs converted\n";
      vector <float *> tin(inBuff.size());
      for (i=0;i<inBuff.size();i++)
	 tin[i]=&object_cast <Vector<float> > (inBuff[i])[0];
      
      vector <float *> tout(outBuff.size());
      for (i=0;i<outBuff.size();i++)
	 tout[i]=&object_cast <Vector<float> > (outBuff[i])[0];

      vector <int> id(idBuff.size());
      for (i=0;i<idBuff.size();i++)
	 id[i]=int(floor(object_cast <Vector<float> > (idBuff[i])[0]+.5));
      
      //srand(6827375);
      cerr << "creating net\n";
      NNetSet *net = new NNetSet(id, tin, tout, &object_cast<NNetSet> (net1Value), &object_cast<NNetSet> (net2Value));

      out[count] = ObjectRef(net);
   }
      
protected:

   NNetSetChooseBest() {throw new GeneralException("NNetSetChooseBest copy constructor should not be called",__FILE__,__LINE__);}

};
