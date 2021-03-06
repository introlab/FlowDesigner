// Copyright (C) 1999 Jean-Marc Valin


#include "BufferedNode.h"
#include "ObjectRef.h"
#include "NNetSet.h"
#include <sstream>
#include "ObjectParser.h"
#include "Vector.h"

using namespace std;

namespace FD {

class NNetSetInit;

DECLARE_NODE(NNetSetInit)
/*Node
 *
 * @name NNetSetInit
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
 * @output_name OUTPUT
 * @output_description No description available
 *
 * @parameter_name NB_NETS
 * @parameter_description No description available
 *
 * @parameter_name TOPO
 * @parameter_description No description available
 *
 * @parameter_name FUNCTIONS
 * @parameter_description No description available
 *
 * @parameter_name RAND_SEED
 * @parameter_description No description available
 *
END*/


class NNetSetInit : public BufferedNode {

protected:
   
   /**The ID of the 'TRAIN_IN' input*/
   int trainInID;

   /**The ID of the 'TRAIN_OUT' input*/
   int trainOutID;

   /**The ID of the 'TRAIN_ID' input*/
   int trainIDID;

   /**The ID of the 'OUTPUT' output*/
   int outputID;

   Vector<int> topo;
      
   Vector<string> functions;

   int nbNets;
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetSetInit(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");
      trainIDID = addInput("TRAIN_ID");

      //String topoStr = object_cast<String> (parameters.get("TOPO"));
      //String funcStr = object_cast<String> (parameters.get("FUNCTIONS"));
      
      istringstream str_vector(object_cast <String> (parameters.get("TOPO")));
      str_vector >> topo;

      istringstream str_func(object_cast <String> (parameters.get("FUNCTIONS")));
      str_func >> functions;

      nbNets = dereference_cast<int> (parameters.get("NB_NETS"));
      //ObjectRef Otopo;
      //istringstream toposs(string(topoStr));
      //toposs >> topo;
      //topo = *Otopo;

      //ostringstream funcss(string(funcStr));
      //funcss >> functions;
      
      if (parameters.exist("RAND_SEED"))
	 srand(dereference_cast<int> (parameters.get("RAND_SEED")));
      
   }
      
   void calculate(int output_id, int count, Buffer &out)
   {
      ObjectRef trainInValue = getInput(trainInID, count);
      ObjectRef trainOutValue = getInput(trainOutID, count);
      ObjectRef trainIDValue = getInput(trainIDID, count);

 

      Vector<ObjectRef>  &inBuff = object_cast<Vector<ObjectRef> > (trainInValue);
      Vector<ObjectRef>  &outBuff = object_cast<Vector<ObjectRef> > (trainOutValue);
      Vector<ObjectRef>  &idBuff = object_cast<Vector<ObjectRef> > (trainIDValue);
      
      //cerr << "inputs converted\n";
      vector <float *> tin(inBuff.size());
      for (size_t i=0;i<inBuff.size();i++)
	 tin[i]=&object_cast <Vector<float> > (inBuff[i])[0];
      
      vector <float *> tout(outBuff.size());
      for (size_t i=0;i<outBuff.size();i++)
	 tout[i]=&object_cast <Vector<float> > (outBuff[i])[0];

      vector <int> id(idBuff.size());
      for (size_t i=0;i<idBuff.size();i++)
	 id[i]=int(floor(object_cast <Vector<float> > (idBuff[i])[0]+.5));
      
      //srand(6827375);
      NNetSet *net = new NNetSet(nbNets, topo, functions, id, tin, tout);

      out[count] = ObjectRef(net);
   }
      
protected:

   NNetSetInit() {throw new GeneralException("NNetSetInit copy constructor should not be called",__FILE__,__LINE__);}

};
}//namespace FD
