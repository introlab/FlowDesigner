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
 * @description Initialized the neural network weights to fit the input/output set
 *
 * @input_name TRAIN_IN
 * @input_description No description available
 *
 * @input_name TRAIN_OUT
 * @input_description No description available
 *
 * @output_name OUTPUT
 * @output_description No description available
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


class NNetInit : public BufferedNode {

protected:
   
   /**The ID of the 'TRAIN_IN' input*/
   int trainInID;

   /**The ID of the 'TRAIN_OUT' input*/
   int trainOutID;

   /**The ID of the 'OUTPUT' output*/
   int outputID;

   Vector<int> topo;
      
   vector<string> functions;
public:
   /**Constructor, takes the name of the node and a set of parameters*/
   NNetInit(string nodeName, ParameterSet params)
      : BufferedNode(nodeName, params)
   {
      outputID = addOutput("OUTPUT");
      trainInID = addInput("TRAIN_IN");
      trainOutID = addInput("TRAIN_OUT");

      //String topoStr = object_cast<String> (parameters.get("TOPO"));
      //String funcStr = object_cast<String> (parameters.get("FUNCTIONS"));
      
      istringstream str_vector(object_cast <String> (parameters.get("TOPO")));
      str_vector >> topo;

      istringstream str_func(object_cast <String> (parameters.get("FUNCTIONS")));
      str_func >> functions;

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
      
      /*Vector<int> topo(4);
      topo[0] = 16;
      topo[1] = 12;
      topo[2] = 12;
      topo[3] = 8;
      vector<string> functions(3);
      functions[0] = "tansig";
      functions[1] = "tansig";
      functions[2] = "lin";*/
      //cerr << topo << endl;
      //cerr << functions << endl;
      FFNet *net = new FFNet(topo, functions, tin, tout);

      out[count] = ObjectRef(net);
   }
      
protected:

   NNetInit() {throw new GeneralException("NNetInit copy constructor should not be called",__FILE__,__LINE__);}

};
