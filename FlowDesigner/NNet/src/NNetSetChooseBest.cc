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
#include "NNetSet.h"
#include "Buffer.h"
#include <sstream.h>
#include "ObjectParser.h"
#include "Vector.h"

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

      Buffer &inBuff = object_cast<Buffer> (trainInValue);
      Buffer &outBuff = object_cast<Buffer> (trainOutValue);
      Buffer &idBuff = object_cast<Buffer> (trainIDValue);
      
      //cerr << "inputs converted\n";
      vector <float *> tin(inBuff.getCurrentPos());
      for (i=0;i<inBuff.getCurrentPos();i++)
	 tin[i]=object_cast <Vector<float> > (inBuff[i]).begin();
      
      vector <float *> tout(outBuff.getCurrentPos());
      for (i=0;i<outBuff.getCurrentPos();i++)
	 tout[i]=object_cast <Vector<float> > (outBuff[i]).begin();

      vector <int> id(idBuff.getCurrentPos());
      for (i=0;i<idBuff.getCurrentPos();i++)
	 id[i]=int(floor((object_cast <Vector<float> > (idBuff[i]).begin())[0]+.5));
      
      //srand(6827375);
      cerr << "creating net\n";
      NNetSet *net = new NNetSet(id, tin, tout, &object_cast<NNetSet> (net1Value), &object_cast<NNetSet> (net2Value));

      out[count] = ObjectRef(net);
   }
      
protected:

   NNetSetChooseBest() {throw new GeneralException("NNetSetChooseBest copy constructor should not be called",__FILE__,__LINE__);}

};
