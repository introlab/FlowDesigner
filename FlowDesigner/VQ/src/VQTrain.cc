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

#include "VQTrain.h"
#include "net_types.h"
#include "Buffer.h"
#include "kmeans.h"
#include "Vector.h"
//#include "multithread.h"

//DECLARE_NODE(VQTrain)
NODE_INFO(VQTrain,"VQ", "FRAMES", "OUTPUT", "MEANS")

VQTrain::VQTrain(string nodeName, ParameterSet params) 
   : Node(nodeName, params)
{
   try {
      //cerr << "VQTrain initialize\n";
      outputID = addOutput("OUTPUT");
      framesInputID = addInput("FRAMES");
      //cerr << "VQTrain initialization done\n";
      nbMeans = dereference_cast<int> (parameters.get("MEANS"));
   } catch (BaseException *e)
   {
      e->print(cerr);
      throw new NodeException(NULL, "Exception caught in VQTrain constructor", __FILE__, __LINE__);
   }
}

void VQTrain::specificInitialize()
{
   this->Node::specificInitialize();
}

void VQTrain::reset()
{
   this->Node::reset();
}

ObjectRef VQTrain::getOutput(int output_id, int count)
{
   //cerr << "Getting output in VQTrain\n";
   if (output_id==outputID)
   {
      if (count != processCount)
      {
         int i;
         NodeInput framesInput = inputs[framesInputID];

         cerr << "getting frames..." << endl;
         ObjectRef matRef = framesInput.node->getOutput(framesInput.outputID,count);
         cerr << "got frames..." << endl;
         Buffer &mat = object_cast<Buffer> (matRef);

         KMeans *vq = new KMeans;

         vector <float *> data(mat.getCurrentPos()+1);
         for (i=0;i<=mat.getCurrentPos();i++)
            data[i]=object_cast <Vector<float> > (mat[i]).begin();
         int length = object_cast <Vector<float> > (mat[0]).size();

         cerr << "training..." << endl;
         vq->train(nbMeans,data,length);
         cerr << "training complete." << endl;

         current = ObjectRef(vq);
      }
      return current;
   }
   else 
      throw new NodeException (this, "VQTrain: Unknown output id", __FILE__, __LINE__);
}
